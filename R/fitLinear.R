#' Fit linear regression model
#'
#' @description
#' Fit linear regression model on a continuous endpoint.
#'
#' @param formula an object of class \code{formula}. Must include \code{arm} and
#' endpoint in \code{data}. Covariates can be adjusted.
#' @param placebo Character. String indicating the placebo arm in \code{data$arm}.
#' @param data Data frame. Usually it is a locked data set.
#' @param alternative a character string specifying the alternative hypothesis,
#' must be one of \code{"greater"} or \code{"less"}. No default value.
#' \code{"greater"} means superiority of treatment over placebo is established
#' by a greater mean in treated arm.
#' @param ... Subset conditions compatible with \code{dplyr::filter}.
#' \code{glm} will be fitted on this subset only. This argument can be useful
#' to create a subset of data for analysis when a trial consists of more
#' than two arms. By default, it is not specified,
#' all data will be used to fit the model. More than one condition can be
#' specified in \code{...}, e.g.,
#' \code{fitLinear(cfb ~ arm, 'pbo', data, 'greater', arm \%in\% c('pbo', 'low dose'), cfb > 0.5)},
#' which is equivalent to:
#' \code{fitLinear(cfb ~ arm, 'pbo', data, 'greater', arm \%in\% c('pbo', 'low dose') & cfb > 0.5)}.
#' Note that if more than one treatment arm are present in the data after
#' applying filter in \code{...}, models are fitted for placebo verse
#' each of the treatment arms.
#'
#' @returns a data frame with columns:
#' \describe{
#' \item{\code{arm}}{name of the treatment arm. }
#' \item{\code{placebo}}{name of the placebo arm. }
#' \item{\code{estimate}}{estimate of average treatment effect of \code{arm}. }
#' \item{\code{p}}{one-sided p-value for between-arm difference (treated vs placebo). }
#' \item{\code{info}}{sample size used in model with \code{NA} being removed. }
#' \item{\code{z}}{z statistics of between-arm difference (treated vs placebo). }
#' }
#'
#' @importFrom stats glm pt
#' @importFrom emmeans emmeans contrast
#' @export
#'
fitLinear <- function(formula, placebo, data, alternative, ...) {

  if(!inherits(formula, 'formula')){
    stop('formula must be a formula object with "arm" indicating the column arm in data. ')
  }

  if(!is.character(placebo) || length(placebo) != 1){
    stop("placebo must be a single character string")
  }

  if(!is.data.frame(data)){
    stop("data must be a data frame")
  }

  alternative <- match.arg(alternative, choices = c('greater', 'less'))

  vars_in_formula <- all.vars(formula)
  missing_vars <- setdiff(vars_in_formula, names(data))
  if(length(missing_vars) > 0){
    stop('The following variable(s) used in formula are missing from data: \n',
         paste0(missing_vars, collapse = ', '))
  }

  if(!'arm' %in% vars_in_formula){
    stop('formula must include main effect term for arm. ')
  }

  # Prepare the data based on condition ...
  filtered_data <- if(...length() == 0){
    data
  }else{
    tryCatch({
      data %>% dplyr::filter(...)
    },
    error = function(e){
      stop('Error in filtering data for fitting a linear regression model. ',
           'Please check condition in ..., ',
           'which should be compatible with dplyr::filter. ')
    })
  }

  # Check if any data remains after filtering
  if (nrow(filtered_data) == 0) {
    stop("No data remaining after applying subset condition. ")
  }

  treatment_arms <- setdiff(unique(filtered_data$arm), placebo) %>% sort()

  ret <- NULL

  for(trt_arm in treatment_arms){
    sub_data <- filtered_data %>% dplyr::filter(.data$arm %in% c(placebo, trt_arm))

    # Ensure arm is a factor with placebo and treatment
    sub_data$arm <- factor(sub_data$arm, levels = c(placebo, trt_arm))

    # Fit the linear regression model
    fit <- glm(formula, data = sub_data, family = 'gaussian')

    res <- fit %>%
      emmeans(~ arm) %>%
      contrast(method =
                 list('trt_vs_pbo' = setNames(c(-1, 1), c(placebo, trt_arm)))
               ) %>%
      summary()

    estimate <- res$estimate
    z <- res$t.ratio
    df <- res$df
    p <- ifelse(alternative == 'greater', 1 - pt(z, df = df), pt(z, df = df))

    info <- fit$df.residual + fit$rank

    ret <- rbind(ret, data.frame(arm = trt_arm, placebo = placebo,
                                 estimate = estimate,
                                 p = p, info = info, z = z
                                 )
                 )
  }

  class(ret) <- c('fit_linear', class(ret))
  ret
}

