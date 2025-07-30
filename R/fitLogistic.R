#' Fit logistic regression model
#'
#' @description
#' Fit logistic regression model on an binary endpoint.
#'
#' @param formula An object of class \code{formula}. Must include \code{arm}
#' and endpoint in \code{data}. Covariates can be adjusted.
#' @param placebo Character. String indicating the placebo in \code{data$arm}.
#' @param data Data frame. Usually it is a locked data set.
#' @param alternative a character string specifying the alternative hypothesis,
#' must be one of \code{"greater"} or \code{"less"}. No default value.
#' \code{"greater"} means superiority of treatment over placebo is established
#' by an odds ratio greater than 1.
#' @param scale character. The type of estimate in the output. Must be one
#' of \code{"coefficient"}, \code{"log odds ratio"}, \code{"odds ratio"}, \code{"risk ratio"},
#' or \code{"risk difference"}. No default value.
#' @param ... Subset conditions compatible with \code{dplyr::filter}.
#' \code{glm} will be fitted on this subset only. This argument can be useful
#' to create a subset of data for analysis when a trial consists of more
#' than two arms. By default, it is not specified,
#' all data will be used to fit the model. More than one condition can be
#' specified in \code{...}, e.g.,
#' \code{fitLogistic(remission ~ arm, 'pbo', data, 'greater', 'odds ratio', arm \%in\% c('pbo', 'low dose'), cfb > 0.5)},
#' which is equivalent to:
#' \code{fitLogistic(remission ~ arm, 'pbo', data, 'greater', 'odds ratio', arm \%in\% c('pbo', 'low dose') & cfb > 0.5)}.
#' Note that if more than one treatment arm are present in the data after
#' applying filter in \code{...}, models are fitted for placebo verse
#' each of the treatment arms.
#'
#' @returns a data frame with columns:
#' \describe{
#' \item{\code{arm}}{name of the treatment arm. }
#' \item{\code{placebo}}{name of the placebo arm. }
#' \item{\code{estimate}}{estimate depending on \code{scale}. }
#' \item{\code{p}}{one-sided p-value for log odds ratio (treated vs placebo). }
#' \item{\code{info}}{sample size used in model with \code{NA} being removed. }
#' \item{\code{z}}{z statistics of log odds ratio (treated vs placebo). }
#' }
#'
#' @importFrom emmeans regrid
#' @export
#'
fitLogistic <- function(formula, placebo, data, alternative, scale, ...) {

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

  valid_scales <- c('coefficient', 'log odds ratio', 'odds ratio', 'risk ratio', 'risk difference')
  if(!is.character(scale) || length(scale) != 1 || !(scale %in% valid_scales)){
    stop('scale must be one of ', paste0(valid_scales, collapse = ', '))
  }

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
      stop('Error in filtering data for fitting a logistic regression model. ',
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
    # Fit the Cox model
    fit <- tryCatch({
      glm(formula, data = sub_data, family = 'binomial')
    }, error = function(e){
      stop('logistic regression model fitting failed: ', e$message)
    })

    if(scale == 'coefficient'){

      sfit <- summary(fit)

      ## identify coefficient of treatment arm (main effect)
      coef_row <- grep(paste0('^arm', trt_arm, '$'), rownames(sfit$coef))
      if(length(coef_row) != 1){
        stop('fixLogistic: unable to identify arm main effect coefficient for arm <', trt_arm,
             '>. Please use <arm> in formula explicitly. ')
      }

      estimate <- sfit$coef[coef_row, 'Estimate']
      z <- sfit$coef[coef_row, 'z value']

    }else{

      type1 <- c('log odds ratio' = 'logit',
                 'odds ratio' = 'response',
                 'risk ratio' = 'response',
                 'risk difference' = 'response')[scale]

      type2 <- c('log odds ratio' = 'none',
                 'odds ratio' = 'none',
                 'risk ratio' = 'log',
                 'risk difference' = 'response')[scale]

      estimate_index <- c('log odds ratio' = 'estimate',
                          'odds ratio' = 'odds.ratio',
                          'risk ratio' = 'ratio',
                          'risk difference' = 'estimate')[scale]

      res <- fit %>%
        emmeans(~ arm, type = type1) %>%
        regrid(type2) %>%
        contrast(method = list('trt_vs_pbo' = setNames(c(-1, 1), c(placebo, trt_arm)))) %>%
        summary()

      estimate <- res[[estimate_index]]
      z <- res$z.ratio
    }

    p <- ifelse(alternative == 'greater', 1 - pnorm(z), pnorm(z))

    info <- fit$df.residual + fit$rank

    ret <- rbind(ret, data.frame(arm = trt_arm, placebo = placebo,
                                 estimate = estimate,
                                 p = p, info = info, z = z
                                 )
                 )
  }

  rownames(ret) <- NULL
  class(ret) <- c('fit_logistic', class(ret))
  ret
}

