#' Carry out log rank test
#' @description
#' Compute log rank test statistic on an endpoint.
#'
#' Refer to \href{https://zhangh12.github.io/TrialSimulator/articles/wrappers.html}{this vignette}
#' for more information and examples.
#'
#' @param formula An object of class \code{formula} that can be used with
#' \code{survival::coxph}. Must consist \code{arm} and endpoint in \code{data}.
#' No covariate is allowed. Stratification variables are supported and can be
#' added using \code{strata(...)}.
#' @param placebo character. String of placebo in \code{data$arm}.
#' @param data data frame. Usually it is a locked data.
#' @param alternative a character string specifying the alternative hypothesis,
#' must be one of \code{"greater"} or \code{"less"},
#' i.e., one-sided test is enforced. No default value.
#' \code{"greater"} means superiority of treatment over placebo is established
#' by an hazard ratio greater than 1.
#' @param tidy logical. \code{FALSE} if more information are returned.
#' Default \code{TRUE}.
#' @param ... subset condition that is compatible with \code{dplyr::filter}.
#' \code{survival::coxph} with \code{ties = "exact"} will be fitted on this
#' subset only. This argument could be useful to create a subset of data for
#' analysis when a trial consists of more than two arms. By default it is not
#' specified, all data will be used to fit the model. More than one conditions
#' can be specified in \code{...}, e.g.,
#' \code{fitLogrank(formula, data, arm \%in\% c('pbo', 'low dose'), x > 0.5)},
#' which is equivalent to
#' \code{fitLogrank(formula, data, arm \%in\% c('pbo', 'low dose') & x > 0.5)}.
#' Note that if more than one treatment arm are present in the data after
#' applying filter in \code{...}, models are fitted for placebo verse
#' each of the treatment arms.
#'
#' @returns a data frame with three columns:
#' \describe{
#' \item{\code{arm}}{name of the treatment arm. }
#' \item{\code{placebo}}{name of the placebo arm. }
#' \item{\code{p}}{one-sided p-value for log-rank test (treated vs placebo). }
#' \item{\code{info}}{the number of events of the endpoint in the subset. }
#' \item{\code{z}}{the z statistics of log hazard ratios. }
#' }
#'
#' @importFrom stats terms
#'
#' @export
#'
fitLogrank <- function(formula, placebo, data, alternative, ..., tidy = TRUE) {

  if(!inherits(formula, 'formula')){
    stop('formula must be a formula object with "arm" indicating the column arm in data. ')
  }

  is_valid_arm_formula <- function(formula, arm_var = "arm") {
    trms <- terms(formula, specials = "strata")
    labels <- attr(trms, "term.labels")
    strata_vars <- untangle.specials(trms, "strata", order = TRUE)$vars
    non_strata_vars <- setdiff(labels, strata_vars)
    length(non_strata_vars) == 1 && non_strata_vars == arm_var
  }

  if(!is_valid_arm_formula(formula, 'arm')){
    stop('formula should be in the format of Surv(time, event) ~ arm or Surv(time, event) ~ arm + strata(...) + ... + strata(...). ')
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

  # Prepare the data based on condition in ...
  filtered_data <- if(...length() == 0){
    data
  }else{
    tryCatch({
      data %>% dplyr::filter(...)
    },
    error = function(e){
      stop('Error in filtering data for logrank test. ',
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

    fit_cox <- fitCoxph(formula, placebo, sub_data, alternative,
                        scale = 'log hazard ratio', tidy = tidy) ## ... is not needed

    # Fit the Cox model
    lr <- tryCatch({
      coxph(formula, data = sub_data, ties = 'exact')
    }, error = function(e){
      stop('coxph model fitting failed in fitLogrank: ', e$message)
    })

    # calculate log rank statistic
    z <- sqrt(summary(lr)$sctest['test']) * ifelse(fit_cox$z > 0, 1, -1)
    p <- ifelse(alternative == 'greater', 1 - pnorm(z), pnorm(z))
    info <- fit_cox$info

    res <- data.frame(arm = trt_arm, placebo = placebo,
                      p = p, info = info, z = z
                    )
    if(!tidy){
      res$info_pbo <- fit_cox$info_pbo
      res$info_trt <- fit_cox$info_trt
      res$n_pbo <- fit_cox$n_pbo
      res$n_trt <- fit_cox$n_trt
    }

    ret <- rbind(ret, res)
  }

  rownames(ret) <- NULL
  class(ret) <- c('fit_logrank', class(ret))
  ret
}
