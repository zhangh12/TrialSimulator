#' Carry out log rank test
#' @description
#' Compute log rank test statistic on an endpoint.
#'
#' Refer to \href{https://zhangh12.github.io/TrialSimulator/articles/wrappers.html}{this vignette}
#' for more information and examples.
#'
#' @param formula An object of class \code{formula} that can be used with
#' \code{survival::survdiff}. Must consist \code{arm} and endpoint in \code{data}.
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
#' The log rank test (\code{survival::survdiff}) is carried out on this
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
#' @returns a data frame with columns:
#' \describe{
#' \item{\code{arm}}{name of the treatment arm. }
#' \item{\code{placebo}}{name of the placebo arm. }
#' \item{\code{p}}{one-sided p-value for log-rank test (treated vs placebo). }
#' \item{\code{info}}{the number of events of the endpoint in the subset. }
#' \item{\code{z}}{the z statistic of the log rank test, with the sign of
#' the log hazard ratio of treatment vs placebo. }
#' }
#' If the statistic is undefined because its variance is zero (e.g., no
#' informative event comparison in the subset), a simulation placeholder
#' \code{z = 0} with the corresponding \code{p = 0.5} is returned with a
#' warning rather than an error, so that a few such replicates in a large
#' simulation do not require error handling in action functions. The
#' placeholder carries no evidence either way and is not a standardized
#' normal statistic.
#'
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

  if(!is.character(placebo) || length(placebo) != 1 ||
     is.na(placebo) || !nzchar(placebo)){
    stop("placebo must be a single character string and cannot be missing or empty")
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

  available_arms <- unique(as.character(filtered_data$arm[!is.na(filtered_data$arm)]))
  if(!(placebo %in% available_arms)){
    stop('placebo arm <', placebo,
         '> is not present after applying the subset condition. ')
  }

  treatment_arms <- sort(setdiff(available_arms, placebo))
  if(length(treatment_arms) == 0){
    stop('No treatment arm is present after applying the subset condition. ')
  }

  ret <- NULL

  for(trt_arm in treatment_arms){
    sub_data <- filtered_data[filtered_data$arm %in% c(placebo, trt_arm), , drop = FALSE]

    # Ensure arm is a factor with placebo and treatment
    sub_data$arm <- factor(sub_data$arm, levels = c(placebo, trt_arm))

    # (stratified) logrank test. survdiff() returns observed and expected
    # event counts per arm (per arm and stratum when strata() is in the
    # formula) and the variance of the signed score. For two arms the signed
    # logrank statistic is U / sqrt(V), where U is observed minus expected
    # events in the treatment arm; its sign is the sign of the log hazard
    # ratio of treatment vs placebo, and U^2 / V is the chi-square of
    # survdiff(), i.e., the score test of coxph(ties = 'exact').
    lr_error <- NULL
    lr <- tryCatch({
      withCallingHandlers(
        survdiff(formula, data = sub_data),
        warning = function(w){
          # survdiff() warns "NaNs produced" when its variance is zero (e.g.,
          # no event); that case is reported below with a specific warning
          if(grepl('NaNs produced', conditionMessage(w), fixed = TRUE)){
            invokeRestart('muffleWarning')
          }
        })
    }, error = function(e){
      lr_error <<- e
      NULL
    })

    ## With two groups, survdiff() can fail while inverting its variance
    ## before returning, when that scalar variance is zero (for example,
    ## when all subjects fail at one common time). Recover the counts in
    ## that degenerate case, recognized by the "singular" in the message of
    ## solve(); all other fitting errors remain errors.
    singular_variance <- FALSE
    if(is.null(lr)){
      singular_variance <- grepl('singular', conditionMessage(lr_error),
                                 ignore.case = TRUE)

      if(!singular_variance){
        stop('survdiff() failed in fitLogrank: ', conditionMessage(lr_error))
      }

      model_data <- model.frame(formula, data = sub_data)
      surv_obj <- model.response(model_data)
      event <- surv_obj[, ncol(surv_obj)] == 1
      model_arm <- as.character(model_data$arm)
      obs <- c(sum(event & model_arm == placebo),
               sum(event & model_arm == trt_arm))
      n <- c(sum(model_arm == placebo), sum(model_arm == trt_arm))
      U <- 0
      V <- 0
    }else{
      obs <- if(is.matrix(lr$obs)) rowSums(lr$obs) else lr$obs
      exp <- if(is.matrix(lr$exp)) rowSums(lr$exp) else lr$exp
      n   <- if(is.matrix(lr$n))   rowSums(lr$n)   else lr$n
      U <- obs[2] - exp[2]
      V <- lr$var[2, 2]
    }

    if(singular_variance || !is.finite(V) || V <= 0){
      # the statistic is undefined when V = 0. In a large simulation a few
      # such replicates are expected; report z = 0 (no evidence either way,
      # hence p = 0.5 under either alternative, consistent with z) with a
      # warning rather than an error, so that action functions need no
      # error handling for this case.
      warning('Logrank statistic of arm <', trt_arm, '> vs <', placebo,
              '> is undefined because its variance is zero; ',
              'the simulation placeholder z = 0 (p = 0.5) is returned. ', immediate. = TRUE)
      z <- 0
    }else{
      z <- unname(U / sqrt(V))
    }
    p <- if(alternative == 'greater') pnorm(z, lower.tail = FALSE) else pnorm(z)

    ## counts are integer, as they were when derived from the Cox model frame
    res <- data.frame(arm = trt_arm, placebo = placebo,
                      p = p, info = as.integer(round(sum(obs))), z = z
                    )
    if(!tidy){
      res$info_pbo <- as.integer(round(obs[1]))
      res$info_trt <- as.integer(round(obs[2]))
      res$n_pbo <- as.integer(round(n[1]))
      res$n_trt <- as.integer(round(n[2]))
    }

    ret <- rbind(ret, res)
  }

  rownames(ret) <- NULL
  class(ret) <- c('fit_logrank', class(ret))
  ret
}
