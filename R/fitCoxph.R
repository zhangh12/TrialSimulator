#' Fit Cox proportional hazard ratio model
#'
#' @description
#' Fit Cox proportional hazards model on an time-to-event endpoint.
#'
#' @param formula An object of class \code{formula} that can be used with
#' \code{survival::coxph}. Must include \code{arm} and endpoint in \code{data}.
#' Covariates can be adjusted. Interactions between \code{arm} and covariates are
#' allowed in \code{formula}, but \code{arm} must has a term of main effect,
#' and only estimate of that main effect is tested.
#' @param placebo Character. String indicating the placebo in \code{data$arm}.
#' @param data Data frame. Usually it is a locked data set.
#' @param alternative a character string specifying the alternative hypothesis,
#' must be one of \code{"greater"} or \code{"less"}. No default value.
#' \code{"greater"} means superiority of treatment over placebo is established
#' by an hazard ratio greater than 1.
#' @param scale character. The type of estimate in the output. Must be one
#' of \code{"log hazard ratio"} or \code{"hazard ratio"}. No default value.
#' @param ... Subset conditions compatible with \code{dplyr::filter}.
#' \code{coxph} will be fitted on this subset only. This argument can be useful
#' to create a subset of data for analysis when a trial consists of more
#' than two arms. By default, it is not specified,
#' all data will be used to fit the model. More than one condition can be
#' specified in \code{...}, e.g.,
#' \code{fitCoxph('pfs', 'pbo', data, 'less', 'log hazard ratio', arm \%in\% c('pbo', 'low dose'), pfs > 0.5)},
#' which is equivalent to:
#' \code{fitCoxph('pfs', 'pbo', data, 'less', 'log hazard ratio', arm \%in\% c('pbo', 'low dose') & pfs > 0.5)}.
#'
#' @returns a data frame with three columns:
#' \describe{
#' \item{\code{arm}}{name of the treatment arm. }
#' \item{\code{placebo}}{name of the placebo arm. }
#' \item{\code{estimate}}{estimate of main effect of arm, depending on \code{scale}. }
#' \item{\code{p}}{one-sided p-value for
#' log hazard ratio (treated vs placebo). }
#' \item{\code{info}}{the number of events of the endpoint in the subset. }
#' \item{\code{z}}{the z statistics of log hazard ratios. }
#' }
#'
#' @export
#'
fitCoxph <- function(formula, placebo, data, alternative, scale, ...) {

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

  valid_scales <- c('log hazard ratio', 'hazard ratio')
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
      stop('Error in filtering data for coxph test. ',
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

    # Fit the Cox model
    fit <- tryCatch({
      coxph(formula, data = sub_data, model = TRUE)
    }, error = function(e){
      stop('coxph model fitting failed: ', e$message)
    })

    sfit <- summary(fit)

    ## identify coefficient of treatment arm (main effect)
    coef_row <- grep(paste0('^arm', trt_arm, '$'), rownames(sfit$coef))
    if(length(coef_row) != 1){
      stop('fixCoxph: unable to identify arm main effect coefficient for arm <', trt_arm,
           '>. Please use <arm> in formula explicitly. ')
    }

    estimate <- sfit$coef[coef_row, 'coef']
    se <- sfit$coef[coef_row, 'se(coef)']
    z <- estimate / se
    p <- ifelse(alternative == 'greater', 1 - pnorm(z), pnorm(z))

    model_data <- model.frame(fit)
    surv_obj <- model.response(model_data)
    info <- sum(surv_obj[, 'status'] == 1)

    ret <- rbind(ret, data.frame(arm = trt_arm, placebo = placebo,
                                 estimate = ifelse(scale == 'log hazard ratio',
                                                   estimate, exp(estimate)),
                                 p = p, info = info, z = z
                                 )
                 )
  }

  class(ret) <- c('fit_coxph', class(ret))
  ret
}

