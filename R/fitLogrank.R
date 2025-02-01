#' Carry out log rank test
#' @description
#' Compute log rank test statistic on an endpoint.
#' @param endpoint character. Name of endpoint in \code{data}.
#' @param placebo character. String of placebo in \code{data}.
#' @param data data frame. Usually it is a locked data.
#' @param ... subset condition that is compatible with \code{dplyr::filter}.
#' \code{survival::survdiff} will be fitted on this subset only.
#' It could be useful when a
#' trial consists of more than two arms. By default it is not specified,
#' all data will be used to fit the model. More than one conditions can be
#' specified in \code{...}, e.g.,
#' \code{fitLogrank('pfs', 'pbo', data, arm \%in\% c('pbo', 'low dose'), pfs > 0.5)},
#' which is equivalent to
#' \code{fitLogrank('pfs', 'pbo', data, arm \%in\% c('pbo', 'low dose') & pfs > 0.5)}.
#' @returns a data frame with three columns:
#' \describe{
#' \item{\code{p}}{one-sided p-value for
#' log rank test (alternative hypothesis: risk is higher in placebo arm). }
#' \item{\code{info}}{the number of events of the endpoint in the subset. }
#' \item{\code{z}}{the one-sided logrank statistics. }
#' }
#'
#' @export
#'
fitLogrank <- function(endpoint, placebo, data, ...) {

  if(!is.character(endpoint) || length(endpoint) != 1){
    stop("endpoint must be a single character string")
  }

  if(!is.character(placebo) || length(placebo) != 1){
    stop("placebo must be a single character string")
  }

  if(!is.data.frame(data)){
    stop("data must be a data frame")
  }

  event <- paste0(endpoint, '_event')
  required_cols <- c('arm', endpoint, event)
  if(!all(required_cols %in% names(data))){
    stop('Columns <',
         paste0(setdiff(required_cols, names(data)), collapse = ', '),
         '> are not present in locked data. ',
         'Please check endpoint\'s name. ')
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

  # Create the formula
  formula_str <- paste0("Surv(time = ", endpoint,
                        ", event = ", endpoint, "_event) ~ ",
                        "I(arm == '", placebo, "')")

  # Fit the Cox model to get direction of effect
  fit <- summary(coxph(as.formula(formula_str), data = filtered_data))$coef

  # calculate log rank statistic
  lr <- survdiff(as.formula(formula_str), data = filtered_data)

  z <- ifelse(fit[1, 'coef']/fit[1, 'se(coef)'] < 0, -1, 1) * sqrt(lr$chisq)
  p <- 1 - pnorm(z)
  info <- sum(filtered_data[[event]] %in% 1)
  info_pbo <- sum(filtered_data[[event]] %in% 1 & filtered_data$arm %in% placebo)
  info_trt <- info - info_pbo
  n_pbo <- sum(filtered_data$arm %in% placebo)
  n_trt <- sum(!(filtered_data$arm %in% placebo))

  ret <- data.frame(p = p, info = info, z = z,
                    info_pbo = info_pbo, info_trt = info_trt,
                    n_pbo = n_pbo, n_trt = n_trt,
                    pbo = placebo,
                    trt = paste0(setdiff(unique(filtered_data$arm), placebo), collapse = ', ')
                    )
  class(ret) <- c('fit_logrank', class(ret))
  ret
}

