#' Fit Cox proportional hazard ratio model
#'
#' @description
#' Fit Cox proportional hazards model on an endpoint.
#'
#' @param endpoint Character. Name of the endpoint in \code{data}.
#' @param placebo Character. String indicating the placebo in \code{data}.
#' @param data Data frame. Usually it is a locked data set.
#' @param ... Subset conditions compatible with \code{dplyr::filter}.
#' \code{coxph} will be fitted on this subset only. This can be useful
#' when a trial consists of more than two arms. By default, it is not specified,
#' and all data will be used to fit the model. More than one condition can be
#' specified in \code{...}, e.g.,
#' \code{fitCoxph('pfs', 'pbo', data, arm \%in\% c('pbo', 'low dose'), pfs > 0.5)},
#' which is equivalent to:
#' \code{fitCoxph('pfs', 'pbo', data, arm \%in\% c('pbo', 'low dose') & pfs > 0.5)}.
#'
#' @returns a data frame with three columns:
#' \describe{
#' \item{\code{p}}{one-sided p-value for
#' log hazard ratio (alternative hypothesis: log hazard ratio > 0). }
#' \item{\code{info}}{the number of events of the endpoint in the subset. }
#' \item{\code{z}}{the z statistics of log hazard ratios. }
#' }
#'
#' @export
#'
fitCoxph <- function(endpoint, placebo, data, ...) {

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

  # Create the formula
  formula_str <- paste0("Surv(time = ", endpoint,
                        ", event = ", endpoint, "_event) ~ ",
                        "I(arm == '", placebo, "')")

  # Fit the Cox model
  fit <- summary(coxph(as.formula(formula_str), data = filtered_data))$coef

  z <- fit[1, 'coef']/fit[1, 'se(coef)']
  p <- 1 - pnorm(z)
  info <- sum(filtered_data[[event]] %in% 1)

  ret <- data.frame(p = p, info = info, z = z)
  class(ret) <- c('fit_coxph', class(ret))
  ret
}

