#' Fit Cox proportional hazard ratio model
#' @description
#' Fit coxph model on an endpoint.
#' @param endpoint character. Name of endpoint in \code{data}.
#' @param placebo character. String of placebo in \code{data}.
#' @param data data frame. Usually it is a locked data.
#' @param condition subset condition that is compatible with \code{dplyr::filter}.
#' coxph will be fitted on this subset only. It could be useful when a
#' trial consists of more than two arms. By default, \code{subset = NULL}
#' then all data will be used in fitted the model.
#' @returns a data frame with two columns: \code{p}, one-sided p-value for
#' log hazard ratio (alternative hypothesis: log(HR) < 0), and
#' \code{info}, the number of events of the endpoint in the subset.
#'
#' @export
#'
fitCoxph <- function(endpoint, placebo, data, condition = NULL) {

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

  # Prepare the data based on condition
  filtered_data <- if (missing(condition)) {
    data
  } else {
    condition_call <- substitute(condition)
    tryCatch({
      subset(data, eval(condition_call, data))
    },
    error = function(e) {
      stop('Error in filtering data for coxph modeling. ',
           'Please check \'subset\', ',
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
                        "I(arm != '", placebo, "')")

  # Fit the Cox model
  fit <- summary(coxph(as.formula(formula_str), data = filtered_data))$coef

  p <- pnorm(fit[1, 'coef']/fit[1, 'se(coef)'])
  info <- sum(filtered_data[[event]] %in% 1)

  data.frame(p = p, info = info)
}

