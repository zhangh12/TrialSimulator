#' Calculate Parameters of Weibull Distribution as a Dropout Method
#'
#' @description
#' Fit scale and shape parameters of the Weibull distribution to match
#' dropout rates at two specified time points. Weibull distribution can be
#' used as a dropout distribution because it has two parameters.
#'
#' Note that It is users' responsibility to assure that the units of dropout
#' time, readout of non-tte endpoints, and trial duration are consistent.
#'
#' @param time a numeric vector of two time points at which dropout rates are
#' specified.
#' @param dropout_rate a numeric vector of dropout rates at \code{time}.
#'
#' @returns a named vector for scale and shape parameters.
#'
#' @examples
#' ## dropout rates are 8% and 18% at time 12 and 18.
#' weibullDropout(time = c(12, 18), dropout_rate = c(.08, .18))
#'
#'
#' @export
weibullDropout <- function(time, dropout_rate){

  if(length(time) != 2){
    stop('time should be of length 2. ')
  }

  if(length(dropout_rate) != 2){
    stop('dropout_rate should be of length 2. ')
  }

  if(!(all(time > 0))){
    stop('time should be positive')
  }

  if(!(all(dropout_rate > 0) && all(dropout_rate < 1))){
    stop('dropout_rate should be between 0 and 1')
  }

  shape <- diff(log(-log(1 - dropout_rate))) / diff(log(time))
  scale <- exp(log(time[1]) - log(-log(1 - dropout_rate[1]))/shape)

  stopifnot(max(abs(1 - exp(-(time / scale)^shape) - dropout_rate)) < 1e-4)
  c(shape = shape, scale = scale)

}
