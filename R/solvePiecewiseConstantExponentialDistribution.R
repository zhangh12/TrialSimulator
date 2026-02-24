#' Compute Constant Rates of Piecewise Exponential Distribution
#'
#' @description
#' This function computes the rate parameters \code{lambda} in each of the time
#' windows. \code{lambda} are natural parameters of piecewise exponential
#' distribution, but in practice, users may define the distribution by
#' specifying the survival probabilities at time points where event rates
#' change.
#'
#' This function returns a data frame, which can be used as input of the
#' argument \code{risk} of the data generator \code{PiecewiseConstantExponentialRNG}.
#'
#' @param surv_prob numeric. A vector of survival probabilities at \code{times}.
#' @param times numeric. A vector of time points where event rates change.
#' \code{times} and \code{surv_prob} must be of equal length.
#'
#' @returns
#' a data frame of two columns
#' \describe{
#' \item{\code{end_time}}{End time for a constant event rate. The start time of
#' the first time window is 0. }
#' \item{piecewise_risk}{A constant event rate in the time window ending with
#' \code{end_time} on the same row. }
#' }
#' @export
#'
#' @examples
#' solvePiecewiseConstantExponentialDistribution(
#'   surv_prob = c(.9, .75, .64, .42, .28),
#'   times = c(.4, 1.2, 4, 5.5, 9)
#' )
#'
solvePiecewiseConstantExponentialDistribution <- function(surv_prob, times){

  if(length(times) != length(surv_prob)){
    stop('times and surv_prob must be of the same length. ')
  }

  if(any(surv_prob <= 0) || any(surv_prob > 1)){
    stop('surv_prob must be within (0, 1]. ')
  }

  if(any(times < 0)){
    stop('times must be non-negative. ')
  }

  surv_prob <- surv_prob[order(times, decreasing = FALSE)]
  times <- sort(times, decreasing = FALSE)

  if(any(diff(surv_prob) >= 0)){
    stop('surv_prob must be increasing over time')
  }

  if(times[1] != 0){
    times <- c(0, times)
    surv_prob <- c(1.0, surv_prob)
  }

  lambda <- -diff(log(surv_prob))/diff(times)
  end_time <- times[-1]
  data.frame(end_time, piecewise_risk = lambda)

}
