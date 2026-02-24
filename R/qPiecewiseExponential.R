#' Quantile Function of Piecewise Exponential Distribution
#'
#' @description
#' To generate endpoint that follows a piecewise exponential distribution and
#' is correlated with other endpoints, the copula method is commonly used. The
#' quantile function needs to be specified (e.g., in the \code{simdata} package).
#' If a piecewise exponential distributed endpoint is independent to other
#' endpoints, one can simply use
#' \code{TrialSimulator::PiecewiseConstantExponentialRNG()}
#' to specify the \code{generator} argument in \code{endpoint()}.
#'
#' There are many R packages implementing the quantile function of the
#' piecewise exponential distributed random variable. Why do I implement it
#' again? The reason is that this function is extremely important for simulating
#' time-to-event endpoint in clinical trial simulation, thus the speed matters.
#' \code{qPiecewiseExponential()} is implemented purely in R for
#' code transparency, and is much faster than other packages.
#'
#' @param p numeric. A vector of probabilities.
#' @param times numeric. A vector of time points where risk (event
#' rates) change. 0 shouldn't be in \code{times}.
#' @param piecewise_risk numeric. A vector of constant risk (event rates) in
#' a time window. \code{length(piecewise_risk) = length(times) + 1}. To fully
#' specify a piecewise exponential distribution, the number of risk parameters
#' is one greater than the number of changepoints in \code{times}.
#'
#' @returns A vector of quantiles.
#' @export
#'
#' @examples
#' ## This code snippet can take > 10s to execute
#' if(interactive()){
#' library(TrialSimulator)
#'
#' run <- TRUE
#' if(!requireNamespace("rpact", quietly = TRUE)){
#'   run <- FALSE
#'   message("Please install 'rpact' to run this example.")
#' }
#'
#' if(!requireNamespace("PWEXP", quietly = TRUE)){
#'   run <- FALSE
#'   message("Please install 'PWEXP' to run this example.")
#' }
#'
#' if(!requireNamespace("PWEALL", quietly = TRUE)){
#'   run <- FALSE
#'   message("Please install 'PWEALL' to run this example.")
#' }
#'
#' if(run){
#'
#' x <- solvePiecewiseConstantExponentialDistribution(
#'   surv_prob = c(.9, .75, .64, .42, .28),
#'   times = c(.4, 1.2, 4, 5.5, 9)
#' )
#'
#' p <- stats::runif(1e5)
#'
#' ## fast, and only five lines of R codes
#' message('TrialSimulator::qPiecewiseExponential(): ')
#' system.time(
#'   a <- qPiecewiseExponential(
#'     p, times = x$end_time, piecewise_risk = c(x$piecewise_risk, .1)
#'   )
#' ) |> print()
#'
#' ## > 10s
#' message('rpact::getPiecewiseExponentialQuantile(): ')
#' system.time(
#'   b <- rpact::getPiecewiseExponentialQuantile(
#'     p, piecewiseSurvivalTime = c(0, x$end_time),
#'     piecewiseLambda=c(x$piecewise_risk, .1)
#'   )
#' ) |> print()
#'
#' ## equally fast, but implemented in Fortran
#' message('PWEALL::qpwe(): ')
#' system.time(
#'   c <- PWEALL::qpwe(p, rate = c(x$piecewise_risk, .1), tchange = c(0, x$end_time))$q
#' ) |> print()
#'
#' ## equally fast, long codes in R (maybe more versatile?)
#' message('PWEXP::qpwexp(): ')
#' system.time(
#'   d <- PWEXP::qpwexp(p, rate = c(x$piecewise_risk, .1), breakpoint = x$end_time)
#' ) |> print()
#'
#' message('a == b: ')
#' all.equal(a, b) |> print()
#' message('a == c: ')
#' all.equal(a, c) |> print()
#' message('a == d: ')
#' all.equal(a, d) |> print()
#'
#' }
#' }
qPiecewiseExponential <- function(p, times, piecewise_risk){

  if(length(piecewise_risk) != length(times) + 1){
    stop('length(piecewise_risk) must equal length(times) + 1. ')
  }

  if(any(times <= 0)){
    stop('times must be positive. ')
  }

  if(any(piecewise_risk < 0)){
    stop('piecewise_risk must be non-negative. ')
  }

  if(any(diff(times) <= 0)){
    stop('times must be strictly monotonous increasing. ')
  }

  if(0 %in% times){
    stop('times are time points where event rate change. Do not put 0 in times. ')
  }

  d <- c(0, times) # index: d_0, d_1, ..., d_r
  lam <- c(0, piecewise_risk) # index: lam_0, lam_1, ..., lam_{r+1}

  tmp <- cumsum(diff(lam) * d)
  idx <- findInterval(p, 1 - exp(-lam[-1] * d + tmp), rightmost.closed = TRUE)

  (tmp[idx] - log(1 - p))/lam[-1][idx]

}

