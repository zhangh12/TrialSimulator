#' Conditional power of a two-stage group sequential design
#'
#' Compute conditional power on the canonical z scale for a design with one
#' interim and one final analysis, assuming the trial continues as designed:
#' constant allocation ratio, planned final statistic, and the planned final
#' boundary represented by the one-sided nominal level \code{alpha}. This is
#' an internal function;
#' the user-facing entry point is \code{Trials$conditionalPower()}, which
#' extracts \code{z} and \code{d} from locked data. It is deliberately not
#' exported; unit tests reach it via \code{TrialSimulator:::.conditional_power}.
#'
#' Arguments are vectorized over comparisons: \code{z}, \code{d}, \code{D},
#' \code{alpha} and \code{omega} can be vectors of a common length (scalars
#' are recycled), while \code{effect} and \code{alternative} are scalars
#' shared by all comparisons.
#'
#' Under \code{alternative = 'less'}, rejection at the final analysis is
#' \code{Z <= qnorm(alpha)} and negative drift favors treatment. The
#' \code{'greater'} case is mapped onto the \code{'less'} scale by negating
#' \code{z} and the drift.
#'
#' @param z numeric. Observed z statistic(s) at interim, in the sign
#' convention of \code{fitLogrank()}: \code{z > 0} corresponds to an
#' estimated hazard ratio greater than 1 (treatment vs placebo).
#' @param d numeric. Observed number of events at interim, counted on the
#' two arms of the comparison.
#' @param D numeric. Planned number of events at the final analysis, counted
#' on the same two arms. Must satisfy \code{d < D}.
#' @param alpha numeric. The one-sided nominal significance level
#' corresponding to the final critical boundary, in (0, 1). If \code{c} is
#' the final critical value on the z scale, \code{alpha = pnorm(c)} for
#' \code{alternative = 'less'} and \code{alpha = 1 - pnorm(c)} for
#' \code{alternative = 'greater'}. In a group sequential design, this
#' boundary-derived nominal level generally differs from the total design
#' alpha and from the cumulative or incremental alpha spent at the final
#' look.
#' @param effect \code{'trend'} (extrapolate the interim estimate),
#' \code{'null'} (conditional type I error), or a single positive numeric
#' value interpreted as a hazard ratio.
#' @param omega numeric. Schoenfeld per-event information
#' \code{r / (1 + r)^2} where \code{r} is the allocation ratio of the pair
#' (e.g., 1/4 under 1:1). Only used when \code{effect} is numeric; may be
#' \code{NA} otherwise.
#' @param alternative \code{'greater'} or \code{'less'}.
#'
#' @return a numeric vector of conditional powers.
#'
#' @noRd
.conditional_power <- function(z, d, D, alpha, effect, omega, alternative){

  stopifnot(alternative %in% c('greater', 'less'))
  stopifnot(all(d > 0), all(d < D))
  stopifnot(all(alpha > 0), all(alpha < 1))

  ## mirror the 'greater' problem onto the 'less' scale
  if(alternative == 'greater'){
    z <- -z
  }

  t <- d / D
  crit <- qnorm(alpha)

  if(identical(effect, 'trend')){
    ## theta_hat = z / sqrt(omega * d); omega cancels in the drift:
    ## sqrt(t) * z + theta_hat * sqrt(omega * D) * (1 - t) = z * sqrt(D / d)
    num <- crit - z * sqrt(D / d)
  }else if(identical(effect, 'null')){
    num <- crit - sqrt(t) * z
  }else{
    stopifnot(is.numeric(effect), length(effect) == 1, effect > 0)
    ## omega = r / (1 + r)^2 is at most 1/4 (attained at r = 1)
    stopifnot(all(!is.na(omega)), all(omega > 0), all(omega <= 0.25))
    theta <- log(effect)
    if(alternative == 'greater'){
      theta <- -theta
    }
    num <- crit - sqrt(t) * z - theta * sqrt(omega * D) * (1 - t)
  }

  unname(pnorm(num / sqrt(1 - t)))
}
