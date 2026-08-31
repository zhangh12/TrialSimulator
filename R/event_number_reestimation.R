#' Re-estimate the final event number from conditional power
#'
#' Find the smallest whole number of events at the final analysis, \code{D},
#' such that the conditional power of a two-stage group sequential design
#' reaches a target, given interim observations. The forward calculation is
#' \code{.conditional_power()}; this function inverts it in \code{D} by a
#' bracketing search followed by integer bisection, so the returned \code{D}
#' always satisfies \code{CP(D) >= target_cp} and \code{CP(D - 1) < target_cp}.
#' This is an internal function; the user-facing entry point is
#' \code{Trials$eventNumberReestimationFromConditionalPower()}, which extracts
#' \code{z} and \code{d} from locked data. It is deliberately not exported;
#' unit tests reach it via
#' \code{TrialSimulator:::.event_number_reestimation}.
#'
#' Arguments are vectorized over comparisons: \code{z}, \code{d},
#' \code{alpha}, \code{target_cp}, \code{D_cap} and \code{omega} can be
#' vectors of a common length (scalars are recycled), while \code{effect}
#' and \code{alternative} are scalars shared by all comparisons.
#'
#' A finite \code{D_cap} bounds the search: conditional power is evaluated
#' at the cap first, and when it cannot reach the target the cap itself is
#' returned. An infinite \code{D_cap} asks for the exact solution and is
#' only valid when conditional power approaches 1 as \code{D} grows, i.e.,
#' under a beneficial drift on the \code{'less'} scale (a negative interim
#' \code{z} for \code{effect = 'trend'}, or a beneficial hazard ratio for a
#' numeric \code{effect}); the caller is expected to have checked this.
#' \code{effect = 'null'} is not supported: under the null, conditional
#' power is bounded and re-estimation is meaningless.
#'
#' @param z numeric. Observed z statistic(s) at interim, in the sign
#' convention of \code{fitLogrank()}: \code{z > 0} corresponds to an
#' estimated hazard ratio greater than 1 (treatment vs placebo).
#' @param d numeric. Observed number of events at interim, counted on the
#' two arms of the comparison.
#' @param alpha numeric. The one-sided nominal significance level
#' corresponding to the final critical boundary, in (0, 1). See
#' \code{.conditional_power()}.
#' @param target_cp numeric. Target conditional power in (0, 1).
#' @param D_cap numeric. Upper bound(s) on the returned event number; whole
#' numbers greater than \code{d}, or \code{Inf} for an exact solution.
#' @param effect \code{'trend'} (extrapolate the interim estimate) or a
#' single positive numeric value interpreted as a hazard ratio.
#' @param omega numeric. Schoenfeld per-event information
#' \code{r / (1 + r)^2} where \code{r} is the allocation ratio of the pair.
#' Only used when \code{effect} is numeric; may be \code{NA} otherwise.
#' @param alternative \code{'greater'} or \code{'less'}.
#'
#' @return a numeric vector of whole numbers: for each comparison, the
#' smallest \code{D} with conditional power at least \code{target_cp}, or
#' \code{D_cap} when the target cannot be reached under the cap.
#'
#' @noRd
.event_number_reestimation <- function(z, d, alpha, target_cp, D_cap,
                                       effect, omega, alternative){

  stopifnot(alternative %in% c('greater', 'less'))
  stopifnot(!identical(effect, 'null'))
  stopifnot(all(d > 0))
  stopifnot(all(alpha > 0), all(alpha < 1))
  stopifnot(all(target_cp > 0), all(target_cp < 1))
  stopifnot(all(D_cap > d))

  n <- max(length(z), length(d), length(alpha), length(target_cp),
           length(D_cap), length(omega))
  z <- rep_len(z, n)
  d <- rep_len(d, n)
  alpha <- rep_len(alpha, n)
  target_cp <- rep_len(target_cp, n)
  D_cap <- rep_len(D_cap, n)
  omega <- rep_len(omega, n)

  ## an infinite cap requires conditional power to approach 1 as D grows,
  ## i.e., a beneficial drift on the 'less' scale; the method checks this
  ## with an informative error before calling the kernel
  z_less <- if(alternative == 'greater') -z else z
  beneficial <- if(identical(effect, 'trend')){
    z_less < 0
  }else{
    theta_less <- if(alternative == 'greater') -log(effect) else log(effect)
    rep(theta_less < 0, n)
  }
  stopifnot(all(is.finite(D_cap) | beneficial))

  cp_at <- function(i, D){
    .conditional_power(z = z[i], d = d[i], D = D, alpha = alpha[i],
                       effect = effect, omega = omega[i],
                       alternative = alternative)
  }

  D <- numeric(n)
  for(i in seq_len(n)){

    lo <- d[i] + 1
    if(cp_at(i, lo) >= target_cp[i]){
      D[i] <- lo
      next
    }

    if(is.finite(D_cap[i]) && cp_at(i, D_cap[i]) < target_cp[i]){
      D[i] <- D_cap[i]
      next
    }

    ## bracket the crossing: CP(lo) < target_cp <= CP(hi)
    hi <- 2 * lo
    repeat{
      if(hi >= D_cap[i]){
        hi <- D_cap[i]
        break
      }
      if(cp_at(i, hi) >= target_cp[i]){
        break
      }
      lo <- hi
      hi <- 2 * hi
    }

    ## integer bisection, keeping CP(lo) < target_cp <= CP(hi)
    while(hi - lo > 1){
      mid <- floor((lo + hi) / 2)
      if(cp_at(i, mid) >= target_cp[i]){
        hi <- mid
      }else{
        lo <- mid
      }
    }
    D[i] <- hi

  }

  D

}
