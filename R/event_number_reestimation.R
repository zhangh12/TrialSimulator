#' Re-estimate the final event number from conditional power
#'
#' Find the smallest whole number of events at the final analysis, \code{D},
#' such that the conditional power of a two-stage group sequential design
#' reaches a target, given interim observations. The forward calculation is
#' \code{.conditional_power()}. Conditional power need not be monotone in
#' \code{D}; this function finds its stationary points from a cubic equation,
#' partitions the integer search range into monotone intervals, and searches
#' them from left to right. The returned \code{D} is therefore the smallest
#' whole number not below the interim event number with
#' \code{CP(D) >= target_cp}.
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
#' A finite \code{D_cap} bounds the search. When no event number up to the cap
#' reaches the target, the cap itself is returned. An infinite \code{D_cap}
#' asks for the exact solution and is only valid when conditional power
#' approaches 1 as \code{D} grows, i.e.,
#' under a beneficial drift on the \code{'less'} scale (a negative interim
#' \code{z} for \code{effect = 'trend'}, or a beneficial hazard ratio for a
#' numeric \code{effect}); the caller is expected to have checked this.
#' \code{effect = 'null'} is not supported; use \code{.conditional_power()}
#' to calculate conditional type I error at a chosen event number.
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
#' When the interim statistic already reaches the final boundary, the
#' degenerate conditional power at \code{D = d} is one and \code{d} is
#' returned.
#'
#' @return a numeric vector of whole numbers: for each comparison, the
#' smallest \code{D >= d} with conditional power at least \code{target_cp},
#' or \code{D_cap} when the target cannot be reached under the cap.
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

  ## Work on the lower-tail scale for both alternatives. For a numeric effect,
  ## eta is the standardized drift accumulated over the interim information;
  ## under trend extrapolation it reduces to z_less.
  z_less <- if(alternative == 'greater') -z else z
  crit <- qnorm(alpha)
  eta <- if(identical(effect, 'trend')){
    z_less
  }else{
    theta_less <- if(alternative == 'greater') -log(effect) else log(effect)
    theta_less * sqrt(omega * d)
  }
  beneficial <- eta < 0
  crossed <- z_less <= crit

  ## An infinite search is guaranteed to terminate only on the beneficial
  ## tail, unless the final boundary has already been reached at D = d. The
  ## public method checks this with a more informative error.
  stopifnot(all(is.finite(D_cap) | beneficial | crossed))

  cp_at <- function(i, D){
    .conditional_power(z = z[i], d = d[i], D = D, alpha = alpha[i],
                       effect = effect, omega = omega[i],
                       alternative = alternative)
  }

  D <- numeric(n)
  for(i in seq_len(n)){

    ## EAST rule: D_new is the smallest D >= the event number observed at the
    ## interim. With no future information, the decision is deterministic; if
    ## the final boundary is already reached, CP(d) = 1 and no increase is
    ## needed.
    if(crossed[i]){
      D[i] <- d[i]
      next
    }

    D_min <- d[i] + 1
    if(cp_at(i, D_min) >= target_cp[i]){
      D[i] <- D_min
      next
    }

    ## With y = sqrt(D / d), stationary points of probit(CP(D)) solve
    ##
    ##   -eta*y^3 + (z_less + eta)*y - crit = 0.
    ##
    ## polyroot() uses coefficients in increasing order. Only real roots in
    ## the search range matter. The cubic may degenerate to a lower degree;
    ## polyroot() handles trailing zero coefficients directly.
    stationary <- polyroot(c(-crit[i], z_less[i] + eta[i], 0, -eta[i]))
    imag_tol <- sqrt(.Machine$double.eps)
    keep <- abs(Im(stationary)) <=
      imag_tol * (1 + abs(Re(stationary)))
    stationary <- sort(Re(stationary[keep]))
    stationary <- stationary[stationary > 1]

    stationary_D <- d[i] * stationary^2
    stationary_D <- stationary_D[is.finite(stationary_D) &
                                   stationary_D >= D_min]
    if(is.finite(D_cap[i])){
      stationary_D <- stationary_D[stationary_D <= D_cap[i]]
      D_hi <- D_cap[i]
    }else{
      ## Start beyond every stationary point, then expand on the final
      ## increasing branch until the target is bracketed.
      D_hi <- max(2 * D_min,
                  if(length(stationary_D) > 0){
                    ceiling(max(stationary_D)) + 1
                  }else{
                    0
                  })
      while(cp_at(i, D_hi) < target_cp[i]){
        next_hi <- 2 * D_hi
        if(!is.finite(next_hi) || next_hi <= D_hi){
          stop('Unable to bracket the target conditional power with a ',
               'finite event number. Specify a finite D_cap. ')
        }
        D_hi <- next_hi
      }
    }

    ## A continuous stationary point may lie between two integers. Including
    ## both neighboring integers makes every interval below monotone on the
    ## integer grid. Search intervals from left to right so the first upward
    ## crossing is the EAST solution even when CP later falls and recovers.
    knots <- sort(unique(c(D_min,
                           floor(stationary_D), ceiling(stationary_D),
                           D_hi)))
    knots <- knots[knots >= D_min & knots <= D_hi]

    found <- FALSE
    if(length(knots) > 1){
      for(k in seq_len(length(knots) - 1L)){
        lo <- knots[k]
        hi <- knots[k + 1L]
        cp_lo <- cp_at(i, lo)

        if(cp_lo >= target_cp[i]){
          D[i] <- lo
          found <- TRUE
          break
        }

        if(cp_at(i, hi) >= target_cp[i]){
          ## On this monotone interval, keep
          ## CP(lo) < target_cp <= CP(hi).
          while(hi - lo > 1){
            mid <- floor((lo + hi) / 2)
            if(cp_at(i, mid) >= target_cp[i]){
              hi <- mid
            }else{
              lo <- mid
            }
          }
          D[i] <- hi
          found <- TRUE
          break
        }
      }
    }

    if(!found){
      ## This can occur only with a finite cap: no integer in the permitted
      ## range reaches the target.
      D[i] <- D_cap[i]
    }

  }

  D

}
