#' Re-estimate the final event number from conditional power
#'
#' Find the smallest whole number of events at the final analysis, \code{D},
#' such that the conditional power of a two-stage group sequential design
#' reaches a target, given interim observations. The forward calculation is
#' \code{.conditional_power()}. Conditional power need not be monotone in
#' \code{D}; this function finds its stationary points from a cubic equation,
#' partitions the integer search range into monotone intervals, and searches
#' them from left to right. The returned \code{D} is therefore the smallest
#' whole number strictly greater than the interim event number with
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
#' A finite \code{D_cap} bounds the search, while an infinite \code{D_cap}
#' requests an unbounded search. When no finite event number in the requested
#' search range reaches the target, \code{NA_real_} is returned.
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
#' numbers greater than \code{d}, or \code{Inf} for an unbounded search.
#' @param effect \code{'trend'} (extrapolate the interim estimate) or a
#' single positive numeric value interpreted as a hazard ratio.
#' @param omega numeric. Schoenfeld per-event information
#' \code{r / (1 + r)^2} where \code{r} is the allocation ratio of the pair.
#' Only used when \code{effect} is numeric; may be \code{NA} otherwise.
#' @param alternative \code{'greater'} or \code{'less'}.
#'
#' @return a numeric vector: for each comparison, the smallest whole number
#' \code{D > d} with conditional power at least \code{target_cp}, or
#' \code{NA_real_} when no finite solution exists in the requested range.
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
  cp_at <- function(i, D){
    .conditional_power(z = z[i], d = d[i], D = D, alpha = alpha[i],
                       effect = effect, omega = omega[i],
                       alternative = alternative)
  }

  D <- rep(NA_real_, n)
  for(i in seq_len(n)){

    ## Re-estimation is restricted to a genuine future final analysis, so the
    ## integer search starts at the first event number strictly above d. This
    ## also applies when the interim z already reaches the final boundary:
    ## continuing the trial can dilute that result.
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
      ## First search every bounded monotone interval. D_hi lies beyond all
      ## stationary points, at the start of the final monotone branch.
      D_hi <- max(2 * D_min,
                  if(length(stationary_D) > 0){
                    ceiling(max(stationary_D)) + 1
                  }else{
                    0
                  })
      if(!is.finite(D_hi)){
        stop('Unable to search for a finite event number without numeric ',
             'overflow. ')
      }
    }

    ## A continuous stationary point may lie between two integers. Including
    ## both neighboring integers makes every interval below monotone on the
    ## integer grid. Search intervals from left to right so the first upward
    ## crossing is returned even when CP later falls and recovers.
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

    if(found){
      next
    }

    if(is.finite(D_cap[i])){
      ## No integer in the permitted range reaches the target.
      next
    }

    ## On the final monotone branch, CP tends to 1 under beneficial drift,
    ## to alpha under zero drift, and to 0 under adverse drift. If that limit
    ## is not above the target, all possible earlier crossings have already
    ## been excluded by the interval search above.
    limit_cp <- if(eta[i] < 0){
      1
    }else if(eta[i] > 0){
      0
    }else{
      alpha[i]
    }
    if(limit_cp <= target_cp[i]){
      ## No finite solution exists on the unbounded final branch.
      next
    }

    ## The final branch must now cross the target. Expand until it is
    ## bracketed, then use integer bisection.
    lo <- D_hi
    hi <- 2 * lo
    while(!is.finite(hi) || cp_at(i, hi) < target_cp[i]){
      if(!is.finite(hi) || hi <= lo){
        stop('Unable to bracket the target conditional power with a ',
             'finite event number before numeric overflow. ')
      }
      lo <- hi
      hi <- 2 * hi
    }
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
