#' Define a Trial
#'
#' @description
#' Define a trial. This is a user-friendly wrapper for
#' the class constructor \code{Trial$new}. Users who are not familiar with
#' the concept of classes may consider using this wrapper directly.
#'
#' @param name character. Name of trial.
#' @param n_patients integer. Maximum number of patients could be enrolled
#' to the trial.
#' @param duration Numeric. Trial duration.
#' @param description character. Optional for description of the trial. By
#' default it is set to be trial's \code{name}.
#' @param seed random seed. If \code{NULL}, \code{set.seed()} will not be
#' called, which uses seed set outside.
#' @param enroller a function returning a vector enrollment time for
#' patients. Its first argument is the number of enrolled patients.
#' @param dropout a function returning a vector of dropout time for
#' patients. Its first argument is the number of enrolled patients.
#' @param ... arguments of \code{enroller} and \code{dropout}.
#'
#' @examples
#' risk1 <- data.frame(
#'   end_time = c(1, 10, 26.0, 52.0),
#'   piecewise_risk = c(1, 1.01, 0.381, 0.150) * exp(-3.01)
#' )
#'
#' pfs1 <- endpoints(name = 'pfs', type='tte',
#'           generator = PiecewiseConstantExponentialRNG,
#'           risk = risk1, endpoint_name = 'pfs')
#'
#' orr1 <- endpoints(
#'   name = 'orr', type = 'non-tte',
#'   readout = c(orr=1), generator = rbinom,
#'   size = 1, prob = .4)
#'
#' placebo <- arm(name = 'pbo', description = 'Placebo arm')
#'
#' placebo$add_endpoints(pfs1, orr1)
#'
#' risk2 <- risk1
#' risk2$hazard_ratio <- .8
#'
#' pfs2 <- endpoints(name = 'pfs', type='tte',
#'           generator = PiecewiseConstantExponentialRNG,
#'           risk = risk2, endpoint_name = 'pfs')
#'
#' orr2 <- endpoints(
#'   name = 'orr', type = 'non-tte',
#'   generator = rbinom, readout = c(orr=3),
#'   size = 1, prob = .6)
#'
#' active <- arm(name = 'ac', description = 'Active arm')
#'
#' active$add_endpoints(pfs2, orr2)
#'
#' ## Plan a trial, Trial-3415, of up to 100 patients.
#' ## Enrollment time follows an exponential distribution, with median 5
#' trial <- trial(
#'   name = 'Trial-3415', n_patients = 100,
#'   seed = 31415926, duration = 100,
#'   enroller = rexp, rate = log(2) / 5)
#' trial$add_arms(sample_ratio = c(1, 2), placebo, active)
#'
#' trial
#'
#' @export
#'
trial =
  function(
    name,
    n_patients,
    duration,
    description = name,
    seed = NULL,
    enroller,
    dropout = NULL,
    ...
  ){

    Trial$new(
      name = name,
      n_patients = n_patients,
      duration = duration,
      description = description,
      seed = seed,
      enroller = enroller,
      dropout = dropout,
      ...
    )

  }
