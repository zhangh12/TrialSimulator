#' Define a Trial
#'
#' @description
#' Define a trial. This is a user-friendly wrapper for
#' the class constructor \code{Trial$new()}. Users who are not familiar with
#' the concept of classes may consider using this wrapper directly.
#'
#' Trial's name, planned size/duration, enrollment plan,
#' dropout mechanism and seeding are specified in this function. Note that
#' many of these parameters can be altered adaptively during a trial.
#'
#' Note that it is users' responsibility to assure that the units of dropout
#' time, trial duration, and readout of non-tte endpoints are consistent.
#'
#' @param name character. Name of trial. Usually, hmm..., useless.
#' @param n_patients integer. Maximum (and initial) number of patients
#' could be enrolled when planning the trial. It can be altered adaptively
#' during a trial.
#' @param duration Numeric. Trial duration. It can be altered adaptively
#' during a trial.
#' @param description character. Optional for description of the trial. By
#' default it is set to be trial's \code{name}. Usually useless.
#' @param seed random seed. If \code{NULL}, seed is set for each simulated
#' trial automatically and saved in output. It can be retrieved in the
#' \code{seed} column in \code{$get_output()}. Setting it to be \code{NULL}
#' is recommended. For debugging, set it to a specific integer.
#' @param enroller a function returning a vector enrollment time for
#' patients. Its first argument \code{n} is the number of enrolled patients.
#' Set it to \code{StaggeredRecruiter} can handle most of the use cases.
#' See \code{?TrialSimulator::StaggeredRecruiter} for more information.
#' @param dropout a function returning a vector of dropout time for patients.
#' It can be any random number generator with first argument \code{n},
#' the number of enrolled patients. Usually \code{rexp} if dropout rate
#' is set at a single time point, or \code{rweibull} if dropout rates are
#' set at two time points. See \code{?TrialSimulator::weibullDropout}.
#' @param silent logical. \code{TRUE} to mute messages. However, warning
#' message is still displayed. Usually set it to \code{TRUE} in formal
#' simulation. Default: \code{FALSE}.
#' @param ... (optional) arguments of \code{enroller} and \code{dropout}.
#'
#' @examples
#' risk1 <- data.frame(
#'   end_time = c(1, 10, 26.0, 52.0),
#'   piecewise_risk = c(1, 1.01, 0.381, 0.150) * exp(-3.01)
#' )
#'
#' pfs1 <- endpoint(name = 'pfs', type='tte',
#'           generator = PiecewiseConstantExponentialRNG,
#'           risk = risk1, endpoint_name = 'pfs')
#'
#' orr1 <- endpoint(
#'   name = 'orr', type = 'non-tte',
#'   readout = c(orr=1), generator = rbinom,
#'   size = 1, prob = .4)
#'
#' placebo <- arm(name = 'pbo')
#'
#' placebo$add_endpoints(pfs1, orr1)
#'
#' risk2 <- risk1
#' risk2$hazard_ratio <- .8
#'
#' pfs2 <- endpoint(name = 'pfs', type='tte',
#'           generator = PiecewiseConstantExponentialRNG,
#'           risk = risk2, endpoint_name = 'pfs')
#'
#' orr2 <- endpoint(
#'   name = 'orr', type = 'non-tte',
#'   generator = rbinom, readout = c(orr=3),
#'   size = 1, prob = .6)
#'
#' active <- arm(name = 'ac')
#'
#' active$add_endpoints(pfs2, orr2)
#'
#' ## Plan a trial, Trial-3415, of up to 100 patients.
#' ## Enrollment time follows an exponential distribution, with median 5
#' trial <- trial(
#'   name = 'Trial-3415', n_patients = 100,
#'   seed = 31415926, duration = 100,
#'   enroller = rexp, rate = log(2) / 5)
#'
#' trial
#'
#' trial$add_arms(sample_ratio = c(1, 2), placebo, active)
#'
#' ## updated information after arms are registered
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
    silent = FALSE,
    ...
  ){

    Trials$new(
      name = name,
      n_patients = n_patients,
      duration = duration,
      description = description,
      seed = seed,
      enroller = enroller,
      dropout = dropout,
      silent = silent,
      ...
    )

  }
