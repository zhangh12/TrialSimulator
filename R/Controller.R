#' Define a Controller
#'
#' @description
#' Define a controller of a trial. This is a user-friendly wrapper for
#' the class constructor \code{Controller$new()}. Users who are not familiar with
#' the concept of classes may consider using this wrapper directly.
#'
#' \code{TrialSimulator} uses a controller to coordinate a trial object
#' and a listener object to run simulations, in which the trial object defines
#' endpoints, arms, and other settings of a trial, while the listener object
#' monitors trials to triggered pre-defined milestones and execute action
#' functions. See vignettes of this package for more examples.
#'
#' @param trial an object returned from \code{trial()}.
#' @param listener an object returned from \code{listener()}.
#'
#' @examples
#'
#' # a minimum, meaningful, and executable example,
#' # where a randomized trial with two arms is simulated and analyzed.
#'
#' control <- arm(name = 'control arm')
#' active <- arm(name = 'active arm')
#'
#' pfs_in_control <- endpoint(name = 'PFS', type = 'tte',
#'                            generator = rexp, rate = log(2) / 5)
#' control$add_endpoints(pfs_in_control)
#'
#' pfs_in_active <- endpoint(name = 'PFS', type = 'tte',
#'                           generator = rexp, rate = log(2) / 6)
#' active$add_endpoints(pfs_in_active)
#'
#' accrual_rate <- data.frame(end_time = c(10, Inf),
#'                            piecewise_rate = c(30, 50))
#' trial <- trial(name = 'trial',
#'                n_patients = 1000,
#'                duration = 40,
#'                enroller = StaggeredRecruiter,
#'                accrual_rate = accrual_rate,
#'                dropout = rweibull, shape = 2, scale = 38)
#'
#' trial$add_arms(sample_ratio = c(1, 1), control, active)
#'
#' action_at_final <- function(trial, milestone_name){
#'   locked_data <- trial$get_locked_data(milestone_name)
#'   fitLogrank(Surv(PFS, PFS_event) ~ arm, placebo = 'control arm',
#'              data = locked_data, alternative = 'less')
#'   invisible(NULL)
#' }
#'
#' final <- milestone(name = 'final analysis',
#'                    action = action_at_final,
#'                    when = calendarTime(time = 40))
#'
#' listener <- listener()
#' listener$add_milestones(final)
#'
#' controller <- controller(trial, listener)
#' controller$run(n = 1)
#'
#' @export
#'
controller = function(trial, listener){

  Controllers$new(trial = trial, listener = listener)

}
