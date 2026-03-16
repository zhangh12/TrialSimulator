#' Define a Regime
#'
#' @description
#' Define a regime of a trial. This is a user-friendly wrapper for
#' the class constructor \code{Regimes$new()}. Users who are not familiar with
#' the concept of classes may consider using this wrapper directly.
#'
#' A regime defines the rules to select patients who
#' switch treatments, to determine the time of switching, and to update patients'
#' endpoint data.
#'
#' @param what a function determining whether a patient's data would be
#' updated due to switching treatment. It takes \code{one_patient_data},
#' a data frame of one row as input, and returns a character of new treatment.
#' Return \code{NA} if no treatment switching could be done for this patient.
#' Note that the returned value will be passed into function `how()`, which
#' is also provide by users. Thus, it is up to users to decide what to return
#' from this function. No default value.
#' @param when a function determining the time at which a patient switches
#' to another treatment regimen, measured from the time of enrollment.
#' It takes \code{one_patient_data}, a data frame of one row as
#' input, and returns a numeric switching time (from \code{enroll_time}).
#' Note that the returned object will be passed into function `how()`, which
#' is also provided by users. No default value.
#' @param how a function updating a patient's data after treatment switching.
#' @param ... optional arguments for the three functions. No default value.
#'
#' @export
#'
regime <- function(what, when, how, ...){

  Regimes$new(
    what = what,
    when = when,
    how = how,
    ...
  )

}
