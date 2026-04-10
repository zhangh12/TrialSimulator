#' Define a Regimen
#'
#' @description
#' Define a regimen of a trial. This is a user-friendly wrapper for
#' the class constructor \code{Regimens$new()}. Users who are not familiar with
#' the concept of classes may consider using this wrapper directly.
#'
#' A regimen defines the rules to select patients who
#' switch treatments, to determine the time of switching, and to update patients'
#' endpoint data.
#'
#' @param what a function determining whether patients' data would be
#' updated due to switching treatment. It takes \code{patient_data},
#' a data frame as argument, and returns a data frame of two columns
#' \code{patient_id} and \code{new_treatment}. Patients with \code{NA} in
#' \code{new_treatment} will be skipped.
#' The number of rows in the returned data frame may be smaller than the
#' number of patients in the input data frame. This indicates that some
#' patients' data will not be modifier.
#' Note that the returned object will be passed into function `how()`, which
#' is also provide by users. This argument can also be a
#' list of functions that will be executed sequentially. No default value.
#' @param when a function determining the time at which a patient switches
#' to another treatment regimen, measured from the time of enrollment.
#' It takes \code{patient_data}, a data frame as
#' argument, and returns a data frame of two columns \code{patient_id} and
#' \code{switch_time} (from \code{enroll_time}). No \code{NA} is allowed
#' in \code{switch_time} and the number of rows in the returned data frame
#' must equal the number of rows in \code{patient_data}, i.e., switching
#' time must be specified to every patients.
#' Note that the returned object will be passed into function `how()`, which
#' is also provided by users. This argument can also be a
#' list of functions that will be executed sequentially. No default value.
#' @param how a function updating patients' data after treatment switching.
#' Only modified columns and \code{patient_id} are returned. A cell will
#' be omitted if \code{NA}, meaning no change to that patient for the endpoint
#' or other variables. Equivalently, users can also fill the cell with
#' its original value. This argument can also be a list of functions that
#' will be executed sequentially. No default value.
#' @param ... (optional) named arguments to be passed to one or more of
#' \code{what}, \code{when}, and \code{how}. Each argument is routed to every
#' function whose formal parameter list contains that name. All arguments must
#' be named, and every name must match at least one parameter of at least one
#' function in \code{what}, \code{when}, or \code{how}.
#'
#' @export
#'
regimen <- function(what, when, how, ...){

  Regimens$new(
    what = what,
    when = when,
    how = how,
    ...
  )

}
