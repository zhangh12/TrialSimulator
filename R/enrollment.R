#' Triggering condition by number of randomized patients
#'
#' @description
#' Define a condition to trigger trial event by the number of randomized
#' patients. The event will be trigger when a trial has enrolled
#' at least the specified number of patients. It can be used combined with
#' conditions specified by \link[TrialSimulator]{calendarTime} and
#' \link[TrialSimulator]{eventNumber}.
#'
#' @param n integer. Number of randomized patients.
#' @param arms vector of character. Name of arms on which the number of patients
#' is counted. If \code{NULL}, use all arms that are not yet removed from the
#' trial by the time of calculation.
#'
#' @returns an object of class `Condition`
#'
#' @export
enrollment <- function(n, arms = NULL){

  EnrollmentCountCondition$new(n, arms)

}
