#' Triggering condition by number of events
#'
#' @description
#' Define a condition to trigger trial event by the number of events of an
#' endpoint. The event will be trigger when a trial has observed
#' at least the specified number of endpoint events. It can be used combined with
#' conditions specified by \link[TrialSimulator]{calendarTime} and
#' \link[TrialSimulator]{enrollment}.
#'
#' @param endpoint character. Name of an endpoint.
#' @param n integer. Targeted number of events.
#' @param arms vector of character. Name of arms on which the number of events
#' is counted. If \code{NULL}, use all arms that are not yet removed from the
#' trial by the time of calculation.
#'
#' @returns an object of class `Condition`
#'
#' @export
eventNumber <- function(endpoint, n, arms = NULL){

  EventCountCondition$new(endpoint, n, arms)

}
