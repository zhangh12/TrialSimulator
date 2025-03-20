#' Triggering condition by calendar time
#'
#' @description
#' Define a condition to trigger trial event by calendar time. The event will
#' be trigger when a trial has been running for at least the specified
#' duration. It can be used combined with conditions specified by
#' \link[TrialSimulator]{enrollment} and \link[TrialSimulator]{eventNumber}.
#'
#' @param time numeric. Calendar time to trigger an event.
#'
#' @returns an object of class `Condition`
#'
#' @export
calendarTime <- function(time){

  CalendarTimeCondition$new(time)

}
