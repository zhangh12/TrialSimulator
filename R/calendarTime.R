#' Triggering condition by calendar time
#'
#' @description
#' Define a condition to trigger trial event by calendar time.
#'
#' @param time numeric. Calendar time to trigger an event.
#'
#' @returns an object of class `Condition`
#'
#' @export
calendarTime <- function(time){

  CalendarTimeCondition$new(time)

}
