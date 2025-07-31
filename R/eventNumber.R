#' Triggering condition by number of events or observations of an endpoint
#'
#' @description
#' Define a condition to trigger trial milestone by the number of events of a
#' time-to-event endpoint or the number of non-missing observations of a
#' non-time-to-event endpoint. The milestone will be triggered when a trial has
#' observed at least the specified number of endpoint events (or non-missing
#' observations). It can be used combined with
#' conditions specified by \link[TrialSimulator]{calendarTime} and
#' \link[TrialSimulator]{enrollment}.
#'
#' Number of events for a time-to-event endpoint can vary at different
#' milestones as more patients are randomized into a trial, or more events
#' onset over time.
#'
#' Number of non-missing observations for a non-time-to-event endpoint can vary
#' at different milestones as more patients are randomized into a trial, or more
#' patients have been treated until their readout time (thus, \code{NA} turns
#' to a value).
#'
#' @param endpoint character. Name of an endpoint.
#' @param n integer. Targeted number of events.
#' @param arms vector of character. Name of arms on which the number of events
#' is counted. If \code{NULL}, use all arms that are not yet removed from the
#' trial by the time of calculation.
#' @param ... subset conditions compatible with \code{dplyr::filter}. Number
#' of events will be counted on subset of trial data only.
#'
#' @returns an object of class `Condition`
#'
#' @export
eventNumber <- function(endpoint, n, ..., arms = NULL){

  EventCountCondition$new(endpoint, n, ..., arms = arms)

}
