#' Triggering Condition by Calendar Time
#'
#' @description
#' Define a condition to trigger trial milestone by calendar time. The
#' milestone will be triggered when a trial has been running for at least the
#' specified duration since the first patient is enrolled.
#' It can be used combined with conditions specified by
#' \link[TrialSimulator]{enrollment} and \link[TrialSimulator]{eventNumber}.
#'
#' Refer to the
#' \href{https://zhangh12.github.io/TrialSimulator/articles/conditionSystem.html}{vignette}
#' to learn how to define milestones when performing simulation using
#' \code{TrialSimulator}.
#'
#' @param time numeric. Calendar time to trigger a milestone of a trial.
#'
#' @returns an object of class `Condition`
#'
#' @examples
#' milestone(name = 'end of trial', when = calendarTime(time = 12))
#'
#' @export
calendarTime <- function(time){

  CalendarTimeCondition$new(time)

}
