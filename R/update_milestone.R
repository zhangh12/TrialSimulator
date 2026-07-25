#' Updating a Not-Yet-Triggered Milestone of a Trial
#'
#' @description
#'
#' update the trigger condition and/or the action of a not-yet-triggered
#' milestone. The milestone to be updated is identified by its name, which
#' cannot be changed. This function can be used in adaptive designs, e.g.,
#' when conditional power at an interim analysis is lower than expected, the
#' final analysis can be postponed by increasing the target number of events
#' in its triggering condition, or its triggering condition can be switched
#' from a calendar time to an event count entirely.
#'
#' The update is not applied immediately: it is queued and takes effect
#' right after the current action function returns, before the next
#' milestone is evaluated. The new trigger condition and action replace the
#' old ones as a whole. Between simulation replicates the milestone is
#' restored to its as-designed trigger condition and action, so every
#' replicate starts from the original design. A milestone that has already
#' been triggered cannot be updated.
#'
#' Note that this function should only be called within action functions of
#' milestones. It is users' responsibility to ensure that and
#' \code{TrialSimulator} has no way to track it. Calling it before any
#' milestone has been triggered is an error. Also note that milestones must
#' trigger in their registration order: an updated triggering condition that
#' makes a later-registered milestone fire before an earlier one stops the
#' simulation with an error.
#'
#' This is a user-friendly wrapper of the member function of trial, i.e.,
#' \code{Trials$update_milestone()}, which is used in vignettes. Users who
#' are not familiar with the concept of classes may consider using this
#' wrapper directly.
#'
#' @param trial a trial object returned by \code{trial()}.
#' @param name character. Name of the milestone to be updated. It must be
#' registered with the listener and not yet triggered.
#' @param when (optional) new triggering condition, an object returned by
#' \code{calendarTime()}, \code{enrollment()}, \code{eventNumber()} or their
#' combinations using \code{&} and \code{|}. If \code{NULL}, the triggering
#' condition is left unchanged.
#' @param action (optional) new action function. See \code{action} of
#' \code{milestone()}. If \code{NULL}, the action is left unchanged.
#' @param ... (optional) named arguments of the new \code{action}. Only
#' allowed when \code{action} is provided.
#' The new action is executed with exactly the arguments supplied
#' here: fixed arguments of the previous action are never carried over.
#'
#' @return no return value, called for its side effect of updating \code{trial}.
#'
#' @export
#'
update_milestone <- function(trial, name, when = NULL, action = NULL, ...){

  trial$update_milestone(name = name, when = when, action = action, ...)

}
