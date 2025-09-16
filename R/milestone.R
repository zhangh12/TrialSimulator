#' Define a Milestone
#'
#' @description
#' Define a milestone of a trial. This is a user-friendly wrapper for
#' the class constructor \code{Milestones$new()}. Users who are not familiar with
#' the concept of classes may consider using this wrapper directly.
#'
#' A milestone means the time point to take an action,
#' e.g., carrying out (futility, interim, final) analysis for
#' adding/removing arms, or stopping a trial early.
#' It can also be any more general time point where trial
#' data is used in decision making or adaptation. For example, one can define a
#' milestone for changing randomization scheme, sample size re-assessment, trial
#' duration extension etc.
#'
#' Refer to the
#' \href{https://zhangh12.github.io/TrialSimulator/articles/conditionSystem.html}{vignette}
#' to learn how to define milestones when performing simulation using
#' \code{TrialSimulator}.
#'
#' @param name character. Name of milestone.
#' @param when condition to check if this milestone should be
#' triggered. It taks value returned from functions \code{calendarTime()},
#' \code{enrollment()}, \code{eventNumber()} or their logic combinations.
#' @param action function to execute when the milestone triggers.
#' If no action to be executed but simply need to record triggering time and
#' number of events/non-missing observations of endpoints at
#' a milestone, \code{action} can be its default value, a built-in function
#' \code{doNothing}.
#'
#' @examples
#' ## See vignette('conditionSystem')
#'
#'
#' @export
#'
milestone = function(name, when, action = doNothing){

  Milestones$new(
    name = name,
    type = name,
    trigger_condition = when,
    action = action
  )

}
