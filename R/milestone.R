#' Define a Milestone of a Trial
#'
#' @description
#' Define a milestone of a trial. This is a user-friendly wrapper for
#' the class constructor \code{Milestones$new}. Users who are not familiar with
#' the concept of classes may consider using this wrapper directly.
#'
#' @param name character. Name of milestone.
#' @param when condition to check if this milestone should be
#' triggered.
#' @param action function to execute when the milestone triggers.
#' If no action to be executed but simply need to record triggering time of
#' a milestone, \code{action} can be its default value, a built-in function
#' \code{doNothing}.
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
