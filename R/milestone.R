#' Define a Milestone of a Trial
#'
#' @description
#' Define a milestone of a trial. This is a user-friendly wrapper for
#' the class constructor \code{Milestones$new}. Users who are not familiar with
#' the concept of classes may consider using this wrapper directly.
#'
#' @param name character. Name of milestone.
#' @param type character vector. Milestone type(s) (futility, interim, final, etc.),
#' an milestone can be of multiple types. This is for information purpose so any
#' string is valid.
#' @param trigger_condition condition to check if this milestone should be
#' triggered.
#' @param action function to execute when the milestone triggers.
#' @param ... arguments for \code{trigger_condition}.
#'
#' @export
#'
milestone = function(name, type = name, trigger_condition, action = doNothing, ...){

  Milestones$new(
    name = name,
    type = type,
    trigger_condition = trigger_condition,
    action = action,
    ...
  )

}
