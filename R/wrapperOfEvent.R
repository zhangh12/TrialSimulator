#' Define an Trial Event
#'
#' @description
#' Define an event of a trial. This is a user-friendly wrapper for
#' the class constructor \code{Event$new}. Users who are not familiar with
#' the concept of classes may consider using this wrapper directly.
#'
#' @param name character. Name of event.
#' @param type character vector. Event type(s) (futility, interim, final),
#' an event can be of multiple types.
#' @param trigger_condition function to check if this event should
#' trigger. Return TRUE/FALSE.
#' @param action function to execute when the event triggers.
#' @param ... arguments for \code{trigger_condition}.
#'
#' @export
#'
event = function(name, type = name, trigger_condition, action = doNothing, ...){

  Event$new(
    name = name,
    type = type,
    trigger_condition = trigger_condition,
    action = action,
    ...
  )

}
