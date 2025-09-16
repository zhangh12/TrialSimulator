#' Define a Listener
#'
#' @description
#' Define a listener. This is a user-friendly wrapper for
#' the class constructor \code{Listener$new()}. Users who are not familiar with
#' the concept of classes may consider using this wrapper directly.
#'
#' Listener is an important concept of \code{TrialSimulator}. Used with a
#' trial object in a controller, a listener can monitor a running trial to
#' execute user-defined actions when it determine condition of triggering a
#' milestone is met. This mechanism allows the package users to focus on
#' the development of action functions in a simulation.
#'
#' @param silent logical. \code{TRUE} to mute messages.
#'
#' @examples
#'
#' listener <- listener()
#'
#' @export
#'
listener <- function(silent = FALSE){
  Listeners$new(silent = silent)
}
