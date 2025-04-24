#' Define a Controller
#'
#' @description
#' Define a controller of a trial. This is a user-friendly wrapper for
#' the class constructor \code{Controller$new}. Users who are not familiar with
#' the concept of classes may consider using this wrapper directly.
#'
#' @param trial a \code{Trial} object.
#' @param listener a \code{Listener} object.
#'
#' @examples
#'
#' ## controller <- controller(trial, listener)
#'
#' @export
#'
controller = function(trial, listener){

  Controllers$new(trial = trial, listener = listener)

}
