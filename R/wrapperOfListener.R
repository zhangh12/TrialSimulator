#' Define a Listener
#'
#' @description
#' Define a listener. This is a user-friendly wrapper for
#' the class constructor \code{Listener$new}. Users who are not familiar with
#' the concept of classes may consider using this wrapper directly.
#'
#' @examples
#'
#' listener <- listener()
#'
#' @export
#'
listener <- function(){
  Listener$new()
}
