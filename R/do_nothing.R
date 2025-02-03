#' An action function that does nothing
#' @description
#' This is an action function that does nothing when the corresponding event
#' is triggered. When the listener is monitoring a trial and determining the
#' time to trigger an event, data is automatically locked with other necessary
#' data manipulation being executed. If the users have no intent to modify the
#' trial adaptively at the event, e.g., adding (\code{Trial$add_arms()}) or
#' removing (\code{Trial$remove_arms()}) arm(s),
#' changing sampling ratio(s) (\code{Trial$update_sample_ratio()}),
#' modifying trial duration, carrying out statistical testing,
#' or saving intermediate results (\code{Trial$save()}), then this function
#' can be used to set the argument \code{action} when creating a new \code{Event}.
#'
#' @param trial a \code{Trial} object.
#' @param event_name character. Name of event being triggered.
#'
#' @returns
#' This function returns \code{NULL}. Actually, nothing is done in this function.
#'
#' @export
#'
do_nothing <- function(trial, event_name){

  # locked_data <- trial$get_locked_data(event_name)
  invisible(NULL)

}
