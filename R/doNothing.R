#' An action function that does nothing
#' @description
#' This is an action function that does nothing when the corresponding milestone
#' is triggered. When the listener is monitoring a trial and determining the
#' time to trigger a milestone, data is automatically locked with other necessary
#' data manipulation being executed. If the users have no intent to modify the
#' trial adaptively at the milestone, e.g., adding (\code{add_arms()}) or
#' removing (\code{remove_arms()}) arm(s),
#' changing sampling ratio(s) (\code{update_sample_ratio()}),
#' modifying trial duration, carrying out statistical testing,
#' or saving intermediate results (\code{save()}), then this function
#' can be used to set the argument \code{action} when creating a new milestone.
#' Note that the triggering time of a milestone with \code{action = doNothing}
#' is still recorded in output automatically.
#'
#' @param trial a \code{Trial} object.
#' @param milestone_name character. Name of milestone being triggered.
#'
#' @returns
#' This function returns \code{NULL}. Actually, nothing is done in this function.
#'
#' @export
#'
doNothing <- function(trial, milestone_name){

  # locked_data <- trial$get_locked_data(milestone_name)
  invisible(NULL)

}
