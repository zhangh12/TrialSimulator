#' An Action Function that Does Nothing
#' @description
#' This is an action function that does nothing when the corresponding milestone
#' is triggered. When the listener is monitoring a trial and determining the
#' time to trigger a milestone, data is automatically locked with other necessary
#' data manipulations (censoring, truncation, etc.) are executed.
#' If the users have no intent to modify the
#' trial adaptively at the milestone, e.g., adding (\code{add_arms()}) or
#' removing (\code{remove_arms()}) arm(s),
#' changing sampling ratio(s) (\code{update_sample_ratio()}),
#' modifying trial duration (\code{set_duration()}), carrying out statistical
#' testing, or saving intermediate results (\code{save()}, etc.),
#' then this function
#' can be used to set the argument \code{action} when creating a new milestone.
#' Note that the triggering time and number of observations/events of endpoints
#' at a milestone with \code{action = doNothing} is still recorded in output
#' automatically.
#'
#' @param trial an object returned from \code{trial()}.
#' @param ... (optional) arguments. This is for capturing redundant arguments
#' in \code{milestone()} only.
#'
#' @returns
#' This function returns \code{NULL}. Actually, nothing is done in this function.
#'
#' @export
#'
doNothing <- function(trial, ...){

  dots <- list(...)
  if(length(dots) > 0){
    stop('Argument(s) <',
         paste0(names(dots), collapse = ', '),
         '> should not be specified in milestone() ',
         'if no action function is specified. ')
  }

  invisible(NULL)

}
