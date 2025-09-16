
#' Default Action Function for Dry Run of a Trial
#
#' No meaningful action is taken when the argument \code{action} of
#' \code{milestone()} is set to be \code{default_action}. When milestones
#' are triggered, milestone time and number of observations/events are recorded.
#' but no custom information is saved.
#'
#' @keywords internal
#' @noRd
.default_action <- function(){

  action <- 'This is a dry run to determine milestone time and number of endpoint events. '
  action

}
