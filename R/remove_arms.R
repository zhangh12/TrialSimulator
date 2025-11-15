#' Removing One or More Arms From a Trial
#'
#' @description
#'
#' remove arms from a trial. The application of this function includes, but is
#' not limited to, dose selection, enrichment analysis
#' (select sub-population).
#'
#' Note that this function should only be called within action functions of
#' milestones. It is users' responsibility to ensure that and
#' \code{TrialSimulator} has no way to track it. In addition, data of the
#' removed arms are censored or truncated by the time of arm removal.
#'
#' This is a user-friendly wrapper of the member function of trial, i.e.,
#' \code{Trials$remove_arms()}, which is used in vignettes. Users who are not
#' familiar with the concept of classes may consider using this wrapper
#' directly.
#'
#' @param trial a trial object returned by \code{trial()}.
#' @param arms_name character vector. Name of arms to be removed.
#'
#' @export
#'
remove_arms <- function(trial, arms_name){

  trial$remove_arms(arms_name = arms_name)

}

