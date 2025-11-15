#' Resizing a Trial
#'
#' @description
#'
#' resize a trial with a greater sample size. This function is used to update
#' the maximum sample size adaptively after sample size reassessment. Note that
#' this function should be called within action functions. It is users'
#' responsibility to ensure it and \code{TrialSimulator} has no way to track this.
#'
#' This is a user-friendly wrapper of the member function of trial, i.e.,
#' \code{Trials$resize()}, which is used in vignettes. Users who are not
#' familiar with the concept of classes may consider using this wrapper
#' directly.
#'
#'
#' @param trial a trial object returned by \code{trial()}.
#' @param n_patients integer. Number of maximum sample size of a trial.
#'
#' @export
#'
resize <- function(trial, n_patients){

  trial$resize(n_patients = n_patients)

}

