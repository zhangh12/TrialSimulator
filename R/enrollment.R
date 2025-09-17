#' Triggering Condition by Number of Randomized Patients
#'
#' @description
#' Define a condition to trigger trial milestone by the number of randomized
#' patients. The milestone will be triggered when a trial has enrolled
#' at least the specified number of patients. It can be used combined with
#' conditions specified by \link[TrialSimulator]{calendarTime} and
#' \link[TrialSimulator]{eventNumber}.
#'
#' Refer to the
#' \href{https://zhangh12.github.io/TrialSimulator/articles/conditionSystem.html}{vignette}
#' to learn how to define milestones when performing simulation using
#' \code{TrialSimulator}.
#'
#' @param n integer. Number of randomized patients.
#' @param ... subset conditions compatible with \code{dplyr::filter}. Number
#' of randomized patients will be counted on subset of trial data only.
#' @param arms vector of character. Name of arms on which the number of patients
#' is counted. If \code{NULL}, use all arms that are not yet removed from the
#' trial by the time of calculation.
#' @param min_treatment_duration numeric. Zero or positive value.
#' minimum treatment duration of enrolled patients.
#' Default is 0, i.e., looking for triggering time based on number of enrolled
#' patients in population specified by \code{...} and \code{arms}. If positive,
#' it means that milestone is triggered when a specific number of enrolled
#' patients have received treatment for at least \code{min_treatment_duration}
#' duration. It is users' responsibility to assure that the unit of
#' \code{min_treatment_duration} are consistent with
#' readout of non-tte endpoints, dropout time, and trial duration.
#'
#' @returns an object of class `Condition`
#'
#' @examples
#'
#' ## ensure sufficient sample size of whole trial
#' enrollment(n = 100)
#'
#' ## ensure sufficient sample size in sub-group of interest
#' enrollment(n = 100, biomarker1 == 'positive' & biomarker2 == 'high')
#'
#' ## ensure sufficient sample size in high dose + placebo
#' enrollment(n = 1000, arms = c('high dose', 'placebo'))
#'
#' ## ensure sufficient treatment duration
#' enrollment(n = 500, min_treatment_duration = 2)
#'
#'
#' @export
enrollment <- function(n, ..., arms = NULL, min_treatment_duration = 0){

  EnrollmentCountCondition$new(n, ...,
                               arms = arms,
                               min_treatment_duration = min_treatment_duration)

}
