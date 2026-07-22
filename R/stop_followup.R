#' Stopping Follow-Up of Selected Patients in a Trial
#'
#' @description
#'
#' stop follow-up of a subset of enrolled patients at a specified time at or
#' after the current milestone. Data of affected patients are censored
#' (time-to-event endpoints) or set to missing (non-time-to-event endpoints
#' with readout after the stopping time), as if those patients were no longer
#' followed since then. This function can be used in adaptive designs; its
#' application includes, but is not limited to, treatment discontinuation,
#' early termination of follow-up for a sub-population, or enrichment design
#' where follow-up of a de-selected sub-population is stopped after an
#' interim analysis. It can also be called at a pre-specified milestone that
#' splits a trial into cohorts, e.g., a milestone marking the last patient
#' of the first cohort and the first patient of the second cohort. Such a
#' milestone is usually event driven, so its time is unknown until the trial
#' is simulated. Stopping follow-up of the earlier cohort at that milestone,
#' or after a pre-specified, fixed \code{additional_followup} beyond it,
#' makes statistics computed from the two cohorts independent, which
#' facilitates tests requiring independence, e.g., combination tests.
#'
#' Only patients who are enrolled by the time this function is called and
#' satisfy the subset conditions in \code{...} (if any) are affected.
#' Patients enrolled afterwards are followed as usual.
#'
#' Note that this function should only be called within action functions of
#' milestones. It is users' responsibility to ensure that and
#' \code{TrialSimulator} has no way to track it. Calling it before any
#' milestone has been triggered is an error.
#'
#' This is a user-friendly wrapper of the member function of trial, i.e.,
#' \code{Trials$stop_followup()}, which is used in vignettes. Users who are not
#' familiar with the concept of classes may consider using this wrapper
#' directly.
#'
#' @param trial a trial object returned by \code{trial()}.
#' @param ... subset conditions compatible with \code{dplyr::filter}.
#' Follow-up is stopped for selected patients only. If no condition is
#' provided, follow-up is stopped for all patients enrolled by the time
#' this function is called.
#' @param additional_followup numeric. Extra follow-up time granted to the
#' selected patients after the current milestone. If 0 (default), follow-up
#' stops at the milestone itself.
#'
#' @export
#'
stop_followup <- function(trial, ..., additional_followup = 0){

  trial$stop_followup(..., additional_followup = additional_followup)

}
