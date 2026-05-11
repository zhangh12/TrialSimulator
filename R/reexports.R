#' Objects re-exported from the survival package
#'
#' These are re-exported so that user-supplied formulas in action functions
#' (e.g. \code{Surv(time, event) ~ arm} or \code{... + strata(site)}) resolve
#' correctly after \code{library(TrialSimulator)} alone, without requiring
#' \code{library(survival)} or the \code{survival::} prefix. This is
#' particularly relevant for parallel runs (\code{n_workers > 1}), where each
#' worker only attaches TrialSimulator.
#'
#' @importFrom survival Surv strata
#' @name reexports
#' @aliases Surv strata
#' @keywords internal
#' @export Surv
#' @export strata
NULL
