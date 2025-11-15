
#' Updating Sampling Ratios of Existing Arms in a Trial
#'
#' @description
#'
#' update sample ratios of arms. This could be called after an arm is added or
#' removed. Sample ratios can be updated for any existing arms.
#'
#' This is a user-friendly wrapper of the member function of trial, i.e.,
#' \code{Trials$update_sample_ratio()}, which is used in vignettes.
#' Users who are not familiar with the concept of classes may consider using
#' this wrapper directly.
#'
#' @param trial a trial object returned by \code{trial()}.
#' @param arm_names character vector. Name of arms.
#' @param sample_ratios numeric vector. New sample ratios of arms. If sample
#' ratio is a whole number, the permuted block randomization is adopted;
#' otherwise, \code{sample()} will be used instead, which can cause imbalance
#' between arms by chance. However, this is usually fine for simulation.
#'
#' @export
#'
update_sample_ratio <- function(trial, arm_names, sample_ratios){

  trial$update_sample_ratio(arm_names = arm_names, sample_ratios = sample_ratios)

}
