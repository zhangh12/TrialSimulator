#' Get simulation output in the vignette adaptiveDesign.Rmd
#'
#' Internal function that retrieves precomputed simulation results.
#' Not meant for use by package users.
#'
#' @return A data frame containing simulation results of 1000 replicates.
#'
getAdaptiveDesignOutput <- function(){
  return(adaptive_design_output)
}
