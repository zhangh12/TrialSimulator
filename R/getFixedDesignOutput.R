#' Get simulation output in the vignette fixedDesign.Rmd
#'
#' Internal function that retrieves precomputed simulation results.
#' Not meant for use by package users.
#'
#' @return A data frame containing simulation results of 1000 replicates.
#'
getFixedDesignOutput <- function(){
  ## this is saved by calling
  ## usethis::use_data(fixed_design_output, internal = TRUE, overwrite = TRUE)
  return(fixed_design_output)
}
