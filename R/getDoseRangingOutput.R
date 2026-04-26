#' Get simulation output in the vignette doseRanging.Rmd
#'
#' Internal function that retrieves precomputed simulation results.
#' Not meant for use by package users.
#'
#' @return A data frame containing simulation results of 10 replicates.
#'
getDoseRangingOutput <- function(){
  ## this is saved by calling
  ## usethis::use_data(dose_ranging_output, internal = TRUE, overwrite = TRUE)
  return(dose_ranging_output)
}
