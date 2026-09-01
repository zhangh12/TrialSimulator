#' Get simulation output in the vignette conditionalPower.Rmd
#'
#' Internal function that retrieves precomputed simulation results.
#' Not meant for use by package users.
#'
#' @return A data frame containing simulation results of 1000 replicates.
#'
getConditionalPowerOutput <- function(){
  ## This is saved by calling
  ## usethis::use_data(conditional_power_output, internal = TRUE,
  ##                   overwrite = TRUE)
  conditional_power_output
}
