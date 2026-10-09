#' Get simulation output in the vignette enrichmentDesign.Rmd
#'
#' Internal function that retrieves precomputed simulation results.
#' Not meant for use by package users.
#'
#' @return A data frame containing simulation results of 10000 replicates
#' under the alternative and 10000 replicates under the global null,
#' distinguished by the column \code{scenario}.
#'
getEnrichmentDesignOutput <- function(){
  ## This is saved, together with the other internal data sets, by calling
  ## usethis::use_data(..., enrichment_design_output, internal = TRUE,
  ##                   overwrite = TRUE)
  enrichment_design_output
}
