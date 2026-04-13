#' Expand regimen trajectory into long format
#'
#' @description
#' Expands the compact \code{regimen_trajectory} column in locked data
#' (returned by \code{trial$get_locked_data()}) into a long-format data frame
#' with one row per regimen segment per patient.
#'
#' The \code{regimen_trajectory} column stores each patient's treatment history
#' as a semicolon-separated string of \code{"name\@time"} entries, e.g.
#' \code{"placebo\@0;low dose\@8.5"}.  \code{expandRegimen} parses this into
#' two additional columns:
#' \itemize{
#'   \item \code{regimen} — name of the treatment regimen
#'   \item \code{switch_time_from_enrollment} — time from enrollment at which
#'         the patient switched to this regimen
#' }
#' The \code{regimen_trajectory} column is dropped from the result.
#'
#' @param data a data frame returned by \code{trial$get_locked_data()}.
#'
#' @return a long-format data frame: one row per regimen segment per patient,
#' with \code{regimen} and \code{switch_time_from_enrollment} appended and
#' \code{regimen_trajectory} removed.
#'
#' @examples
#' \dontrun{
#' locked <- trial$get_locked_data('final')
#' long   <- expandRegimen(locked)
#' }
#'
#' @export
expandRegimen <- function(data){

  required_cols <- c('patient_id', 'regimen_trajectory')
  missing_cols  <- setdiff(required_cols, names(data))
  if(length(missing_cols) > 0){
    stop('expandRegimen: column(s) not found in `data`: ',
         paste(missing_cols, collapse = ', '), '. ',
         '`data` does not look like it was returned by trial$get_locked_data(). ',
         'Also make sure the trial was run with a regimen registered via trial$add_regimen().',
         call. = FALSE)
  }

  if(inherits(data, 'data.table')) data <- as.data.frame(data)

  rows <- lapply(seq_len(nrow(data)), function(i){
    entries  <- strsplit(data$regimen_trajectory[i], ';', fixed = TRUE)[[1]]
    data.frame(
      patient_id                  = data$patient_id[i],
      regimen                     = sub('@.*',  '',  entries),
      switch_time_from_enrollment = as.numeric(sub('.*@', '', entries)),
      stringsAsFactors            = FALSE
    )
  })

  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  result

}
