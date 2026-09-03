#' Reduce subset conditions to a logical row mask
#'
#' @description
#' Internal helper. Evaluate subset conditions captured as quosures (from
#' \code{...} of \code{arm()}, \code{eventNumber()}, \code{enrollment()},
#' etc.) on a data frame and return a logical row mask with the semantics
#' of \code{dplyr::filter()}: conditions are combined with \code{&}, and a
#' row is dropped when any condition evaluates to \code{NA}. The
#' \code{.data} and \code{.env} pronouns are supported. Subsetting with the
#' mask avoids the fixed per-call overhead of \code{dplyr::filter()} on the
#' hot paths (patient generation, lock-time search).
#'
#' @param data a data frame.
#' @param conditions a list of quosures.
#'
#' @return a logical vector of length \code{nrow(data)}.
#'
#' @keywords internal
#' @noRd
filter_conditions_mask <- function(data, conditions){
  mask <- rep(TRUE, nrow(data))
  for(q in conditions){
    v <- rlang::eval_tidy(q, data = data)
    if(!is.logical(v) || !(length(v) %in% c(1L, nrow(data)))){
      stop('condition <', rlang::as_label(q),
           '> does not evaluate to a logical vector of length 1 or nrow(data)')
    }
    mask <- mask & !is.na(v) & v
  }
  mask
}
