#' Deep-copy objects with reference semantics
#'
#' @description
#' Internal helper for the global data of a trial (the \code{global_data}
#' argument of \code{trial()} and \code{trial$get_global_data()}). A value
#' stored there must stay immutable, but R's copy-on-modify does not
#' protect objects with reference semantics: an R6 object handed out by
#' reference is mutated in place by its own methods, and a
#' \code{data.table} is modified in place by \code{:=} and the
#' \code{set*()} family. Copy those on both the way in (the trial owns an
#' independent template) and the way out (callers work on their own copy);
#' everything else is returned as is, protected by copy-on-modify.
#'
#' Plain (unclassed) lists are walked recursively, so reference objects
#' nested in, e.g., \code{list(gt = <R6>, alpha = 0.025)} are protected as
#' well; classed list-alikes such as data frames are left to
#' copy-on-modify. An R6 class holding further reference objects inside
#' list fields needs to implement a \code{deep_clone} method for the deep
#' clone to reach them (see the \code{Arms} class for an example). A
#' \code{data.table} whose namespace is not available cannot be modified
#' in place either, so it is returned as is in that (theoretical) case.
#'
#' @param value object to be copied.
#'
#' @return an independent copy of \code{value} when it has reference
#' semantics, otherwise \code{value} itself.
#'
#' @keywords internal
#' @noRd
copy_reference_object <- function(value){

  if(inherits(value, 'R6')){
    return(value$clone(deep = TRUE))
  }

  if(inherits(value, 'data.table') &&
     requireNamespace('data.table', quietly = TRUE)){
    return(data.table::copy(value))
  }

  if(is.list(value) && !is.object(value)){
    ## index assignment keeps every attribute of the list (names included)
    value[seq_along(value)] <- lapply(value, copy_reference_object)
    return(value)
  }

  value

}

#' Detect plain environments that copy_reference_object() cannot protect
#'
#' @description
#' Internal helper for validating the \code{global_data} argument of
#' \code{trial()}: \code{TRUE} when \code{value} is a plain (non-R6)
#' environment, or a plain list holding one at any depth, i.e., when the
#' value cannot be protected from modification by reference and must be
#' rejected.
#'
#' @param value object to be checked.
#'
#' @return logical.
#'
#' @keywords internal
#' @noRd
holds_plain_environment <- function(value){

  if(inherits(value, 'R6')){
    return(FALSE)
  }

  if(is.environment(value)){
    return(TRUE)
  }

  if(is.list(value) && !is.object(value)){
    return(any(vapply(value, holds_plain_environment, logical(1))))
  }

  FALSE

}
