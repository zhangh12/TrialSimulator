#' Generate constant variable
#' @description
#' A random number generator returning only a constant. This can be used to
#' set dropout time. Currently it is default value of dropout time, with
#' `value = Inf`.
#' @param n integer. Number of observations.
#' @param value value of constant observations.
#'
#' @export
rconst <- function(n, value){
  rep(value, n)
}
