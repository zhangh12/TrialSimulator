#' Generate Constant Variable
#' @description
#' A random number generator returning only a constant. This can be used to
#' set dropout time. Currently it is the default value of dropout time, with
#' \code{value = Inf}.
#'
#' This function can also be used as a generator of \code{endpoint()} if a
#' constant endpoint is needed.
#'
#' @param n integer. Number of observations.
#' @param value scalar. Value of constant observations.
#'
#' @return a numeric vector of length \code{n} with all entries equal to \code{value}.
#'
#' @export
rconst <- function(n, value){
  rep(value, n)
}
