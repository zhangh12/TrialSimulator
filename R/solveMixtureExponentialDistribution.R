#' Solve parameters in a mixture exponential distribution
#'
#' @description
#' Assume that the overall population is a mixture of two exponential
#' distributions with medians \code{median1} (\eqn{m_1}) and
#' \code{median2} (\eqn{m_2}). Given the proportion of the first component
#' (\eqn{p_1}) and the overall median \eqn{m}, we have
#'
#' \deqn{p_1 (1 - e^{-\log(2)m/m_1}) + (1 - p_1) (1 - e^{-\log(2)m/m_2}) = 1/2}
#'
#' This function computes \eqn{m_2} or \eqn{m} given \eqn{p_1} and \eqn{m_1}.
#'
#' @param weight1 numeric. The proportion of the first component.
#' @param median1 numeric. Median of the first component.
#' @param median2 numeric. Median of the second component. If \code{NULL},
#' then \code{overall_median} must be specified, and this function will
#' calculate and return \code{median2}.
#' @param overall_median numeric. Median of the overall population. If
#' \code{NULL}, then \code{median2} must be specified, and this function
#' will calculate and return \code{overall_median}.
#'
#' @returns a named vector of \code{median2} or \code{overall_median}.
#'
#' @importFrom stats uniroot
#'
#' @examples
#'
#' library(dplyr)
#'
#' median2 <-
#'   solveMixtureExponentialDistribution(
#'     weight1 = .3,
#'     median1 = 10,
#'     overall_median = 8)
#'
#' median2
#'
#' n <- 1e6
#' ifelse(
#'   runif(n) < .3,
#'   rexp(n, rate=log(2)/10),
#'   rexp(n, rate=log(2)/median2)) %>%
#'   median() ## should be close to 8
#'
#' overall_median <-
#'   solveMixtureExponentialDistribution(
#'     weight1 = .4,
#'     median1 = 12,
#'     median2 = 4)
#'
#' overall_median
#'
#' ifelse(
#'   runif(n) < .4,
#'   rexp(n, rate=log(2)/12),
#'   rexp(n, rate=log(2)/4)) %>%
#'   median() ## should be close to overall_median
#'
#' @export
#'
solveMixtureExponentialDistribution <- function(weight1, median1, median2 = NULL, overall_median = NULL){

  if(is.null(median2) && is.null(overall_median)){
    stop('Only one value of median2 or overall_median needs to be specified. ',
         'The other value is returned after calculation in function. ')
  }

  if(!is.null(median2) && !is.null(overall_median)){
    stop('Only one value of median2 or overall_median needs to be specified. ',
         'The other value is returned after calculation in function. ')
  }

  if(is.null(median2)){
    ret <- -log(2) * overall_median /
      log(1 - (0.5 - weight1 * (1 - exp(-log(2)/median1 * overall_median))) / (1 - weight1))

    names(ret) <- 'median2'

  }else{
    object_function <- function(overall_median, weight1, median1, median2){
      weight1 * (1 - exp(-log(2) / median1 * overall_median)) +
        (1 - weight1) * (1 - exp(-log(2) / median2 * overall_median)) - 0.5
    }

    ret <- uniroot(interval = c(0, max(median1, median2)),
                   f = object_function,
                   tol = 1e-5,
                   weight1 = weight1,
                   median1 = median1,
                   median2 = median2)$root

    names(ret) <- 'overall_median'
  }

  ret

}

