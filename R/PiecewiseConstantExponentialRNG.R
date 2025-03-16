#' Generate time-to-event endpoint from piecewise constant exponential distribution
#' @description
#' Implementation is based on \href{https://www.demogr.mpg.de/papers/technicalreports/tr-2010-003.pdf}{this algorithm}.
#' @param n number of random numbers
#' @param risk a data frame of columns
#' \describe{
#' \item{\code{end_time}}{ End time for a constant risk in a time window. The start time of the first time window is 0.}
#' \item{\code{piecewise_risk}}{ A constant risk in a time window, which is absolute risk * relative risk, or (h0 * g) in the link.}
#' \item{\code{hazard_ratio}}{ An optional column for simulating an active arm. If absent, a column of 1s will be added. Equivalently, user can multiply piecewise_risk by hazard_ratio manually and ignore this column.}
#' }
#' @param endpoint_name name of endpoint
#'
#' @import rlang
#' @examples
#' # example code
#' # In this example, absolute risk in [0, 1) and [26, 52] are 0.0181 and
#' # 0.0027, respectively.
#' risk <- data.frame(
#'   end_time = c(1, 4.33, 26.0, 52.0),
#'   piecewise_risk = c(1, 1.01, 0.381, 0.150) * exp(-4.01)
#' )
#' PiecewiseConstantExponentialRNG(10, risk, 'PFS')
#'
#' @export
PiecewiseConstantExponentialRNG <- function(n, risk, endpoint_name){

  stopifnot(is.data.frame(risk))

  required_columns <- c('end_time', 'piecewise_risk')
  missed_columns <- setdiff(required_columns, names(risk))
  if(length(missed_columns) > 0){
    stop('Missed column(s) in risk table: <', paste0(missed_columns, collapse = ', '), '>. ')
  }

  if('hazard_ratio' %in% names(risk)){
    risk <- risk %>%
      mutate(piecewise_risk = .data$piecewise_risk * .data$hazard_ratio) %>%
      dplyr::select(-.data$hazard_ratio)
  }

  n_windows <- nrow(risk)
  tau <- c(0, risk$end_time)
  delta_tau <- tau[-1] - tau[-length(tau)]
  h0_g <- risk$piecewise_risk

  m <- matrix(0, nrow = n_windows + 1, ncol = n_windows)
  m[lower.tri(m)] <- 1

  logS <- log(1 - runif(n))

  tmp <- -as.vector(m %*% (h0_g * delta_tau))

  tte <- rep(NA, n)
  for(i in 1:n_windows){
    tte <- ifelse(
      tmp[i] >= logS & logS > tmp[i + 1],
      tau[i] + (tmp[i] - logS) / h0_g[i],
      tte
    )
  }

  data.frame(tte = tte) %>%
    mutate(event = ifelse(is.na(tte), 0, 1)) %>%
    mutate(tte = ifelse(is.na(tte), tau[length(tau)], tte)) %>%
    rename(!!paste0(endpoint_name, '_event') := .data$event) %>%
    rename(!!endpoint_name := tte)
}



