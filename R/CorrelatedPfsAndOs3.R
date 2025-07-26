

#' Generate PFS and OS using the three-states model
#'
#' @param n integer. Number of observations.
#' @param h01 constant transition hazard from state "stable" to state "progression".
#' @param h02 constant transition hazard from state "stable" to state "death".
#' @param h12 constant transition hazard from state "progression" to state "death".
#' @param pfs_name column name of PFS in returned data frame. It must be
#' consistent with `name` in the function `endpoint()`.
#' @param os_name column name of OS in returned data frame. It must be
#' consistent with `name` in the function `endpoint()`.
#'
#' @returns
#' A data frame of four columns, including PFS, OS and their event indicators.
#' The event indicators are all 1s.
#' @export
#'
#' @importFrom stats model.frame model.response rexp
#'
#' @examples
#'
#' pfs_and_os_trt <- CorrelatedPfsAndOs3(1e4, 0.06, 0.30, 0.30, 'PFS', 'OS')
#' pfs_and_os_pbo <- CorrelatedPfsAndOs3(1e4, 0.10, 0.40, 0.30, 'PFS', 'OS')
#'
CorrelatedPfsAndOs3 <- function(n, h01, h02, h12,
                                pfs_name = 'pfs',
                                os_name = 'os'){

  ## assuming constant transition hazards

  ## stable time follows exponential(h01 + h02)
  stable_time <- rexp(n, rate = h01 + h02)

  ## given PFS, time from progression to death follows exponential(h12)
  t <- rexp(n, rate = h12)

  state_at_t <- sample(c('progression', 'death'), size = n,
                       replace = TRUE, prob = c(h01, h02))

  pfs <- stable_time

  os <- ifelse(state_at_t == 'death', stable_time, stable_time + t)

  data.frame(pfs, os) %>%
    mutate(pfs_event = 1) %>%
    mutate(os_event = 1) %>%
    rename(!!paste0(pfs_name, '_event') := .data$pfs_event) %>%
    rename(!!pfs_name := .data$pfs) %>%
    rename(!!paste0(os_name, '_event') := .data$os_event) %>%
    rename(!!os_name := .data$os)

}
