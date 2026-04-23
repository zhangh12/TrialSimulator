
#' Generate Correlated PFS and OS Using Gumbel Copula
#'
#' @description
#'
#' Generate correlated PFS and OS endpoints using the Gumbel copula. Marginally,
#' both PFS and OS follow exponential distributions. This function can be used
#' as custom \code{generator} in the function \code{endpoint()}.
#'
#' Note that the Gumbel copula is applied to the survival functions of OS and
#' time-to-progression (TTP). PFS is defined as min(TTP, OS), which also
#' follows an exponential distribution.
#'
#' For more information, refer to
#' \href{https://zhangh12.github.io/TrialSimulator/articles/simulatePfsAndOsGumbel.html}{this vignette}.
#'
#' @param n integer. Number of observations.
#' @param median_pfs numeric. Median of PFS.
#' @param median_os numeric. Median of OS.
#' @param kendall numeric. Kendall's tau between observed, uncensored PFS and OS.
#' Must be positive and usually away from zero. Note that
#' this argument is not the Kendall's tau between TTP and OS.
#' @param pfs_name column name of PFS in returned data frame. It must be
#' consistent with name in the function \code{endpoint()}.
#' @param os_name column name of OS in returned data frame. It must be
#' consistent with name in the function \code{endpoint()}.
#'
#' @returns
#' A data frame of \code{n} rows and four columns, including PFS, OS and their
#' event indicators. The event indicators are all 1s. The column names are
#' \code{<pfs_name>}, \code{<pfs_name>_event}, \code{<os_name>},
#' and \code{<os_name>_event}.
#'
#' @export
#'
#' @examples
#' pfs_and_os <- endpoint(name = c('PFS', 'Os'),
#'                        type = c('tte', 'tte'),
#'                        generator = CorrelatedPfsAndOs2,
#'                        median_pfs = 5,
#'                        median_os = 11,
#'                        kendall = .6,
#'                        pfs_name = 'PFS',
#'                        os_name = 'Os')
#'
#' pfs_and_os # run it in console to see summary report
#'
#' ## for validation purpose only
#' ## not the recommended way to use TrialSimulator
#' dat <- pfs_and_os$test_generator(n = 1e4)
#' cor(dat[, 1:2], method = 'kendall') ## close to 0.6
#'
CorrelatedPfsAndOs2 <- function(n, median_pfs, median_os, kendall, pfs_name = 'pfs', os_name = 'os'){

  if(kendall < 0){
    stop("Kendall's tau cannot be negative in CorrelatedPfsAndOs2(). ")
  }

  if(median_os <= 0){
    stop('Median of OS must be positive. ')
  }

  if(median_pfs <= 0){
    stop('Median of PFS must be positive. ')
  }

  f <- function(tau, kendall, median_pfs, median_os){
    kendall - 1 + (1 - tau) * (1 - (median_pfs / median_os)^(1/(1-tau)))
  }

  fit <- try(uniroot(f, c(0, 1),
                     kendall = kendall,
                     median_pfs = median_pfs,
                     median_os = median_os),
             silent = TRUE)

  if('try-error' %in% class(fit)){
    stop("Kendall's tau (", kendall, ') between OS and PFS is too small given the two medians ',
         median_pfs, ' and ', median_os, '. ')
  }

  ## Kendall's tau between TTP and OS
  tau <- fit$root
  theta <- 1 / (1 - tau)

  dsgn <- copula::gumbelCopula(param = theta, dim = 2)
  u <- copula::rCopula(n, dsgn)

  rate_pfs <- log(2) / median_pfs
  rate_os <- log(2) / median_os

  rate_ttp <- (rate_pfs^theta - rate_os^theta)^(1/theta)

  ttp <- -log(u[, 1]) / rate_ttp
  os <- -log(u[, 2]) / rate_os

  pfs <- ifelse(ttp > os, os, ttp)

  ## Kendall's tau between OS and PFS (i.e. min(TTP, OS))
  tau_ <- 1 - 1/theta * (1 - (median_pfs / median_os)^theta)

  data.frame(pfs, os) %>%
    mutate(pfs_event = 1) %>%
    mutate(os_event = 1) %>%
    rename(!!paste0(pfs_name, '_event') := .data$pfs_event) %>%
    rename(!!pfs_name := .data$pfs) %>%
    rename(!!paste0(os_name, '_event') := .data$os_event) %>%
    rename(!!os_name := .data$os)

}

