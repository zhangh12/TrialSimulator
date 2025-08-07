


#' Solve parameters in a three-state ill-death model
#'
#' @description
#' The ill-death model consists of three states, \code{stable}, \code{progression},
#' and \code{death}. It can be used to model the progression-free survival (PFS)
#' and overall survival (OS) in clinical trial simulation. It models the
#' correlation PFS and OS without assumptions on latent status and copula.
#' Also, it does not assume PFS and OS satisfy the proportional hazard assumption
#' simultaneously. The three-state ill-death model ensure the nice property that
#' PFS <= OS with probability one. However, it requires three hazard parameters
#' under the homogeneous Markov assumption. In practice, hazard parameters are
#' hard to specify intuitively especially when no trial data is available at
#' the planning stage.
#'
#' This function reparametrizes the ill-death model in term of three parameters,
#' i.e. median of PFS, median of OS, and correlation between PFS and OS. The
#' output of this function, which consists of the three hazard parameters, can
#' be used to generate PFS and OS with desired property. It can be used with
#' the built-in data generator \code{CorrelatedPfsAndOs3()} when defining
#' endpoints in \code{TrialSimulator}.
#'
#'
#' @param median_pfs numeric. Median of PFS.
#' @param median_os numeric. Median of OS.
#' @param corr numeric vector. Pearson correlation coefficients between PFS and OS.
#' @param h12 numeric vector. A set of hazard from progression to
#' death that may induce the target correlation \code{corr} given \code{median_pfs}
#' and \code{median_os}. \code{solveThreeStateModel} will do a grid search to
#' find the best hazard parameters that matches to the medians of PFS and OS,
#' and their correlations.
#'
#' @returns a data frame with columns:
#' \describe{
#' \item{\code{corr}}{target Peason's correlation coefficients. }
#' \item{\code{h01}}{hazard from stable to progression. }
#' \item{\code{h02}}{hazard from stable to death.  }
#' \item{\code{h12}}{hazard from progression to death. }
#' \item{\code{error}}{absolute error between target correlation and correlation
#' derived from \code{h01}, \code{h02}, and \code{h12}. }
#' }
#'
#' @export
#'
#' @examples
#'
#' dat <- CorrelatedPfsAndOs3(1e6, h01 = .1, h02 = .05, h12 = .12)
#'
#' cor(dat$pfs, dat$os) ## 0.65
#'
#' median(dat$pfs) ## 4.62
#'
#' median(dat$os) ## 9.61
#'
#' ## find h01, h02, h12 that can match to median_pfs, median_os and corr
#' ## should be close to h01 = 0.10, h02 = 0.05, h12 = 0.12 when corr = 0.65
#' ret <- solveThreeStateModel(median_pfs = 4.6, median_os = 9.6,
#'                             corr = seq(.5, .7, length.out=5))
#' ret
#'
solveThreeStateModel <- function(median_pfs, median_os, corr,
                                 h12 = seq(.05, .2, length.out = 50)){


  stopifnot(is.numeric(median_pfs) && length(median_pfs) == 1)
  stopifnot(is.numeric(median_os) && length(median_os) == 1)
  stopifnot(is.numeric(corr))
  stopifnot(all(corr > 0) && all(corr < 1))
  stopifnot(all(h12 > 0))

  .compute_h01_given_h12 <- function(h12, median_pfs, median_os){

    all_cause_hazard <- log(2)/median_pfs
    (1/2 - exp(-all_cause_hazard * median_os)) * (h12 - all_cause_hazard) /
      (exp(-all_cause_hazard * median_os) - exp(-h12 * median_os))

  }

  h01 <- .compute_h01_given_h12(h12, median_pfs, median_os)
  h02 <- log(2)/median_pfs - h01
  corr_ <- NULL
  h12_ <- NULL
  h01_ <- NULL
  h02_ <- NULL
  for(idx in seq_along(h12)){
    if(h01[idx] < 0 || h02[idx] < 0){
      next
    }

    dat <- CorrelatedPfsAndOs3(1e5, h01 = h01[idx], h02 = h02[idx], h12 = h12[idx])
    corr_ <- c(corr_, cor(dat$pfs, dat$os))
    h12_ <- c(h12_, h12[idx])
    h01_ <- c(h01_, h01[idx])
    h02_ <- c(h02_, h02[idx])
  }

  .find_h12_given_corr <- function(res, corr){

    stopifnot(all(corr > 0) && all(corr < 1))

    h12 <- NULL
    h01 <- NULL
    h02 <- NULL
    error <- NULL
    for(corr_ in corr){
      idx <- which.min(abs(res$corr - corr_))[1]
      h12 <- c(h12, res$h12[idx])
      h01 <- c(h01, res$h01[idx])
      h02 <- c(h02, res$h02[idx])
      error <- c(error, abs(res$corr - corr_)[idx])
    }

    ret <- data.frame(corr, h01, h02, h12, error)

    attr(ret, 'data') <- res

    ret

  }

  ret <- .find_h12_given_corr(data.frame(h01 = h01_, h02 = h02_, h12 = h12_, corr = corr_), corr)

  class(ret) <- c('three_state_model', class(ret))
  ret

}

#' Plot result of three-state ill-death model
#'
#' @param x an object returned by \code{solveThreeStateModel()}.
#' @param ... currently not supported.
#'
#' @export
plot.three_state_model <- function(x, ...){

  data_points <- attr(x, 'data')
  data_lines <- x %>%
    mutate(label = sprintf("(%.2f, %.4f)", .data$corr, .data$h12))

  suppressWarnings(
  p <- ggplot(data_points,
              aes(x = .data$corr, y = .data$h12)) +
    geom_point() +

    geom_segment(data_lines,
                 mapping =
                   aes(x = .data$corr, xend = .data$corr,
                       y = 0, yend = .data$h12),
                 linetype = 'dashed',
                 color = 'blue', inherit.aes = FALSE) +

    geom_segment(data_lines,
                 mapping =
                   aes(x = 0, xend = .data$corr,
                       y = .data$h12, yend = .data$h12),
                 linetype = 'dashed',
                 color = 'blue', inherit.aes = FALSE) +

    geom_text(data_lines,
              mapping =
                aes(x = .data$corr, y = .data$h12, label = .data$label),
              vjust = 2, hjust = -.2, inherit.aes = FALSE) +

    xlim(min(data_points$corr)*.9, max(data_points$corr)*1.1) +
    xlab("Pearson Correlation") +
    ylab(expression(h[12]))
  )

  print.data.frame(x)

  print(p)

  invisible(NULL)
}
