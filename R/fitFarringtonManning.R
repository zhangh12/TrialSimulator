#' @title Farrington-Manning test for rate difference
#'
#' @description
#' Test rate difference by comparing it to a pre-specified value using the
#' Farrington-Manning test.
#'
#' Refer to \href{this vignette}{https://zhangh12.github.io/TrialSimulator/articles/wrappers.html}
#' for more information and examples.
#'
#' @param endpoint Character. Name of the endpoint in \code{data}.
#' @param placebo Character. String indicating the placebo in \code{data$arm}.
#' @param data Data frame. Usually it is a locked data set.
#' @param alternative a character string specifying the alternative hypothesis,
#' must be one of \code{"greater"} or \code{"less"}, i.e.,
#' one-sided test is enforced. No default value.
#' \code{"greater"} means superiority of treatment over placebo is established
#' by rate difference greater than `delta`.
#' @param ... Subset conditions compatible with \code{dplyr::filter}.
#' \code{glm} will be fitted on this subset only. This argument can be useful
#' to create a subset of data for analysis when a trial consists of more
#' than two arms. By default, it is not specified,
#' all data will be used to fit the model. More than one condition can be
#' specified in \code{...}, e.g.,
#' \code{fitFarringtonManning('remission', 'pbo', data, delta, arm \%in\% c('pbo', 'low dose'), cfb > 0.5)},
#' which is equivalent to:
#' \code{fitFarringtonManning('remission', 'pbo', data, delta, arm \%in\% c('pbo', 'low dose') & cfb > 0.5)}.
#' Note that if more than one treatment arm are present in the data after
#' applying filter in \code{...}, models are fitted for placebo verse
#' each of the treatment arms.
#' @param delta the rate difference between a treatment arm and placebo under
#' the null. 0 by default.
#'
#' @returns a data frame with three columns:
#' \describe{
#' \item{\code{arm}}{name of the treatment arm. }
#' \item{\code{placebo}}{name of the placebo arm. }
#' \item{\code{estimate}}{estimate of rate difference. }
#' \item{\code{p}}{one-sided p-value for log odds ratio (treated vs placebo). }
#' \item{\code{info}}{sample size in the subset with \code{NA} being removed. }
#' \item{\code{z}}{the z statistics of log odds ratio (treated vs placebo). }
#' }
#'
#' @references Farrington, Conor P., and Godfrey Manning. "Test statistics and sample size formulae for comparative binomial trials with null hypothesis of non-zero risk difference or non-unity relative risk." Statistics in medicine 9.12 (1990): 1447-1454.
#'
#' @export
#'
fitFarringtonManning <- function(endpoint, placebo, data, alternative, ..., delta = 0) {

  if(!is.character(endpoint) || length(endpoint) != 1){
    stop("endpoint must be a single character string")
  }

  if(!is.character(placebo) || length(placebo) != 1){
    stop("placebo must be a single character string")
  }

  if(!is.data.frame(data)){
    stop("data must be a data frame")
  }

  alternative <- match.arg(alternative, choices = c('greater', 'less'))

  required_cols <- c('arm', endpoint)
  if(!all(required_cols %in% names(data))){
    stop('Columns <',
         paste0(setdiff(required_cols, names(data)), collapse = ', '),
         '> are not present in locked data. ',
         'Please check endpoint\'s name. ')
  }

  if(delta < -1 || delta > 1){
    stop('delta should be in [-1, 1]. ')
  }

  # Prepare the data based on condition ...
  filtered_data <- if(...length() == 0){
    data
  }else{
    tryCatch({
      data %>% dplyr::filter(...)
    },
    error = function(e){
      stop('Error in filtering data for fitting Farrington-Manning test. ',
           'Please check condition in ..., ',
           'which should be compatible with dplyr::filter. ')
    })
  }

  # Check if any data remains after filtering
  if (nrow(filtered_data) == 0) {
    stop("No data remaining after applying subset condition. ")
  }

  treatment_arms <- setdiff(unique(filtered_data$arm), placebo) %>% sort()

  ret <- NULL

  for(trt_arm in treatment_arms){
    sub_data <- filtered_data %>%
      dplyr::filter(.data$arm %in% c(placebo, trt_arm)) %>%
      dplyr::filter(!is.na(.data[[endpoint]]))

    p1 <- mean(sub_data[[endpoint]][sub_data$arm %in% trt_arm])
    p2 <- mean(sub_data[[endpoint]][sub_data$arm %in% placebo])
    n1 <- sum(sub_data$arm %in% trt_arm)
    n2 <- sum(sub_data$arm %in% placebo)
    delta <- 0

    # standard deviation of the rate difference under the null hypothesis (risk difference = -delta)
    theta <- n2/n1
    d <- -p1 * delta * (1 + delta)
    c <- delta^2 + delta * (2 * p1 + theta + 1) + p1 + theta * p2
    b <- -(1 + theta + p1 + theta * p2 + delta * (theta + 2))
    a <- 1 + theta
    v <- b^3/(27*a^3) - b*c/(6*a^2) + d/(2*a)
    u <- sign(v)*sqrt(b^2/(9*a^2) - c/(3*a))
    w <- (pi + acos(   max(min(1, v/u^3), 0, na.rm = TRUE)  ))/3
    p1_null <- 2*u*cos(w) - b/(3*a)
    p2_null <- p1_null - delta
    sd_diff_null <- sqrt(p1_null*(1 - p1_null)/n1 + p2_null*(1 - p2_null)/n2)

    z <- (p1 - p2 - delta)/sd_diff_null
    p <- ifelse(alternative == 'greater', 1 - pnorm(z), pnorm(z))

    info <- nrow(sub_data)

    ret <- rbind(ret, data.frame(arm = trt_arm, placebo = placebo,
                                 estimate = p1 - p2,
                                 p = p, info = info, z = z)
    )
  }

  rownames(ret) <- NULL
  class(ret) <- c('fit_fm', class(ret))
  ret
}
