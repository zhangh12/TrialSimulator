#' Generate Enrollment Time from Piecewise Constant Uniform Distribution
#' @description
#' It assumes a uniform enrollment with constant rate in each of the time
#' windows. This function can be used as the \code{enroller} when calling
#' \code{trial()} to define a trial.
#'
#' @details
#' \code{StaggeredRecruiter} is the only enroller accepted by \code{trial()}:
#' a piecewise constant accrual rate is flexible enough to approximate
#' realistic recruitment in practice, e.g., site ramp-up, steady accrual,
#' and temporary pauses.
#'
#' The returned enrollment times are deterministic, not random. Within a
#' window of positive rate, patients enroll one by one with spacing
#' \code{1/piecewise_rate}; under a constant rate \code{r}, the \code{k}-th
#' patient enrolls exactly at \code{k / r}. In particular, the first patient
#' enrolls at \code{1/piecewise_rate} rather than at time 0, and a
#' milestone triggered by \code{enrollment(n = n)} occurs exactly at the time
#' the planned cumulative accrual reaches \code{n}.
#'
#' A window with \code{piecewise_rate = 0} models a recruitment pause (a hold
#' for safety review, a site not yet activated, a seasonal gap, etc.): no
#' patient is enrolled in that window, and enrollment resumes after its
#' \code{end_time}. Pauses may occur in the first window or span several
#' consecutive windows; a leading pause defers the first enrollment
#' accordingly.
#'
#' A valid \code{accrual_rate} must satisfy all of the following:
#' \itemize{
#' \item it is a data frame with columns \code{end_time} and
#' \code{piecewise_rate};
#' \item \code{end_time} is positive and strictly increasing, and the last
#' entry is \code{Inf} with a positive rate, so that the schedule can supply
#' any number of patients (\code{TrialSimulator} may internally request more
#' than the planned sample size, e.g., for adaptive resizing via
#' \code{resize()});
#' \item rates are non-negative and finite;
#' \item a finite window with a positive rate must expect at least one
#' patient, i.e., window length x \code{piecewise_rate} >= 1. A tiny positive
#' rate meant as a pause is rejected with an error; use
#' \code{piecewise_rate = 0} for a true pause.
#' }
#'
#' @param n integer. Number of enrollment times to generate.
#' @param accrual_rate a data frame of columns
#' \describe{
#'       \item{\code{end_time}}{
#'           End time for a constant rate in a time window. The start time of
#'           the first time window is 0. Values must be positive and strictly
#'           increasing; the last one must be \code{Inf}.}
#'       \item{\code{piecewise_rate}}{
#'           A constant rate in a time window. So the number of patients
#'           being recruited in that window is window length x
#'           \code{piecewise_rate}. A rate of 0 pauses enrollment for that
#'           window. Rates must be non-negative and finite; the last must be
#'           positive.}
#' }
#' @examples
#' ## constant accrual of 25 patients/month: patient k enrolls at k / 25
#' accrual_rate <- data.frame(end_time = Inf, piecewise_rate = 25)
#'
#' StaggeredRecruiter(30, accrual_rate)
#'
#' ## recruitment pause: 30/mo through month 12, paused during months 12-18,
#' ## then 30/mo again. Monthly counts show months 13-17 are empty and
#' ## enrollment resumes at the end of the pause (month 18).
#' accrual_rate <- data.frame(
#'   end_time = c(12, 18, Inf),
#'   piecewise_rate = c(30, 0, 30)
#' )
#'
#' enroll_time <- StaggeredRecruiter(400, accrual_rate)
#' table(ceiling(enroll_time))
#'
#' ## leading pause (first rate is 0): enrollment opens 3 months after study
#' ## start, e.g., the first site is activated with a delay, then 30/mo
#' accrual_rate <- data.frame(
#'   end_time = c(3, Inf),
#'   piecewise_rate = c(0, 30)
#' )
#'
#' StaggeredRecruiter(30, accrual_rate)
#'
#' ## approximate a linear ramp-up by monthly steps: accrual grows by 5/mo
#' ## each month, from 5/mo up to 30/mo, then stays steady at 30/mo
#' accrual_rate <- data.frame(
#'   end_time = c(1:6, Inf),
#'   piecewise_rate = c(seq(5, 30, by = 5), 30)
#' )
#'
#' enroll_time <- StaggeredRecruiter(200, accrual_rate)
#'
#' ## monthly enrolled counts show the ramp (5, 10, ..., 30) and the plateau (30)
#' table(ceiling(enroll_time))
#' @export
StaggeredRecruiter <- function(n, accrual_rate) {

  ## ---- validate n ------------------------------------------------------------
  if (length(n) != 1 || is.na(n) || !is.finite(n) || n <= 0 || n != floor(n)) {
    stop("n must be a positive integer.")
  }

  ## ---- validate accrual_rate -------------------------------------------------
  if (!is.data.frame(accrual_rate) ||
      !all(c("end_time", "piecewise_rate") %in% names(accrual_rate))) {
    stop("accrual_rate must be a data.frame with columns ",
         "'end_time' and 'piecewise_rate'.")
  }

  end_time <- accrual_rate$end_time
  rate <- accrual_rate$piecewise_rate
  k <- length(end_time)

  if (k == 0) {
    stop("accrual_rate must have at least one row.")
  }

  if (!is.numeric(end_time) || !is.numeric(rate)) {
    stop("'end_time' and 'piecewise_rate' must be numeric.")
  }

  if (any(is.na(end_time)) || any(is.na(rate))) {
    stop("'end_time' and 'piecewise_rate' cannot contain NA.")
  }

  if (any(end_time <= 0)) {
    stop("'end_time' must be positive.")
  }

  if (!is.infinite(end_time[k])) {
    stop("The last entry of 'end_time' must be Inf so the schedule can supply ",
         "any number of patients. TrialSimulator may internally request ",
         "several times the planned sample size (for adaptive resizing); an ",
         "open-ended final window prevents a confusing 'insufficient ",
         "patients' error.")
  }

  if (k > 1 && any(is.infinite(end_time[-k]))) {
    stop("Only the last entry of 'end_time' can be Inf.")
  }

  if (k > 1 && any(diff(end_time) <= 0)) {
    stop("'end_time' must be strictly increasing.")
  }

  if (any(!is.finite(rate))) {
    stop("'piecewise_rate' must be finite.")
  }

  if (any(rate < 0)) {
    stop("'piecewise_rate' must be non-negative.")
  }

  if (rate[k] <= 0) {
    stop("The last 'piecewise_rate' must be positive because the last ",
         "'end_time' is Inf.")
  }

  ## ---- per-window geometry ---------------------------------------------------
  start_time <- c(0, head(end_time, -1))
  window_length <- end_time - start_time

  positive_rate <- rate > 0
  window_capacity <- numeric(k)            # 0 for pause windows (rate == 0)
  window_capacity[positive_rate] <-
    window_length[positive_rate] * rate[positive_rate]

  ## A positive-rate finite window expecting fewer than one patient would be a
  ## near-no-op and is almost certainly a mistake (a tiny rate meant as a
  ## pause). Pauses (rate == 0) are intentional and excluded. Stop, don't warn.
  ## The 1e-8 slack keeps an intended one-patient window (rate = 1/width) from
  ## being rejected: width * (1/width) frequently rounds just below 1 in
  ## floating point (e.g. width 49), and such a window does enroll one patient.
  too_low <- which(is.finite(window_length) & positive_rate &
                     window_capacity < 1 - 1e-8)
  if (length(too_low) > 0) {
    details <- vapply(
      too_low,
      function(i) {
        paste0("row ", i,
               " (", signif(start_time[i], 8), ", ", signif(end_time[i], 8),
               "], piecewise_rate = ", signif(rate[i], 8),
               ", width * rate = ", signif(window_capacity[i], 8))
      },
      character(1)
    )
    stop("Some finite accrual windows are too low to enroll anyone ",
         "(window length * piecewise_rate < 1): ",
         paste(details, collapse = "; "),
         ". Use piecewise_rate = 0 to pause enrollment in such a window, ",
         "or a rate of at least 1/width.",
         call. = FALSE)
  }

  ## ---- invert the cumulative accrual intensity -------------------------------
  ## cum_capacity is non-decreasing; pause windows add 0, so they form
  ## zero-width gaps that findInterval (rightmost tie) never selects -- hence
  ## window_id never points at a rate == 0 window and the division below is safe.
  cum_capacity <- cumsum(window_capacity)
  cum_before <- c(0, head(cum_capacity, -1))

  ## patient k enrolls when the expected cumulative enrollment reaches k
  ## (not k - 1): with a constant rate r the n-th patient enrolls exactly at
  ## n / r, so enrollment(n)-triggered milestones occur exactly at the time
  ## the planned cumulative accrual reaches n.
  target <- seq_len(n)
  window_id <- findInterval(target, cum_capacity) + 1

  enroll_time <- start_time[window_id] +
    (target - cum_before[window_id]) / rate[window_id]

  enroll_time
}
