#' Generate enrollment time from piecewise constant uniform distribution
#' @description
#' Accrual rate can be 10 patients/months for the first 2 months, 20 patients/
#' months for the second 2 months, and eventually 30 patients/months until the
#' end of trial.
#' @param n number of random numbers
#' @param accrual_rate a data frame of columns
#'       \code{end_time}:
#'           End time for a constant rate in a time window. The start time of
#'           the first time window is 0.
#'       \code{piecewise_rate}:
#'           A constant rate in a time window. So the number of patients
#'           being recruited in that window is window length x \code{piecewise_rate}
#'
#' @examples
#' accrual_rate <- data.frame(
#'   end_time = c(12, 13:17, Inf),
#'   piecewise_rate = c(15, 15 + 6 * (1:5), 45)
#')
#'
#'accrual_rate <- data.frame(
#'  end_time = c(3, 4, 5, 8, Inf),
#'  piecewise_rate = c(1, 2, 2, 3, 4)
#')
#'
#' StaggeredRecruiter(30, accrual_rate)
#' @export
StaggeredRecruiter <- function(n, accrual_rate){

  start <- 0
  enroll_time <- NULL
  for(i in seq_along(accrual_rate$end_time)){
    end <- accrual_rate$end_time[i]
    rate <- accrual_rate$piecewise_rate[i]
    if(is.infinite(end)){
      ## figure out how many more patients are needed
      ## because accrual rate is flat from now on
      n_ <- n - length(enroll_time)
      end <- start + 1/rate * n_
      enroll_time <- c(enroll_time,
                       seq(from = start, to = end, by = 1/rate)[-1]
                       )
      stopifnot(length(enroll_time) == n)
      return(enroll_time)
    }
    enroll_time <- c(enroll_time,
                     seq(from = start, to = end,
                         by = 1/rate)[-1]
                     )
    if(length(enroll_time) > n){
      return(head(enroll_time, n))
    }

    start <- tail(enroll_time, 1)

  }
}





