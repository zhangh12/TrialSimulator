#' Generate Enrollment Time from Piecewise Constant Uniform Distribution
#' @description
#' It assumes a uniform enrollment with constant rate in each of the time
#' windows. This function can be used as the \code{enroller} when calling
#' \code{trial()} to define a trial.
#'
#' @param n integer. Number of random numbers.
#' @param accrual_rate a data frame of columns
#' \describe{
#'       \item{\code{end_time}}{
#'           End time for a constant rate in a time window. The start time of
#'           the first time window is 0.}
#'       \item{\code{piecewise_rate}}{
#'           A constant rate in a time window. So the number of patients
#'           being recruited in that window is window length x
#'           \code{piecewise_rate}. }
#' }
#' @examples
#' accrual_rate <- data.frame(
#'   end_time = c(12, 13:17, Inf),
#'   piecewise_rate = c(15, 15 + 6 * (1:5), 45)
#' )
#'
#' StaggeredRecruiter(30, accrual_rate)
#'
#' accrual_rate <- data.frame(
#'   end_time = c(3, 4, 5, 8, Inf),
#'   piecewise_rate = c(1, 2, 2, 3, 4)
#' )
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
      stopifnot(n_ > 0)
      end <- start + 1/rate * n_
    }

    new_time <- seq(from = start, to = end, by = 1/rate)
    start <- tail(new_time, 1)
    enroll_time <- c(enroll_time, head(new_time, -1))
    rm(new_time)

    if(length(enroll_time) >= n){
      if(!all(diff(enroll_time) > 0)){
        stop('Debug StaggeredRecruiter. ')
      }
      return(head(enroll_time, n))
    }

  }

  stop('No sufficient patients can be recruited from the accrual rate. ',
       'Check your setting or debug StaggeredRecruiter. ')

}





