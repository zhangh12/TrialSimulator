#' Triggering condition by number of randomized patients
#'
#' @description
#' Define a condition to trigger trial event by the number of randomized patients.
#'
#' @param n integer. Number of randomized patients.
#'
#' @returns an object of class `Condition`
#'
#' @export
enrollment <- function(n){

  EnrollmentCountCondition$new(n)

}
