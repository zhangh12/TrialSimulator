#' Class of Regimens
#' @description
#' Create a class of regimen. A regimen defines the rules to select treatments for
#' patients switch, to determine the time of switching, and to update patients'
#' endpoint data.
#'
#' @docType class
#'
#' @export
Regimens <- R6::R6Class(
  'Regimens',

  public = list(

    #' @description
    #' initialize regimen
    #'
    #' @param what a function determining whether patients' data would be
    #' updated due to switching treatment. It takes \code{patient_data},
    #' a data frame as argument, and returns a data frame of two columns
    #' \code{patient_id} and \code{new_treatment}. Patients with \code{NA} in
    #' \code{new_treatment} will be skipped.
    #' The number of rows in the returned data frame may be smaller than the
    #' number of patients in the input data frame. This indicates that some
    #' patients' data will not be modifier.
    #' Note that the returned object will be passed into function `how()`, which
    #' is also provide by users. No default value.
    #' @param when a function determining the time at which a patient switches
    #' to another treatment regimen, measured from the time of enrollment.
    #' It takes \code{patient_data}, a data frame as
    #' argument, and returns a data frame of two columns \code{patient_id} and
    #' \code{switch_time} (from \code{enroll_time}). No \code{NA} is allowed
    #' in \code{switch_time} and the number of rows in the returned data frame
    #' must equal the number of rows in \code{patient_data}, i.e., switching
    #' time must be specified to every patients.
    #' Note that the returned object will be passed into function `how()`, which
    #' is also provided by users. No default value.
    #' @param how a function updating patients' data after treatment switching.
    #' @param ... optional arguments for the three functions. No default value.
    initialize =
      function(what, when, how){

        stopifnot(is.function(what) || is.null(what))
        stopifnot(is.function(when) || is.null(when))
        stopifnot(is.function(how) || is.null(how))

        isValidFunction <- function(func, func_name, args_order){
          if(!is.function(func)){
            stop(func_name, ' is not a function. ')
          }

          if(!identical(names(formals(func)), args_order)){
            stop('The <', func_name, '> function passed to regimen has arguments <',
                 paste0(names(formals(func)), collapse = ', '), '>. \n',
                 'The TrialSimulator convention requires that this function must take the following arguments in the specified order: \n',
                 paste0(args_order, collapse = ', '))
          }
        }

        isValidFunction(what, deparse(substitute(what)), 'patient_data')
        isValidFunction(when, deparse(substitute(when)), 'patient_data')
        isValidFunction(how, deparse(substitute(how)), 'patient_data')

        private$treatment_allocator <- what
        private$time_selector <- when
        private$data_modifier <- how

      },

    #' @description
    #' return user-defined new treatment for a patient
    get_treatment_allocator = function(){
      private$treatment_allocator
    },

    #' @description
    #' return user-defined time selector
    get_time_selector = function(){
      private$time_selector
    },

    #' @description
    #' return user-defined endpoint data modifier
    get_data_modifier = function(){
      private$data_modifier
    }
  ),

  private = list(
    treatment_allocator = NULL,
    time_selector = NULL,
    data_modifier = NULL
  )

)
