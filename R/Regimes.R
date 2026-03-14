#' Class of Regimes
#' @description
#' Create a class of regime. A regime defines the rules to select treatments for
#' patients switch, to determine the time of switching, and to update patients'
#' endpoint data.
#'
#' @docType class
#'
#' @export
Regimes <- R6::R6Class(
  'Regimes',

  public = list(

    #' @description
    #' initialize regime
    #'
    #' @param what a function determining whether a patient's data would be
    #' updated due to switching treatment. It takes \code{one_patient_data},
    #' a data frame of one row as input, and returns a character of new treatment.
    #' Return \code{NA} if no treatment switching could be done for this patient.
    #' Note that the returned value will be passed into function `how()`, which
    #' is also provide by users. Thus, it is up to users to decide what to return
    #' from this function. No default value.
    #' @param when a function determining the time at which a patient switches
    #' to another treatment regimen, measured from the time of enrollment.
    #' It takes \code{one_patient_data}, a data frame of one row as
    #' input, and returns a numeric switching time (from \code{enroll_time}).
    #' Note that the returned object will be passed into function `how()`, which
    #' is also provided by users. No default value.
    #' @param how a function updating a patient's data after treatment switching.
    #' @param ... optional arguments for the three functions. No default value.
    initialize =
      function(what, when, how, ...){

        stopifnot(is.function(what) || is.null(what))
        stopifnot(is.function(when) || is.null(when))
        stopifnot(is.function(how) || is.null(how))

        # Capture fixed arguments for functions what, when and how
        dots <- list(...)
        if(length(dots) && (is.null(names(dots)) || any(names(dots) == ''))){
          stop('All extra arguments to regime(...) must be named; ',
               'they are passed to functions specified by `what`, `when` and `how`.')
        }

        isValidFunction <- function(func, func_name, args_order){
          if(!is.function(func)){
            stop(func_name, ' is not a function. ')
          }

          if(!identical(names(formals(func)), args_order)){
            stop('The <', func_name, '> function passed to regime has arguments <',
                 paste0(names(formals(func)), collapse = ', '), '>. \n',
                 'The TrialSimulator convention requires that this function must take the following arguments in the specified order: \n',
                 paste0(args_order, collapse = ', '))
          }
        }

        isValidFunction(what, deparse(substitute(what)), 'one_patient_data')
        isValidFunction(when, deparse(substitute(when)), 'one_patient_data')
        isValidFunction(how, deparse(substitute(how)), c('one_patient_data', 'new_treatment', 'switch_time'))

        private$treatment_selector <- what
        private$time_selector <- when
        private$data_modifier <- how
        private$args <- dots

      },

    #' @description
    #' return user-defined new treatment for a patient
    get_treatment_selector = function(){
      private$treatment_selector
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
    treatment_selector = NULL,
    time_selector = NULL,
    data_modifier = NULL,
    args = NULL
  )

)
