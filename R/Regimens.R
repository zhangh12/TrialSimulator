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
    #' is also provide by users. This argument can also be a
    #' list of functions that will be executed sequentially. No default value.
    #' @param when a function determining the time at which a patient switches
    #' to another treatment regimen, measured from the time of enrollment.
    #' It takes \code{patient_data}, a data frame as
    #' argument, and returns a data frame of two columns \code{patient_id} and
    #' \code{switch_time} (from \code{enroll_time}). No \code{NA} is allowed
    #' in \code{switch_time} and the number of rows in the returned data frame
    #' must equal the number of rows in \code{patient_data}, i.e., switching
    #' time must be specified to every patients.
    #' Note that the returned object will be passed into function `how()`, which
    #' is also provided by users. This argument can also be a
    #' list of functions that will be executed sequentially. No default value.
    #' @param how a function updating patients' data after treatment switching.
    #' Only modified columns and \code{patient_id} are returned. A cell will
    #' be omitted if \code{NA}, meaning no change to that patient for the endpoint
    #' or other variables. Equivalently, users can also fill the cell with
    #' its original value. This argument can also be a list of functions that
    #' will be executed sequentially. No default value.
    initialize =
      function(what, when, how){

        private$validate_arguments(what, when, how)

        private$treatment_allocator <- what
        private$time_selector <- when
        private$data_modifier <- how

      },

    #' @description
    #' return number of treatment allocators for regimen
    get_number_treatment_allocator = function(){
      length(private$treatment_allocator)
    },

    #' @description
    #' return user-defined new treatment for a patient
    #' @param index integer. Index of allocator. Return all allocators if \code{NULL}.
    get_treatment_allocator = function(index = NULL){
      if(is.null(index)){
        return(private$treatment_allocator)
      }else{
        stopifnot(is.wholenumber(index) && index > 0)
        stopifnot(length(index) == 1)
        if(index > self$get_number_treatment_allocator()){
          stop('There are only <', self$get_number_treatment_allocator(),
               '> treatment allocators, and cannot request for the ',
               index, 'th. ')
        }
        return(private$treatment_allocator[[index]])
      }
    },

    #' @description
    #' return number of time selector for regimen
    get_number_time_selector = function(){
      length(private$time_selector)
    },

    #' @description
    #' return user-defined time selector
    #' @param index integer. Index of selector. Return all selectors if \code{NULL}.
    get_time_selector = function(index = NULL){
      if(is.null(index)){
        return(private$time_selector)
      }else{
        stopifnot(is.wholenumber(index) && index > 0)
        stopifnot(length(index) == 1)
        if(index > self$get_number_time_selector()){
          stop('There are only <', self$get_number_time_selector(),
               '> time selectors, and cannot request for the ',
               index, 'th. ')
        }
        return(private$time_selector[[index]])
      }
    },

    #' @description
    #' return number of data modifier for regimen
    get_number_data_modifier = function(){
      length(private$data_modifier)
    },

    #' @description
    #' return user-defined endpoint data modifier
    #' @param index integer. Index of selector. Return all modifiers if \code{NULL}.
    get_data_modifier = function(index = NULL){
      if(is.null(index)){
        return(private$data_modifier)
      }else{
        stopifnot(is.wholenumber(index) && index > 0)
        stopifnot(length(index) == 1)
        if(index > self$get_number_data_modifier()){
          stop('There are only <', self$get_number_data_modifier(),
               '> data modifiers, and cannot request for the ',
               index, 'th. ')
        }
        return(private$data_modifier[[index]])
      }
    }
  ),

  private = list(
    treatment_allocator = NULL,
    time_selector = NULL,
    data_modifier = NULL,

    validate_arguments = function(what, when, how){

      isValidFunction <- function(func, func_type, func_name, args_order){
        if(!is.function(func)){
          stop(func_name, ' is not a function. ')
        }

        if(!identical(names(formals(func)), args_order)){
          stop('The <', func_type, '> function <', func_name, '> passed to regimen has arguments <',
               paste0(names(formals(func)), collapse = ', '), '>. \n',
               'The TrialSimulator convention requires that this function must take the following arguments in the specified order: \n',
               paste0(args_order, collapse = ', '))
        }
      }

      stopifnot(is.function(what) || is.list(what))
      stopifnot(is.function(when) || is.list(when))
      stopifnot(is.function(how) || is.list(how))

      if(is.function(what)){
        stopifnot(is.function(when) && is.function(how))
        what <- list(what)
        when <- list(when)
        how <- list(how)
      }else{
        stopifnot(is.list(when) && is.list(how))
        stopifnot(length(what) == length(when))
        stopifnot(length(what) == length(how))
      }

      for(i in seq_along(what)){
        isValidFunction(what[[i]], 'what', deparse(substitute(what[[i]])), 'patient_data')
        isValidFunction(when[[i]], 'when', deparse(substitute(when))[[i]], 'patient_data')
        isValidFunction(how[[i]], 'how', deparse(substitute(how))[[i]], 'patient_data')
      }

    }
  )

)
