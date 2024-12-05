#' Class of Controller
#' @description
#' Create a class of controller to run a trial.
#'
#' @docType class
#' @examples
#' ##
#' @export
Controller <- R6::R6Class(
  'Controller',

  private = list(
    trial = NULL,
    listener = NULL
  ),

  public = list(

    #' @description
    #' initialize a controller of the trial
    #' @param trial a \code{Trial} object.
    #' @param listener a \code{Listener} object.
    initialize = function(trial, listener){
      stopifnot(inherits(trial, 'Trial'))
      stopifnot(inherits(listener, 'Listener'))
      private$trial <- trial
      private$listener <- listener
    },

    #' @description
    #' return listener
    get_listener = function(){
      private$listener
    },

    #' @description
    #' return trial
    get_trial = function(){
      private$trial
    },

    #' @description
    #' run a trial
    run = function(){

      self$get_listener()$monitor(self$get_trial())
      self$get_trial()$event_plot()

    }
  )

)
