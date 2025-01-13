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
    listener = NULL,
    silent = FALSE
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
    #' mute all messages (not including warnings)
    #' @param silent logical.
    mute = function(){
      self$get_trial()$mute(private$silent)
      self$get_listener()$mute(private$silent)
    },

    #' @description
    #' run a trial
    #' @param plot_event create event plot
    #' @param silent logical. \code{TRUE} if muting all messages during a
    #' trial. Note that warning messages are still displayed.
    run = function(plot_event = TRUE, silent = FALSE){

      private$silent <- silent
      self$mute()

      self$get_listener()$monitor(self$get_trial())
      if(plot_event){
        self$get_trial()$event_plot()
      }

    }
  )

)
