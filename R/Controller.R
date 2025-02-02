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
    silent = FALSE,
    dry_run = FALSE,

    run_ = function(plot_event = TRUE, silent = FALSE, dry_run = FALSE){

      private$silent <- silent
      private$dry_run <- dry_run
      self$mute()

      self$get_listener()$monitor(self$get_trial(), private$dry_run)
      if(plot_event){
        self$get_trial()$event_plot()
      }

    }
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
      private$silent <- FALSE
      private$dry_run <- FALSE
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
    #' @param dry_run \code{TRUE} if action function provided by users is
    #' ignored and a built-in default action \code{default_action} is called
    #' instead. This default function only locks data when the event is
    #' triggered. Event time and number of endpoints' events or sample sizes
    #' are saved. It is suggested to set \code{dry_run = TRUE} to estimate
    #' distributions of triggering time and number of events before formally
    #' using custom action functions if a fixed design is in use.
    #' This helps determining planned maximum
    #' information for group sequential design and reasonable time of event
    #' of interest when planning a trial. Set it to \code{FALSE} for formal
    #' simulations. However, for an adaptive design where arm(s) could
    #' possibly be added or removed, setting \code{dry_run} to \code{TRUE}
    #' is usually not helpful because adaption should be actually applied
    #' to estimate event time.
    run = function(plot_event = TRUE, silent = FALSE, dry_run = FALSE){

      tryCatch(
        expr = {
          private$run_(plot_event, silent, dry_run)
        },

        error = function(e){
          self$get_trial()$save(e$message, 'error_message', overwrite = TRUE)
          stop(e$message)
        }
      )

    }
  )

)
