#' Class of Controller
#' @description
#' Create a class of controller to run a trial.
#'
#' @docType class
#' @examples
#' ##
#' @export
Controllers <- R6::R6Class(
  'Controllers',

  private = list(
    trial = NULL,
    listener = NULL,
    silent = FALSE,
    dry_run = FALSE,
    output = NULL,

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
    #' @param trial a \code{Trials} object.
    #' @param listener a \code{Listeners} object.
    initialize = function(trial, listener){
      stopifnot(inherits(trial, 'Trials'))
      stopifnot(inherits(listener, 'Listeners'))
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
    #' reset the trial and listener registered to the controller before running
    #' additional replicate of simulation.
    reset = function(){
      self$get_trial()$reset()
      self$get_listener()$reset()
    },

    #' @description
    #' return a data frame of all current outputs saved by calling \code{save}.
    #' @param cols columns to be returned from \code{Controller$output}. If
    #' \code{NULL}, all columns are returned.
    #' @param simplify logical. Return value rather than a data frame of one
    #' column when \code{length(col) == 1} and \code{simplify == TRUE}.
    get_output = function(cols = NULL, simplify = TRUE){
      if(is.null(cols)){
        cols <- colnames(private$output)
      }

      if(!all(cols %in% names(private$output))){
        stop('Columns <', paste0(setdiff(cols, names(private$output)), collapse = ', '),
             '> are not found in trial$output. Check if there is a typo. ')
      }

      ret <- private$output[, cols, drop = FALSE]
      if(simplify && ncol(ret) == 1){
        return(ret[1, 1])
      }else{
        return(ret)
      }
    },

    #' @description
    #' run a trial
    #' @param n number of replicates of simulation. \code{n = 1} by default.
    #' Simulation results can be accessed by \code{Controller$get_output()}.
    #' @param plot_event create event plot
    #' @param silent logical. \code{TRUE} if muting all messages during a
    #' trial. Note that warning messages are still displayed.
    #' @param dry_run \code{TRUE} if action function provided by users is
    #' ignored and a built-in default action \code{default_action} is called
    #' instead. This default function only locks data when the milestone is
    #' triggered. Milestone time and number of endpoints' events or sample sizes
    #' are saved. It is suggested to set \code{dry_run = TRUE} to estimate
    #' distributions of triggering time and number of events before formally
    #' using custom action functions if a fixed design is in use.
    #' This helps determining planned maximum
    #' information for group sequential design and reasonable time of milestone
    #' of interest when planning a trial. Set it to \code{FALSE} for formal
    #' simulations. However, for an adaptive design where arm(s) could
    #' possibly be added or removed, setting \code{dry_run} to \code{TRUE}
    #' is usually not helpful because adaption should be actually applied
    #' to estimate milestone time.
    run = function(n = 1, plot_event = TRUE, silent = FALSE, dry_run = FALSE){

      self$get_trial()$make_arms_snapshot()
      private$output <- NULL

      for(idx in 1:n){
        tryCatch(
          expr = {
            private$run_(plot_event, silent, dry_run)
          },

          error = function(e){
            self$get_trial()$save(e$message, 'error_message', overwrite = TRUE)
            private$output <- bind_rows(private$output, self$get_trial()$get_output())
            stop(e$message)
          }
        )

        private$output <- bind_rows(private$output, self$get_trial()$get_output())

        if(idx < n){
          self$reset()
        }
      }

    }
  )

)
