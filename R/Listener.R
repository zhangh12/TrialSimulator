#' Class of Listener
#' @description
#' Create a class of listener. A listener monitors the trial while checking
#' condition of pre-defined events. Actions are triggered and executed.
#'
#' @docType class
#' @examples
#' ##
#' @export
Listener <- R6::R6Class(
  'Listener',

  private = list(
    events = list(),
    silent = FALSE
  ),

  public = list(

    #' @description
    #' initialize a listener
    initialize = function(){
      private$events <- list()
    },

    #' @description
    #' register events with listener. Order in \code{...} matter
    #' as they are scanned in that order. It is user's responsibility
    #' to use reasonable order when calling this function, otherwise,
    #' the result of \code{Listener$monitor()} can be problematic.
    #' @param ... events
    add_events = function(...){
      event_list <- list(...)

      for(event in event_list){
        stopifnot(inherits(event, 'Event'))
        if(event$get_name() %in% names(private$events)){
          warning('Listener has event <', event$get_name(), '> already. ',
                  'Do you want to over-write it? \n')
        }
        private$events[[event$get_name()]] <- event

        if(!private$silent){
          message('An event <', event$get_name(), '> is registered. ')
        }
      }
    },

    #' @description
    #' return registered events
    get_events = function(){
      private$events
    },

    #' @description
    #' scan, check, and trigger registered events.
    #' Events are triggered in the order when calling
    #' \code{Listener$add_events}.
    #' @param trial a \code{Trial} object.
    monitor = function(trial){
      for(event in self$get_events()){
        event$trigger_event(trial)
      }
    },

    #' @description
    #' mute all messages (not including warnings)
    #' @param silent logical.
    mute = function(silent){
      private$silent <- silent
      for(event in self$get_events()){
        event$mute(private$silent)
      }
    }
  )

)
