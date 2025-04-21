#' Class of Listener
#' @description
#' Create a class of listener. A listener monitors the trial while checking
#' condition of pre-defined events. Actions are triggered and executed
#' automatically.
#'
#' @docType class
#' @examples
#' ##
#' @export
Listeners <- R6::R6Class(
  'Listeners',

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
        stopifnot(inherits(event, 'Events'))
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
    #' @param event_name return \code{Event} object with given name(s).
    #' If \code{NULL}, all registered events are returned.
    get_events = function(event_name = NULL){
      if(is.null(event_name)){
        return(private$events)
      }

      if(!(event_name %in% names(private$events))){
        stop('Event <', event_name, '> is not registered. ')
      }

      return(private$events[[event_name]])
    },

    #' @description
    #' return names of registered events
    get_event_names = function(){

      names <- NULL
      for(event in private$events){
        names <- c(names, event$get_name())
      }
      return(names)

    },

    #' @description
    #' scan, check, and trigger registered events.
    #' Events are triggered in the order when calling
    #' \code{Listener$add_events}.
    #' @param trial a \code{Trial} object.
    #' @param dry_run logical. See \code{Controller::run} for more information.
    monitor = function(trial, dry_run){

      if(!trial$has_arm()){
        stop('No arm is found in the trial. ',
             'Make sure that Trial$add_arms() has been executed before running the trial. ')
      }
      for(event in self$get_events()){
        event$set_dry_run(dry_run)
        event$trigger_event(trial)
        event$set_dry_run(FALSE)
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
    },

    #' @description
    #' reset all events registered to the listener. Usually, this is called
    #' before a controller can run additional replicates of simulation.
    reset = function(){
      events <- self$get_events()
      for(event in events){
        event$reset()
      }
    }
  )

)
