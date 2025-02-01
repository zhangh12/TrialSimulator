#' Class of Event
#' @description
#' Create a class of event. An event means the time point to take an action,
#' e.g., carry out (futility, interim, final) analysis for add/remove arms,
#' or stop a trial early.
#'
#' @docType class
#' @examples
#' ##
#' @export
Event <- R6::R6Class(
  'Event',

  private = list(
    name = NULL,
    type = NULL,
    trigger_condition = NULL,
    action = NULL,
    triggered = FALSE, ## logical. Whether this event has been triggered
                      ## (to avoid repeated execution)
    silent = FALSE,
    is_dry_run = FALSE
  ),

  public = list(

    #' @description
    #' initialize Event
    #' @param name character. Name of event.
    #' @param type character vector. Event type(s) (futility, interim, final),
    #' an event can be of multiple types.
    #' @param trigger_condition function to check if this event should
    #' trigger. Return TRUE/FALSE.
    #' @param action function to execute when the event triggers.
    #' @param ... arguments for \code{trigger_condition}.
    initialize = function(name, type, trigger_condition, action = NULL, ...){
      stopifnot(is.character(name) && (length(name) == 1))
      stopifnot(is.character(type))
      stopifnot(is.function(trigger_condition))
      # allow no specified action, for testing purpose.
      stopifnot(is.function(action) || is.null(action))

      if(is.function(action)){
        args_in_action <- names(formals(action))
        if(!all(c('trial', 'event_name') %in% args_in_action)){
          stop('action function ', deparse(substitute(action)), ' must use ',
               'arguments \'trial\' and \'event_name\'.')
        }
      }

      private$name <- name
      private$type <- type

      private$trigger_condition <-
        DynamicTriggerConditionFunction(
          trigger_condition,
          trigger_condition_name = deparse(substitute(generator)),
          event_name = name, ...)

      private$action <- action
      private$triggered <- FALSE
      private$is_dry_run <- FALSE
    },

    #' @description
    #' return name of event
    get_name = function(){
      private$name
    },

    #' @description
    #' return type(s) of event
    get_type = function(){
      private$type
    },

    #' @description
    #' return trigger_condition function
    get_trigger_condition = function(){
      private$trigger_condition
    },

    #' @description
    #' return action function
    get_action = function(){
      private$action
    },

    #' @description
    #' set if dry run should be carried out for the event. For more details,
    #' refer to \code{Controller::run}.
    #' @param dry_run logical.
    set_dry_run = function(dry_run){
      private$is_dry_run <- dry_run
    },

    #' @description
    #' execute action function
    #' @param trial a \code{Trial} object.
    execute_action = function(trial){

      if(private$is_dry_run){
        action <- default_action()
      }else{
        action <- self$get_action()(trial, self$get_name())
      }

      if(!private$silent){
        message('Action for <', self$get_name(), '> is executed: \n')
        print(action)
      }

    },

    #' @description
    #' return trigger status
    get_trigger_status = function(){
      private$triggered
    },

    #' @description
    #' trigger an event (always TRUE) and execute action accordingly. It calls
    #' Trial$get_data_lock_time() to lock data based on conditions implemented
    #' in Event$trigger_condition. If time that meets the condition cannot be
    #' found, Trial$get_data_lock_time() will throw an error and stop the
    #' program. This means that user needs to adjust their trigger_condition
    #' (e.g., target number of events (target_n_events) is impossible to
    #' reach).
    #' @param trial a \code{Trial} object.
    #' @param ... other arguments.
    trigger_event = function(trial, ...){
      if(self$get_trigger_status()){
        if(!private$silent){
          message('Event <', self$get_name(),
                  '> has already been triggered before, thus is skipped. ')
        }
        return(NULL)
      }

      ## find time that meets the trigger condition to lock data in trial,
      ## so that action can be executed on it.
      if(!private$silent){
        message('Conditioin of event <', self$get_name(), '> is being checked. \n')
      }
      data_lock_time <- self$get_trigger_condition()(trial)

      ## always lock data after an event and before taking actions
      trial$lock_data(data_lock_time, self$get_name())

      self$execute_action(trial)
      private$triggered <- TRUE
    },

    #' @description
    #' mute all messages (not including warnings)
    #' @param silent logical.
    mute = function(silent){
      private$silent <- silent
    }
  )

)
