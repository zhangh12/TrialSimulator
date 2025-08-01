#' Class of Milestones
#' @description
#' Create a class of milestone. An milestone means the time point to take an action,
#' e.g., carry out (futility, interim, final) analysis for add/remove arms,
#' or stop a trial early. It can also be any more general time point where trial
#' data is used in decision making or adaptation. For example, one can define a
#' milestone for changing randomization scheme, sample size re-assessment, trial
#' duration extension etc.
#'
#' @docType class
#' @examples
#' ##
#' @export
Milestones <- R6::R6Class(
  'Milestones',

  private = list(
    name = NULL,
    type = NULL,
    trigger_condition = NULL,
    action = NULL,
    triggered = FALSE, ## logical. Whether this milestone has been triggered
                      ## (to avoid repeated execution)
    silent = FALSE,
    is_dry_run = FALSE
  ),

  public = list(

    #' @description
    #' initialize milestone
    #' @param name character. Name of milestone.
    #' @param type character vector. Milestone type(s) (futility, interim, final),
    #' a milestone can be of multiple types. This is for information purpose so
    #' can be any string.
    #' @param trigger_condition function to check if this milestone should
    #' trigger. See vignette \code{Condition System for Triggering Milestones in a Trial}.
    #' @param action function to execute when the milestone triggers.
    initialize = function(name, type = name, trigger_condition, action = doNothing){
      stopifnot(is.character(name) && (length(name) == 1))
      stopifnot(is.character(type))
      if(!('Condition' %in% class(trigger_condition))){
        stop('trigger_condition should be created by functions <',
             'eventNumber, calendarTime, enrollment',
             '> and their combination using (), & and |. ')
      }
      # allow no specified action, for testing purpose.
      stopifnot(is.function(action) || is.null(action))

      if(is.function(action)){
        args_in_action <- names(formals(action))
        if(!all(c('trial', 'milestone_name') %in% args_in_action)){
          stop('action function ', deparse(substitute(action)), ' must use ',
               'arguments \'trial\' and \'milestone_name\'.')
        }
      }

      private$name <- name
      private$type <- type

      private$trigger_condition <- trigger_condition

      private$action <- action
      private$triggered <- FALSE
      private$is_dry_run <- FALSE
    },

    #' @description
    #' return name of milestone
    get_name = function(){
      private$name
    },

    #' @description
    #' return type(s) of milestone
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
    #' set if dry run should be carried out for the milestone. For more details,
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

      if(!private$silent && !is.null(action)){
        message('Action for milestone <', self$get_name(), '> is executed: \n')
        print(action)
      }

    },

    #' @description
    #' return trigger status
    get_trigger_status = function(){
      private$triggered
    },

    #' @description
    #' reset an milestone so that it can be triggered again. Usually, this is called
    #' before the controller of a trial can run additional replicates
    #' of simulation.
    reset = function(){
      private$triggered <- FALSE
    },

    #' @description
    #' trigger an milestone (always TRUE) and execute action accordingly. It calls
    #' Trial$get_data_lock_time() to lock data based on conditions implemented
    #' in Milestones$trigger_condition. If time that meets the condition cannot be
    #' found, Trial$get_data_lock_time() will throw an error and stop the
    #' program. This means that user needs to adjust their trigger_condition
    #' (e.g., target number of events (target_n_events) is impossible to
    #' reach).
    #' @param trial a \code{Trial} object.
    trigger_milestone = function(trial){
      if(self$get_trigger_status()){
        if(!private$silent){
          message('Milestone <', self$get_name(),
                  '> has already been triggered before, thus is skipped. ')
        }
        return(NULL)
      }

      ## find time that meets the trigger condition to lock data in trial,
      ## so that action can be executed on it.
      if(!private$silent){
        message('Condition of milestone <', self$get_name(), '> is being checked. \n')
      }

      data_lock_time <- self$get_trigger_condition()$get_trigger_time(trial)

      ## always lock data after an milestone and before taking actions
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
