#' Class of Milestones
#' @description
#' Create a class of milestone. A milestone means the time point to take an action,
#' e.g., carrying out (futility, interim, final) analysis for
#' adding/removing arms, or stopping a trial early.
#' It can also be any more general time point where trial
#' data is used in decision making or adaptation. For example, one can define a
#' milestone for changing randomization scheme, sample size re-assessment, trial
#' duration extension etc.
#'
#' Public methods in this R6 class are used in developing
#' this package. Thus, we have to export the whole R6 class which exposures all
#' public methods. However, none of the public methods on this page is
#' useful to end users. Instead, refer to the
#' \href{https://zhangh12.github.io/TrialSimulator/articles/conditionSystem.html}{vignette}
#' to learn how to define milestones when performing simulation using
#' \code{TrialSimulator}.
#'
#' @docType class
#'
#' @export
Milestones <- R6::R6Class(
  'Milestones',

  private = list(
    name = NULL,
    type = NULL,
    trigger_condition = NULL,
    action = NULL,
    action_args = list(),
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
    #' @param ... (optional) arguments of \code{action}.
    initialize = function(name,
                          type = name,
                          trigger_condition,
                          action = doNothing,
                          ...){
      stopifnot(is.character(name) && (length(name) == 1))
      stopifnot(is.character(type))
      if(!('Condition' %in% class(trigger_condition))){
        stop('trigger_condition should be created by functions <',
             'eventNumber, calendarTime, enrollment',
             '> and their combination using (), & and |. ')
      }
      # allow no specified action, for testing purpose.
      stopifnot(is.function(action) || is.null(action))

      # Capture fixed arguments for action
      dots <- list(...)
      if(length(dots) && (is.null(names(dots)) || any(names(dots) == ''))){
        stop('All extra arguments to milestone(...) must be named; ',
             'they are passed to `action`.')
      }

      if(is.function(action)){
        args_in_action <- names(formals(action))
        if(length(args_in_action) == 0 || args_in_action[1] != 'trial'){
          stop('Action function ', deparse(substitute(action)), ' must have ',
               '\'trial\' as its first argument.')
        }
      }

      if(!('...' %in% args_in_action)){
        allowed_args <- setdiff(args_in_action, 'trial')
        unknown_args <- setdiff(names(dots), allowed_args)
        if(length(unknown_args)){
          stop('Unknown argument(s) <',
               paste0(unknown_args, collapse = ', '),
               '> in the action function of milestone <',
               name, '>. ')
        }

        req_mask <- vapply(formals(action)[allowed_args],
                           function(x)
                             identical(x, quote(expr=)), logical(1))
        required <- allowed_args[req_mask]
        missing_req <- setdiff(required, names(dots))
        if(length(missing_req)){
          stop('Missing required argument(s) <',
               paste0(missing_req, collapse = ', '),
               '> in action function of milestone <',
               name, '>. ')
        }
      }

      private$name <- name
      private$type <- type

      private$trigger_condition <- trigger_condition

      private$action <- action
      private$action_args <- dots
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
        action <- .default_action()
      }else{
        action <- do.call(self$get_action(), c(list(trial), private$action_args))
      }

      if(!private$silent && !is.null(action)){
        message('Action for milestone <', self$get_name(), '> is executed: \n')
        # print(action)
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
