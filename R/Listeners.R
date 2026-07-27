#' Class of Listener
#' @description
#' Create a class of listener. A listener monitors the trial while checking
#' condition of pre-defined milestones. Actions are triggered and executed
#' automatically.
#'
#' Public methods in this R6 class are used in developing
#' this package. Thus, we have to export the whole R6 class which exposures all
#' public methods. However, only the public methods in the list below are
#' useful to end users.
#'
#' \itemize{
#' \item \code{$add_milestones()} register milestone(s) with the listener.
#' \item \code{$get_milestone_names()} return names of registered milestones.
#' }
#'
#' \strong{Internal machinery.} The remaining public methods
#' (\code{$monitor()}, \code{$mute()} and \code{$reset()}) are public only
#' because they are invoked on a listener object by other components of the
#' package (controllers), which R6 cannot grant through private members.
#' Users should not call them directly.
#'
#' @docType class
#' @examples
#' ##
#' @return an \code{R6Class} generator object; use \code{listener()} to create a listener.
#'
#' @export
Listeners <- R6::R6Class(
  'Listeners',

  private = list(
    milestones = list(),
    silent = FALSE,

    ## @description
    ## return registered milestones
    ## @param milestone_name return \code{Milestone} object with given name(s).
    ## If \code{NULL}, all registered milestones are returned.
    get_milestones = function(milestone_name = NULL){
      if(is.null(milestone_name)){
        return(private$milestones)
      }

      if(!(milestone_name %in% names(private$milestones))){
        stop('Milestone <', milestone_name, '> is not registered. ')
      }

      return(private$milestones[[milestone_name]])
    }
  ),

  public = list(

    #' @description
    #' initialize a listener
    #' @param silent logical. \code{TRUE} to mute messages.
    initialize = function(silent = FALSE){
      stopifnot(is.logical(silent))
      private$silent <- silent
      private$milestones <- list()
    },

    #' @description
    #' register milestones with listener. Order in \code{...} matter
    #' as they are scanned and triggered in that order. It is users'
    #' responsibility to use reasonable order when calling this function,
    #' otherwise, the result of \code{Listeners$monitor()} can be problematic.
    #' @param ... one or more objects returned from \code{milestone()}.
    #'
    #' @examples
        #' listener <- listener()
        #' interim <- milestone(name = 'interim',
        #'                      when = eventNumber('endpoint', n = 100)
        #'                     )
        #' final <- milestone(name = 'final',
        #'                    when = calendarTime(time = 24)
        #'                   )
        #' listener$add_milestones(interim, final)
        #'
    add_milestones = function(...){
      milestone_list <- list(...)

      for(milestone in milestone_list){
        stopifnot(inherits(milestone, 'Milestones'))
        if(milestone$get_name() %in% names(private$milestones)){
          stop('Listener has milestone <', milestone$get_name(), '> already. ',
               'A registered milestone cannot be over-written. ',
               'Use a different name, create a new listener, or update a ',
               'not-yet-triggered milestone within an action function ',
               'through update_milestone(). ')
        }
        private$milestones[[milestone$get_name()]] <- milestone

        if(!private$silent){
          message('A milestone <', milestone$get_name(), '> is registered. ')
        }
      }
    },

    #' @description
    #' return names of registered milestones
    get_milestone_names = function(){

      names <- NULL
      for(milestone in private$milestones){
        names <- c(names, milestone$get_name())
      }
      return(names)

    },

    ## ---- internal machinery (called by other components; not for users) -----

    #' @description
    #' \strong{INTERNAL MACHINERY: DO NOT CALL THIS METHOD DIRECTLY.}
    #'
    #' scan, check, and trigger registered milestones.
    #' Milestones are triggered in the order when calling
    #' \code{Listener$add_milestones}.
    #' @param trial a \code{Trial} object.
    monitor = function(trial){

      if(!trial$has_arm()){
        stop('No arm is found in the trial. ',
             'Make sure that Trial$add_arms() has been executed before running the trial. ')
      }
      for(milestone in private$get_milestones()){
        tryCatch(
          {
            milestone$trigger_milestone(trial)

            ## apply milestone updates requested by the action just executed
            ## through Trials$update_milestone(). The trial object serves as
            ## the mailbox between the action function (which can only reach
            ## the trial) and this listener (which owns the milestones):
            ## requests are queued during the action and consumed here,
            ## right after the action returns and before the loop moves on,
            ## so an update to the very next milestone is already in effect
            ## when it is evaluated.
            for(request in trial$pop_milestone_updates()){
              target <- private$milestones[[request$name]]
              if(is.null(target)){
                stop('Milestone <', request$name,
                     '> is not registered with the trial and thus cannot ',
                     'be modified in milestone <', request$requested_by,
                     '>. Registered milestone(s): <',
                     paste0(names(private$milestones), collapse = ', '),
                     '>. ')
              }
              if(target$get_trigger_status()){
                stop('update_milestone() called in the action function of ',
                     'milestone <', request$requested_by, '>: milestone <',
                     request$name,
                     '> has already been triggered and cannot be updated. ')
              }
              if(!is.null(request$when)){
                target$set_trigger_condition(request$when)
              }
              if(!is.null(request$action)){
                target$set_action_function(request$action,
                                           request$action_args)
              }
            }
          },
          error = function(e){
            trial$save(e$message, 'error_message', overwrite = TRUE)
            stop('Error in executing action function of milestone <',
                 milestone$get_name(), '>: \n',
                 e$message, '\n\n',
                 'Please set a breakpoint in its action function to debug it. \n',
                 'The browser() function can be helpful for a step-by-step diagnosis. \n',
                 'To fully replicate the issue in debugging, set seed = ',
                 trial$get_output('seed'), ' in trial(...). ')
          }
        )
      }
    },

    #' @description
    #' \strong{INTERNAL MACHINERY: DO NOT CALL THIS METHOD DIRECTLY.}
    #'
    #' mute all messages (not including warnings)
    #' @param silent logical.
    mute = function(silent){
      private$silent <- silent
      for(milestone in private$get_milestones()){
        milestone$mute(private$silent)
      }
    },

    #' @description
    #' \strong{INTERNAL MACHINERY: DO NOT CALL THIS METHOD DIRECTLY.}
    #'
    #' reset all milestones registered to the listener. Usually, this is called
    #' before a controller can run additional replicates of simulation.
    reset = function(){
      milestones <- private$get_milestones()
      for(milestone in milestones){
        milestone$reset()
      }
    }
  )

)
