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
#' \item \code{$add_milestones()}
#' }
#'
#' @docType class
#' @examples
#' ##
#' @export
Listeners <- R6::R6Class(
  'Listeners',

  private = list(
    milestones = list(),
    silent = FALSE
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
          warning('Listener has milestone <', milestone$get_name(), '> already. ',
                  'Do you want to over-write it? \n')
        }
        private$milestones[[milestone$get_name()]] <- milestone

        if(!private$silent){
          message('A milestone <', milestone$get_name(), '> is registered. ')
        }
      }
    },

    #' @description
    #' return registered milestones
    #' @param milestone_name return \code{Milestone} object with given name(s).
    #' If \code{NULL}, all registered milestones are returned.
    get_milestones = function(milestone_name = NULL){
      if(is.null(milestone_name)){
        return(private$milestones)
      }

      if(!(milestone_name %in% names(private$milestones))){
        stop('Milestone <', milestone_name, '> is not registered. ')
      }

      return(private$milestones[[milestone_name]])
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

    #' @description
    #' scan, check, and trigger registered milestones.
    #' Milestones are triggered in the order when calling
    #' \code{Listener$add_milestones}.
    #' @param trial a \code{Trial} object.
    #' @param dry_run logical. See \code{Controller::run} for more information.
    monitor = function(trial, dry_run){

      if(!trial$has_arm()){
        stop('No arm is found in the trial. ',
             'Make sure that Trial$add_arms() has been executed before running the trial. ')
      }
      for(milestone in self$get_milestones()){
        milestone$set_dry_run(dry_run)
        milestone$trigger_milestone(trial)
        milestone$set_dry_run(FALSE)
      }
    },

    #' @description
    #' mute all messages (not including warnings)
    #' @param silent logical.
    mute = function(silent){
      private$silent <- silent
      for(milestone in self$get_milestones()){
        milestone$mute(private$silent)
      }
    },

    #' @description
    #' reset all milestones registered to the listener. Usually, this is called
    #' before a controller can run additional replicates of simulation.
    reset = function(){
      milestones <- self$get_milestones()
      for(milestone in milestones){
        milestone$reset()
      }
    }
  )

)
