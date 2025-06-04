

EventCountCondition <- R6::R6Class(
  'EventCountCondition',

  inherit = Condition,

  public = list(
    endpoint = NULL,
    n = NULL,
    arms = NULL,

    initialize = function(endpoint, n, arms = NULL){
      stopifnot(is.character(endpoint))
      stopifnot(length(endpoint) == 1)

      stopifnot(is.wholenumber(n))
      stopifnot(length(n) == 1)

      stopifnot(is.null(arms) || is.character(arms))

      self$endpoint <- endpoint
      self$n <- n
      self$arms <- arms
    },

    get_trigger_time = function(trial){

      milestone_time <- trial$get_data_lock_time_by_event_number(
        endpoints = self$endpoint,
        arms = self$arms,
        target_n_events = self$n,
        type = 'all'
      )

      milestone_time
    },

    print = function(){
      cat('Number of events (', self$endpoint, ') >= ', self$n, sep = '')

      if(!is.null(self$arms)){
        cat(' in arms <', paste0(self$arms, collapse = ', '), '>. ', sep = '')
      }else{
        cat(' in all arms in a trial. ')
      }

      cat('\n')
      invisible(self)
    }
  )
)


