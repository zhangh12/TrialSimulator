

EnrollmentCountCondition <- R6::R6Class(
  'EnrollmentCountCondition',

  inherit = Condition,

  public = list(
    n = NULL,
    arms = NULL,

    initialize = function(n, arms = NULL){

      stopifnot(is.wholenumber(n))
      stopifnot(length(n) == 1)

      stopifnot(is.null(arms) || is.character(arms))

      self$n <- n
      self$arms <- arms
    },

    get_trigger_time = function(trial){

      milestone_time <- trial$get_data_lock_time_by_event_number(
        endpoints = 'patient_id',
        arms = self$arms,
        target_n_events = self$n,
        type = 'all'
      )

      milestone_time
    },

    print = function(){
      cat('Number of randomized patients >= ', self$n)

      if(!is.null(self$arms)){
        cat(' in arms <', paste0(self$arms, collapse = ', '), '>. ')
      }

      cat('\n')
      invisible(self)
    }
  )
)
