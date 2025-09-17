

EnrollmentCountCondition <- R6::R6Class(
  'EnrollmentCountCondition',

  inherit = Condition,

  public = list(
    n = NULL,
    filter_conditions = NULL,
    arms = NULL,
    min_treatment_duration = 0,

    initialize = function(n, ..., arms = NULL, min_treatment_duration = 0){

      stopifnot(is.wholenumber(n))
      stopifnot(length(n) == 1)

      stopifnot(is.null(arms) || is.character(arms))

      stopifnot(is.numeric(min_treatment_duration))
      stopifnot(min_treatment_duration >= 0)

      self$n <- n
      self$filter_conditions <- enquos(...) #filter_conditions
      self$arms <- arms
      self$min_treatment_duration <- min_treatment_duration
    },

    get_trigger_time = function(trial){

      milestone_time <- trial$get_data_lock_time_by_event_number(
        endpoints = 'patient_id',
        arms = self$arms,
        target_n_events = self$n,
        !!!self$filter_conditions,
        type = 'all'
      )

      milestone_time <- milestone_time + self$min_treatment_duration
      milestone_time
    },

    print = function(){
      cat('Number of randomized patients >= ', self$n)

      if(!is.null(self$arms)){
        cat(' in arms <', paste0(self$arms, collapse = ', '), '> \n')
      }

      if(self$min_treatment_duration > 0){
        cat(' and all enrolled patients have been treated for <',
            self$min_treatment_duration, '> (unit time) \n')
      }

      if(length(self$filter_conditions) > 0) {
        cat(' with conditions: ')
        for(i in seq_along(self$filter_conditions)) {
          cat(deparse(self$filter_conditions[[i]]))
          if(i < length(self$filter_conditions)){
            cat(', ')
          }
        }
      }

      cat('\n')
      invisible(self)
    }
  )
)
