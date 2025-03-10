

CalendarTimeCondition <- R6::R6Class(
  'CalendarTimeCondition',

  inherit = Condition,

  public = list(
    time = NULL,
    arms = NULL,

    initialize = function(time, arms = NULL){

      stopifnot(is.numeric(time))
      stopifnot(length(time) == 1)
      stopifnot(time > 0)

      stopifnot(is.character(arms))

      self$time <- time
      self$arms <- arms
    },

    get_trigger_time = function(trial){
      event_time <- get_data_lock_time_by_calendar_time(
        calendar_time = self$time,
        arms = self$arms)

      event_time
    },

    print = function(){
      cat('Calendar time condition: ',
          'T >= ', self$time, '\n')

      invisible(self)
    }
  )
)
