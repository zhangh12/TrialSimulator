


ConditionCombiner <- R6::R6Class(
  'ConditionCombiner',

  inherit = Condition,

  public = list(
    conditions = NULL,
    operation = NULL, ## 'and' or 'or'

    initialize = function(conditions, operation){

      if(!is.list(conditions)){
        conditions <- list(conditions)
      }

      operation <- match.arg(operation, choices = c('and', 'or'))

      self$conditions <- conditions
      self$operation <- operation

    },

    get_trigger_time = function(trial){

      trigger_times <- lapply(self$conditions,
                              function(condition){
                                condition$get_trigger_time(trial)
                              })

      if(self$operation == 'and'){
        idx <- which.max(unlist(trigger_times))
      }else{
        idx <- which.min(unlist(trigger_times))
      }

      time <- trigger_times[[idx]]

      return(time)
    },

    print = function(){
      cat('Combined Condition (', self$operation, '):\n', sep = '')
      for (i in seq_along(self$conditions)) {
        output <- capture.output(self$conditions[[i]]$print())
        cat(paste0(output, collapse = '\n    '), '\n', sep = '')
      }
      invisible(self)
    }

  )
)
