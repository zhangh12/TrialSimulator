


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

      trigger_times <- sapply(self$conditions,
                              function(condition){
                                condition$get_trigger_time(trial)
                              })

      if(self$operation == 'and'){
        return(max(times))
      }else{
        return(min(times))
      }
    }

  )
)
