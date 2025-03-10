

Condition <- R6::R6Class(
  'Condition',

  public = list(
    initialize = function() {},

    get_trigger_time = function(trial){
      stop('Abstract method: get_trigger_time must be implemented in subclass. ')
    },

    print = function(){
      cat('Condition object\n')
      invisible(self)
    }
  )
)

## operator overloading methods
Condition$set('public',
              '|',
              function(other){
                ConditionCombiner$new(list(self, other), operation = 'or')
              })

Condition$set('public',
              '&',
              function(other){
                ConditionCombiner$new(list(self, other), operation = 'and')
              })

## operator overloading for condition objects
#' @export
`|.Condition` <- function(cond1, cond2){
  cond1$`|`(cond2)
}

#' @export
`&.Condition` <- function(cond1, cond2){
  cond1$`&`(cond2)
}


