#' Class of Arm
#' @description
#' Create a class of arm.
#'
#' @docType class
#'
#' @examples
#' # Instead of using Arm$new, please use arm(), a user-friendly
#' # wrapper. See examples in ?arm
#'
#' @export
Arm <- R6::R6Class(
  'Arm',

  public = list(
    #' @description
    #' initialize an arm
    #' @param name name of arm
    #' @param description optional. Description of arm
    initialize = function(name, description = name){
      stopifnot(is.character(name))
      stopifnot(is.character(description))

      private$name <- name
      private$description <- description
      private$endpoints <- list()
    },

    #' @description
    #' add a list of endpoints to the arm
    #' @param ... one or more objects of class \code{Endpoint}
    add_endpoints = function(...){
      endpoint_list <- list(...)

      for(ep in endpoint_list){
        stopifnot(inherits(ep, 'Endpoint'))
        if(ep$get_uid() %in% names(private$endpoints)){
          stop('Endpoint <', ep$get_uid(), '> is already in the arm <',
               self$get_name(), '>. ')
        }
        private$endpoints[[ep$get_uid()]] <- ep
      }
    },

    #' @description
    #' return name of arm
    get_name = function(){
      private$name
    },

    #' @description
    #' return description of arm
    get_description = function(){
      private$description
    },

    #' @description
    #' return number of endpoints in the arm
    get_number_endpoints = function(){
      length(private$endpoints)
    },

    #' @description
    #' check if the arm has any endpoint. Return \code{TRUE} or \code{FALSE}.
    has_endpoint = function(){
      self$get_number_endpoints() > 0
    },

    #' @description
    #' return a list of endpoints in the arm
    get_endpoints = function(){
      private$endpoints
    }
  ),

  private = list(
    name = NULL,
    description = NULL,
    endpoints = list()
  )
)
