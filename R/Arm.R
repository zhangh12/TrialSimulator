#' Class of Arm
#' @description
#' Create a class of arm.
#'
#' @docType class
#' @examples
#' risk <- data.frame(
#'   end_time = c(1, 10, 26.0, 52.0),
#'   piecewise_risk = c(1, 1.01, 0.381, 0.150) * exp(-3.01)
#' )
#'
#' pfs <- Endpoint$new(name = 'pfs', type='tte',
#' generator = PiecewiseConstantExponentialRNG,
#' risk = risk, endpoint_name = 'pfs')
#'
#' orr <- Endpoint$new(
#'   name = 'orr', type = 'binary',
#'   readout = c(orr = 2), generator = rbinom,
#'   size = 1, prob = .4)
#'
#' placebo <- Arm$new(
#'   name = 'pbo', description = 'Placebo arm')
#'
#' placebo$add_endpoints(pfs, orr)
#' placebo
#' placebo$get_endpoints()[[1]]$get_generator()(n = 1e3) |> head()
#' placebo$get_endpoints()[[2]]$get_name()
#'
#' @export
Arm <- R6::R6Class(
  'Arm',

  public = list(
    #' @description
    #' initialize an arm
    #' @param name name of arm
    #' @param description description of arm
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
