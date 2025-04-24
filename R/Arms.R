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
Arms <- R6::R6Class(
  'Arms',

  public = list(
    #' @description
    #' initialize an arm
    #' @param name name of arm, which is the arm's label in generated data
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
        stopifnot(inherits(ep, 'Endpoints'))
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

      if(length(private$endpoints) == 0){
        return(0)
      }

      sapply(
        private$endpoints,
        function(ep){
          length(ep$get_name())
        }
      ) %>%
        sum()
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
    },

    #' @description
    #' return name of endpoints registered to the arm
    get_endpoints_name = function(){
      lapply(
        self$get_endpoints(),
        function(ep){
          ep$get_name()
        }
      ) %>%
        unlist()
    },

    #' @description
    #' print an arm
    print = function(){
      white_text_blue_bg <- "\033[37;44m"
      reset <- "\033[0m"  # Reset to default color
      logo <- '\u2695\u2695' ## stringi::stri_escape_unicode('⚕')

      cat(white_text_blue_bg, logo, 'Arm Name: ', self$get_name(), reset, '\n')
      cat(white_text_blue_bg, logo, 'Description: ', self$get_description(), reset, '\n')
      cat(white_text_blue_bg, logo, '# of Endpoints: ', self$get_number_endpoints(), reset, '\n')
      cat(white_text_blue_bg, logo, 'Registered Endpoints: ',
          paste0(self$get_endpoints_name(), collapse = ', '), reset, '\n')

      invisible(self)

    }
  ),

  private = list(
    name = NULL,
    description = NULL,
    endpoints = list()
  )
)
