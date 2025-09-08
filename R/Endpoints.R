#' Class of Endpoint
#' @description
#' Create a class of endpoint to specify its name, type and assign a random number
#' generator.
#'
#' Public methods in this R6 class are used in developing
#' this package. Thus, I have to export the whole R6 class which exposures all
#' public methods. However, none of the public methods is useful to end users.
#'
#' \itemize{
#' \item \code{$print()}
#' }
#'
#' @docType class
#' @importFrom R6 R6Class
#' @import dplyr survival
#' @importFrom stats runif
#' @importFrom utils head tail
#' @import ggplot2
#'
#' @examples
#' # Instead of using Endpoint$new, please use endpoint(), a user-friendly
#' # wrapper. See examples in ?endpoint.
#'
#' @export
Endpoints <- R6::R6Class(
  'Endpoints',

  public = list(

    #' @description
    #' initialize an endpoint
    #' @param name character vector. Name(s) of endpoint(s)
    #' @param type character vector. Type(s) of endpoint(s). It supports
    #' \code{"tte"} for time-to-event endpoints, and \code{"non-tte"} for
    #' all other types of endpoints (e.g., continous, binary, categorical,
    #' or repeated measurement. \code{TrialSimulator} will do some verification if
    #' an endpoint is of type \code{"tte"}. However, no special
    #' manipulation is done for non-tte endpoints.
    #' @param generator a RNG function. Its first argument must be `n`, number of
    #' patients. It must return a data frame of `n` rows. It support all
    #' built-in random number generators in \code{stats}, e.g., \code{stats::rnorm},
    #' \code{stats::rexp}, etc. that with \code{n} as the argument for number of
    #' observations. \code{generator} could be any custom functions as long as
    #' (1) its first argument is \code{n}; and (2) it returns a vector of
    #' length \code{n} or a data frame of \code{n} rows. Custom random number
    #' generator can return data of more than one endpoint. This is useful
    #' when users need to simulate correlated endpoints. The column names of
    #' returned data frame should match to \code{name} exactly. If an endpoint
    #' is of type \code{"tte"}, the custom \code{generator} should also return
    #' a column as event indicator. For example, if \code{"pfs"} is \code{"tte"},
    #' then custom \code{generator} should return at least two columns
    #' \code{"pfs"} and \code{"pfs_event"}. Usually \code{pfs_event} can be
    #' all 1s if no censoring. Censoring can be specified later when defining
    #' the \code{Trial} through argument \code{dropout}. See \code{?Trial}.
    #' Note that if covariates, e.g., biomarker, subgroup, are needed in
    #' generating and analyzing trial data, they can be defined as
    #' \code{Endpoint} as well.
    #' @param readout numeric vector with name to be the non-tte endpoint(s).
    #' \code{readout} should be specified for every non-tte endpoint. For
    #' example, \code{c(endpoint1 = 6, endpoint2 = 3)}.  If all
    #' endpoints are tte, \code{readout} can be \code{NULL}.
    #' @param ... optional arguments for \code{generator}.
    initialize = function(
      name,
      type = c('tte', 'non-tte'),
      readout = NULL,
      generator,
      ...
    ){

      private$validate_arguments(name, type, readout, generator, ...)
      private$name <- name
      private$uid <- paste0(name, collapse = '/')

      type <- match.arg(type, several.ok = TRUE)
      if(length(type) == 1 && length(name) > 1){
        type <- rep(type, length(name))
      }
      private$type <- type

      private$readout <- readout

      if(!is.null(generator)){
        private$generator <- DynamicRNGFunction(
          generator, rng = deparse(substitute(generator)),
          var_name = self$get_name(),
          type = self$get_type(),
          readout = self$get_readout(), ...)
        ## ignore all other arguments in ... if generator is provided
        return()
      }

      stop('Fail to create an endpoint because of lack of RNG generator.')

      ## do something else
    },

    #' @description
    #' test random number generator of the endpoints. It returns an example
    #' dataset.
    #' @param n integer. Number of random numbers generated from the generator.
    test_generator = function(n = 1e3){
      stopifnot(is.numeric(n) && is.wholenumber(n) && length(n) == 1)
      self$get_generator()(n)
    },

    #' @description
    #' return random number generator of an endpoint
    get_generator = function(){
      private$generator
    },

    #' @description
    #' return readout function
    get_readout = function(){
      private$readout
    },

    #' @description
    #' return uid
    get_uid = function(){
      private$uid
    },

    #' @description
    #' return endpoints' name
    get_name = function(){
      private$name
    },

    #' @description
    #' return endpoints' type
    get_type = function(){
      private$type
    },

    #' @description
    #' print an endpoint object
    #'
    #' @param categorical_vars categorical_vars character. Vector of categorical variables. This can
    #' be used to specify variables with limited distinct values as categorical
    #' variables in summary.
    print = function(categorical_vars = NULL){
      white_text_blue_bg <- "" ## "\033[37;44m"
      reset <- "" ## "\033[0m"  # Reset to default color
      logo <- '\u2695\u2695' ## stringi::stri_escape_unicode('⚕')

      # cat(white_text_blue_bg, logo, 'Endpoint Name: ', paste0(self$get_name(), collapse = ', '), reset, '\n')
      # cat(white_text_blue_bg, logo, '# of Endpoints: ', length(self$get_name()), reset, '\n')

      title <- paste0('Endpoints (', length(self$get_name()), ')')
      sub_title <- paste0(self$get_name(), collapse = ', ')

      dat <- self$test_generator(n = 1e4)
      vars <- self$get_name()
      event_vars <- intersect(paste0(vars, '_event'), names(dat))
      tte_vars <- gsub('_event$', '', event_vars)
      exclude_vars <- grep('_readout$', names(dat), value = TRUE)

      if(requireNamespace("knitr", quietly = TRUE) &&
         isTRUE(getOption('knitr.in.progress'))) {
        summary_html <- summarizeDataFrame(dat, exclude_vars = exclude_vars,
                                           tte_vars = tte_vars, event_vars = event_vars,
                                           categorical_vars = categorical_vars,
                                           title = title, sub_title = sub_title)

        temp_file <- tempfile(fileext = ".html")
        writeLines(summary_html, temp_file, useBytes = TRUE)

        if(requireNamespace("htmltools", quietly = TRUE)) {
          iframe_html <- htmltools::tags$iframe(
            src = paste0("data:text/html;charset=utf-8;base64,", base64enc::base64encode(temp_file)),
            width = "100%",
            height = "500px",
            style = "border: 1px solid #ccc; border-radius: 4px;"
          )
          cat(as.character(iframe_html))
        } else {
          file_content <- paste(readLines(temp_file), collapse = "\n")
          file_b64 <- base64enc::base64encode(charToRaw(file_content))
          cat('<iframe src="data:text/html;charset=utf-8;base64,', file_b64,
              '" width="100%" height="500px" style="border: 1px solid #ccc;"></iframe>')
        }
      } else {
        summarizeDataFrame(dat, exclude_vars = exclude_vars,
                           tte_vars = tte_vars, event_vars = event_vars,
                           categorical_vars = categorical_vars,
                           title = title, sub_title = sub_title)
      }

      invisible(self)

    }
  ),

  private = list(
    ## @field uid a character for unique ID of an Endpoint object. This is needed
    ## because an object can probably consist of more than one endpoints, i.e.,
    ## \code{name} has length greater than 1.
    ## @field name a character vector for name(s) of endpoint(s), e.g., \code{"pfs"}, \code{"ORR"}.
    ## @field type a character vector for type(s) of endpoint(s), e.g., \code{"tte"},
    ## \code{"non-tte"}.
    ## @field generator a user-defined RNG function. Its first argument must be \code{n},
    ## number of patients. It must return a data frame of \code{n} rows. When `generator`
    ## is \code{NULL}, one can create one endpoint at a time, i.e., both \code{name} and \code{type}
    ## are of length 1. Otherwise, one can create multiple endpoints at a time as
    ## long as \code{generator} is capable to generate data for the endpoints.
    uid = NULL,
    name = NULL,
    type = NULL,
    generator = NULL,
    readout = NULL,

    validate_arguments = function(
      name,
      type = c('tte', 'non-tte'),
      readout,
      generator,
      ...
    ){
      stopifnot(is.character(name))
      stopifnot(is.character(type))
      stopifnot((length(name) == length(type)) || (length(type) == 1))
      type <- match.arg(type, several.ok = TRUE)

      stopifnot(!is.null(generator))

      if(is.null(readout)){
        if(any(type != 'tte')){
          stop('Readout cannot be NULL for <',
               paste0(name[type != 'tte'], collapse = ', '),
               '>. ')
        }
      }else{

        stopifnot(is.numeric(readout) && all(readout >= 0))
        if(is.null(names(readout)) || ('' %in% names(readout))){
          stop('readout should be a named numeric vector, ',
               'e.g. c(os = 1, pfs = 2). ')
        }

        if(!all(names(readout) %in% name[type != 'tte'])){
          stop('Readout is set for unknown or time-to-event endpoint(s) <',
               paste0(setdiff(names(readout), name[type != 'tte']), collapse = ', '),
               '>. ')
        }

        if(!setequal(names(readout), name[type != 'tte'])){
          stop('Readout is missing for endpoint(s) <',
               paste0(setdiff(name[type != 'tte'], names(readout)), collapse = ', '),
               '>. ')
        }
      }

      stopifnot(is.function(generator))

      # Check that the first argument of generator is "n"
      arg_names <- names(formals(generator))
      if (length(arg_names) == 0 || arg_names[1] != "n") {
        stop("The first argument of generator must be 'n'.")
      }

      n_ <- 2
      generator_ <- DynamicRNGFunction(generator,
                                       rng = deparse(substitute(generator)),
                                       var_name = name,
                                       type = type,
                                       readout = readout, ...)
      example_data <- generator_(n = n_)

      if(!is.data.frame(example_data) && !is.vector(example_data)){
        stop('generator must return a data frame (for multiple endpoints or a TTE) or a vector.')
      }

      if(is.vector(example_data)){
        example_data <- data.frame(v1 = example_data) %>%
          rename(!!name := .data$v1)
      }

      if(!all(name %in% colnames(example_data))){
        stop('generator must return data for every endpoints in \'name\'.')
      }

      if(nrow(example_data) != n_){
        stop('\'n\' in generator does not work correctly.')
      }
    }
  )
)

