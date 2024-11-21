#' Class of Endpoint
#' @description
#' Create a class of endpoint to specify its name, type and assign a random number
#' generator.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @importFrom stringr str_glue
#' @import dplyr survival
#' @importFrom stats runif
#'
#' @examples
#' set.seed(12345)
#' ## Example 1. Generate a time-to-event endpoint.
#' ## Two columns are returned, one for time, one for event (1/0, 0 for
#  ## censoring)
#' ## A builtin RNG function is used to handle piecewise constant exponential
#' ## distribution
#' risk <- data.frame(
#'   end_time = c(1, 10, 26.0, 52.0),
#'   piecewise_risk = c(1, 1.01, 0.381, 0.150) * exp(-3.01)
#' )
#'
#' pfs <- Endpoint$new(name = 'pfs', type='tte', method='piecewise_const_exp',
#' risk = risk)
#' pfs$get_generator()
#'
#' ## Example 2. Generate continuous and binary endpoints using R's builtin
#' ## RNG functions, e.g. rnorm, rexp, rbinom, etc.
#' ep1 <- Endpoint$new(
#'          name = 'cd4', type = 'c', generator = rnorm, mean = 1.2)
#' ep2 <- Endpoint$new(
#'          name = 'resp_time', type = 'c', generator = rexp, rate = 4.5)
#' ep3 <- Endpoint$new(
#'          name = 'orr', type = 'binary', generator = rbinom,
#'          size = 1, prob = .4)
#'
#' mean(ep1$get_generator()(1e4)[, 1]) # compared to 1.2
#' sd(ep1$get_generator()(1e4)[, 1]) # compared to 1.0
#'
#' log(2) / median(ep2$get_generator()(1e4)[, 1]) # compared to 4.5
#'
#' mean(ep3$get_generator()(1e4)[, 1]) # compared to 0.4
#'
#' ## An example of piecewise constant exponential random number generator
#' ## Baseline hazards are piecewise constant
#' ## Hazard ratios are piecewise constant, resulting a delayed effect.
#'
#' run <- TRUE
#'
#' if (!requireNamespace("survminer", quietly = TRUE)) {
#'   run <- FALSE
#'   message("Please install 'survminer' to run this example.")
#' }
#'
#' if (!requireNamespace("survival", quietly = TRUE)) {
#'   run <- FALSE
#'   message("Please install 'survival' to run this example.")
#' }
#'
#' if(run){
#' risk1 <- risk
#' ep1 <- TrialSimulator::Endpoint$new(
#'   name = 'pfs', type='tte', method='piecewise_const_exp', risk=risk1)
#'
#' risk2 <- risk1
#' risk2$odds_ratio <- c(1, 1, .6, .4)
#' ep2 <- TrialSimulator::Endpoint$new(
#'   name = 'pfs', type='tte', method='piecewise_const_exp', risk=risk2)
#'
#' n <- 1000
#' tte <- rbind(ep1$get_generator()(n), ep2$get_generator()(n))
#' arm <- rep(0:1, each = n)
#' dat <- data.frame(tte, arm)
#' sfit <- survival::survfit(
#'   survival::Surv(time = pfs, event = pfs_event) ~ arm, dat)
#'
#' survminer::ggsurvplot(sfit,
#'            data = dat,
#'            pval = TRUE,  # Show p-value
#'            conf.int = TRUE,  # Show confidence intervals
#'            risk.table = TRUE,  # Add risk table
#'            palette = c("blue", "red"))
#'
#' }
#' @export
Endpoint <- R6::R6Class(
  'Endpoint',

  public = list(

    #' @description
    #' initialize an endpoint
    #' @param name name(s) of endpoint(s)
    #' @param type type(s) of endpoint(s)
    #' @param generator a RNG function. Its first argument must be `n`, number of
    #' patients. It must return a data frame of `n` rows. It can be optional if
    #'
    #' @param ... Other supported arguments.
    #' It includes `method` a character with possible values as follow
    # (1) piecewise_const_exp: piecewise constant exponential distribution
    initialize = function(
      name,
      type = c('tte', 'continuous', 'binary'),
      generator = NULL,
      ...
    ){

      private$validate_arguments(name, type, generator, ...)
      private$name <- name
      private$uid <- paste0(name, collapse = '/')

      type <- match.arg(type, several.ok = TRUE)
      if(length(type) == 1 && length(name) > 1){
        type <- rep(type, length(name))
      }
      private$type <- type

      if(!is.null(generator)){
        private$generator <- DynamicRNGFunction(
          generator, rng = deparse(substitute(generator)),
          var_name = self$get_name(), ...)
        ## ignore all other arguments in ... if generator is provided
        return()
      }

      args <- list(...)
      method <- args$method

      if('piecewise_const_exp' %in% method){
        private$generator <-
          DynamicRNGFunction(
            PiecewiseConstantExponentialRNG,
            risk = args$risk,
            endpoint_name = name
          )
        return()
      }

      stop('Fail to create an endpoint because of lack of RNG generator.')

      ## do something else
    },

    #' @description
    #' test random number generator of the endpoints. It returns an example
    #' dataset.
    test_generator = function(n = 1e3){
      self$get_generator()(n)
    },

    #' @description
    #' return random number generator of an endpoint
    get_generator = function(){
      private$generator
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
    }
  ),

  private = list(
    ## @field uid a character for unique ID of an Endpoint object. This is needed
    ## because an object can probably consist of more than one endpoints, i.e.,
    ## \code{name} has length greater than 1.
    ## @field name a character vector for name(s) of endpoint(s), e.g., \code{"pfs"}, \code{"ORR"}.
    ## @field type a character vector for type(s) of endpoint(s), e.g., \code{"tte"},
    ## \code{"continuous"}, \code{"binary"}. More types may be supported in the future.
    ## @field generator a user-defined RNG function. Its first argument must be \code{n},
    ## number of patients. It must return a data frame of \code{n} rows. When `generator`
    ## is \code{NULL}, one can create one endpoint at a time, i.e., both \code{name} and \code{type}
    ## are of length 1. Otherwise, one can create multiple endpoints at a time as
    ## long as \code{generator} is capable to generate data for the endpoints.
    uid = NULL,
    name = NULL,
    type = NULL,
    generator = NULL,

    validate_arguments = function(
      name,
      type = c('tte', 'continuous', 'binary'),
      generator = NULL,
      ...
    ){
      stopifnot(is.character(name))
      stopifnot(is.character(type))
      stopifnot((length(name) == length(type)) || (length(type) == 1))
      type <- match.arg(type, several.ok = TRUE)

      if(!is.null(generator)){
        stopifnot(is.function(generator))

        # Check that the first argument of generator is "n"
        arg_names <- names(formals(generator))
        if (length(arg_names) == 0 || arg_names[1] != "n") {
          stop("The first argument of generator must be 'n'.")
        }

        n_ <- 2
        generator_ <- DynamicRNGFunction(
          generator, ...)
        example_data <- generator_(n = n_)

        if(!is.data.frame(example_data) && !is.vector(example_data)){
          stop('generator must return a data frame (for multiple endpoints or a TTE) or a vector.')
        }

        if(is.vector(example_data)){
          example_data <- data.frame(v1 = example_data) %>%
            rename(!!name := .data$v1)
        }

        if(ncol(example_data) != length(name)){
          stop('generator must return data for every endpoints in \'name\'.')
        }

        if(nrow(example_data) != n_){
          stop('\'n\' in generator does not work correctly.')
        }
        ## ignore all other arguments in ... if generator is provided
        return()
      }

      ## if generator is not provided
      if(length(name) != 1){
        stop('Only one endpoint can be specified if \'generator\' is not specified.')
      }

      args <- list(...)
      method <- args$method
      if(is.null(method)){
        stop('\'method\' cannot be NULL if generator is not specified.')
      }

      valid_methods <- c('piecewise_const_exp', 'TBD')
      if(!(method %in% valid_methods)){
        stop(str_glue("Valid methods are \"{paste0(valid_methods, collapse = '\", \"')}\"."))
      }

      if('piecewise_const_exp' %in% method){
        if(is.null(args$risk)){
          stop('\'risk\' should be provided if method = \'piecewise_const_exp\'.')
        }
      }

      return()
    }
  )
)

