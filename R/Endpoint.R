#' Class of Endpoint
#' @description
#' Create a class of endpoint to specify its name, type and assign a random number
#' generator.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @import dplyr survival
#' @importFrom stats runif
#' @importFrom utils head tail
#' @import ggplot2
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
#' pfs <- Endpoint$new(name = 'pfs', type='tte',
#' generator = PiecewiseConstantExponentialRNG,
#' risk = risk, endpoint_name = 'pfs')
#' pfs$get_generator()
#'
#' ## Example 2. Generate continuous and binary endpoints using R's builtin
#' ## RNG functions, e.g. rnorm, rexp, rbinom, etc.
#' ep1 <- Endpoint$new(
#'          name = 'cd4', type = 'c', generator = rnorm, readout = c(cd4=1),
#'          mean = 1.2)
#' ep2 <- Endpoint$new(
#'          name = 'resp_time', type = 'c', generator = rexp, readout = c(resp_time=0),
#'          rate = 4.5)
#' ep3 <- Endpoint$new(
#'          name = 'orr', type = 'binary', readout = c(orr=3), generator = rbinom,
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
#'   name = 'pfs', type='tte',
#'   generator = PiecewiseConstantExponentialRNG,
#'   risk=risk1, endpoint_name = 'pfs')
#'
#' risk2 <- risk1
#' risk2$hazard_ratio <- c(1, 1, .6, .4)
#' ep2 <- TrialSimulator::Endpoint$new(
#'   name = 'pfs', type='tte',
#'   generator = PiecewiseConstantExponentialRNG,
#'   risk=risk2, endpoint_name = 'pfs')
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
    #' @param readout numeric vector with name to be the non-tte endpoint(s).
    #' \code{readout} should be specified for every non-tte endpoint. For
    #' example, \code{c(endpoint1 = 6, endpoint2 = 3)}.  If all
    #' endpoints are tte, \code{readout} can be \code{NULL}.
    #' @param ... optional arguments for \code{generator}.
    initialize = function(
      name,
      type = c('tte', 'continuous', 'binary'),
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
    readout = NULL,

    validate_arguments = function(
      name,
      type = c('tte', 'continuous', 'binary'),
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

