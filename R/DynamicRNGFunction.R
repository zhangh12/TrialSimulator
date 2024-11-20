#' A wrapper of random number generator.
#' @param fn random number generator, e.g., rnorm, rchisq, etc. It can be
#' user-defined random number generator as well, e.g.,
#' PiecewiseConstantExponentialRNG
#' @param ... arguments for \code{fn}. Specifying invalid arguments can trigger error and
#' be stopped. There are three exceptions. (1) \code{rng} can be passed through
#' `...` to give true name of \code{fn}. This could be necessary as it may be
#' hard to parse it accurately in \code{DynamicRNGFunction}, or simply for a more
#' informative purpose in some scenarios. (2) \code{var_name} can be passed
#' through `...` to specify the name of generated variable. (3) \code{simplify}
#' can be set to FALSE to convert a vector into a one-column data frame in returned
#' object. This happens for builtin random number generators, e.g., \code{rnorm},
#' \code{rbinom}, etc. These three arguments will not be passed into \code{fn}.
#'
#' @return a function to generate random number based on `fn` and arguments in
#' `...`. Specified arguments will be fixed and cannot be changed when invoking
#' `DynamicRNGFunction(fn, ...)()`. For example, if `foo <- DynamicRNGFunction(rnorm, sd = 2)`,
#' then `foo(n = 100)` will always generate data from normal distribution of
#' variance 4. `foo(n = 100, sd = 1)` will trigger an error. However,
#' if an argument is not specified in `DynamicRNGFunction`, then it can be specified
#' later. For example, `foo(n = 100, mean = -1)` will generate data from N(-1, 4).
#'
#' @examples
#' # example code
#' dfunc <- DynamicRNGFunction(rnorm, sd = 3.2)
#' x <- dfunc(1e3)[, 1]
#' hist(x)
#'
#' @export
DynamicRNGFunction <- function(fn, ...) {

  # Capture fixed arguments
  fixed_args <- list(...)
  if(is.null(fixed_args$rng)){
    fn_name <- deparse(substitute(fn))
  }else{
    fn_name <- fixed_args$rng
    fixed_args$rng <- NULL
  }

  var_name <- fixed_args$var_name
  fixed_args$var_name <- NULL
  if(length(var_name) > 1){
    stop('var_name should be of length 1 in DynamicRNGFunction. ')
  }

  simplify <- ifelse(is.null(fixed_args$simplify), FALSE, fixed_args$simplify)
  fixed_args$simplify <- NULL

  # Validate fixed arguments against fn
  unused_args <- setdiff(names(fixed_args), names(formals(fn)))
  if (length(unused_args) > 0) {
    stop('Some arguments in ... are not valid for the function ', fn_name, ': \n',
         paste0(unused_args, collapse = ', '))
  }

  # Create the wrapper function
  wrapper <- function(...) {
    # Capture new arguments
    new_args <- list(...)

    # Prevent overriding fixed arguments
    if (any(names(new_args) %in% names(fixed_args))) {
      stop('Cannot override fixed arguments: ',
           paste(intersect(names(new_args), names(fixed_args)), collapse = ', '))
    }

    # Combine fixed and new arguments
    all_args <- c(fixed_args, new_args)

    # Call the original function
    dat <- do.call(fn, all_args)
    if(is.vector(dat) && !simplify){
      dat <- data.frame(V1 = dat)
      if(!is.null(var_name)){
        colnames(dat) <- var_name
      }
    }
    dat
  }

  # Add fixed arguments as an attribute for printing
  attr(wrapper, 'args') <- fixed_args
  attr(wrapper, 'function_name') <- fn_name

  # Define a custom print method for the wrapper
  class(wrapper) <- c('dynamic_rng_function', class(wrapper))
  wrapper
}

# Custom print method for objects of class 'dynamic_rng_function'
#' @export
print.dynamic_rng_function <- function(x, ...) {
  cat(attr(x, 'function_name'), ':\n')
  print(attr(x, 'args'))
}

