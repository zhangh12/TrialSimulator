#' A wrapper of random number generator fn.
#' @param fn random number generator, e.g., rnorm, rchisq, etc. It can be
#' user-defined random number generator as well, e.g.,
#' PiecewiseConstantExponentialRNG
#' @param ... arguments for fn. Specifying invalid arguments can trigger error and
#' be stopped.
#'
#' @return a function to generate random number based on `fn` and arguments in
#' `...`. Specified arguments will be fixed and cannot be changed when invoking
#' `DynamicFunction(fn, ...)()`. For example, if `foo <- DynamicFunction(rnorm, sd = 2)`,
#' then `foo(n = 100)` will always generate data from normal distribution of
#' variance 4. `foo(n = 100, sd = 1)` will trigger an error. However,
#' if an argument is not specified in `DynamicFunction`, then it can be specified
#' later. For example, `foo(n = 100, mean = -1)` will generate data from N(-1, 4).
#'
#' @examples
#' # example code
#' dfunc <- DynamicFunction(rnorm, sd = 3.2)
#' hist(dfunc()(1e3))
#'
#' @export
DynamicFunction <- function(fn, ...) {

  # Capture fixed arguments
  fixed_args <- list(...)
  if(is.null(fixed_args$rng)){
    fn_name <- deparse(substitute(fn))
  }else{
    fn_name <- fixed_args$rng
    fixed_args$rng <- NULL
  }

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
    do.call(fn, all_args)
  }

  # Add fixed arguments as an attribute for printing
  attr(wrapper, 'args') <- fixed_args
  attr(wrapper, 'function_name') <- fn_name

  # Define a custom print method for the wrapper
  class(wrapper) <- c('dynamic_function', class(wrapper))
  wrapper
}

# Custom print method for objects of class 'dynamic_function'
#' @export
print.dynamic_function <- function(x, ...) {
  cat(attr(x, 'function_name'), ':\n')
  print(attr(x, 'args'))
}

