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
#' object. This happens for built-in random number generators, e.g., \code{rnorm},
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
#' x <- dfunc(1e3)
#' hist(x)
#'
#' @export
DynamicRNGFunction <- function(fn, ...) {

  # Capture fixed arguments
  fixed_args <- list(...)

  ## name of generator
  if(is.null(fixed_args$rng)){
    fn_name <- deparse(substitute(fn))
  }else{
    fn_name <- fixed_args$rng
    fixed_args$rng <- NULL
  }

  var_name <- fixed_args$var_name
  fixed_args$var_name <- NULL

  simplify <- ifelse(is.null(fixed_args$simplify), FALSE, fixed_args$simplify)
  fixed_args$simplify <- NULL

  type <- fixed_args$type
  fixed_args$type <- NULL

  readout <- fixed_args$readout
  fixed_args$readout <- NULL

  # Validate fixed arguments against fn
  unused_args <- setdiff(names(fixed_args), names(formals(fn)))
  if (length(unused_args) > 0) {
    warning('Some arguments in ... are not valid for the function <',
            fn_name, '>: \n',
         paste0(unused_args, collapse = ', '))
    for(arg in unused_args){
      fixed_args[[arg]] <- NULL
    }
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

    if(is.vector(dat)){
      if(simplify || is.null(type)){ ## useful when an Endpoint class is used to define enroller
        return(dat)
      }

      stopifnot(is.null(type) || (length(type) == 1))
      stopifnot(is.null(var_name) || (length(var_name) == 1))

      ## otherwise add column names
      if(type %in% 'tte'){ ## if tte, add event indicator
        dat <- data.frame(tte = dat, tte_event = 1)
        if(!is.null(var_name)){
          colnames(dat) <- paste0(var_name, c('', '_event'))
        }
      }else{

        stopifnot(is.null(readout) || (length(readout) == 1))
        dat <- data.frame(V1 = dat, V1_readout = unname(readout))
        if(!is.null(var_name)){
          colnames(dat) <- paste0(var_name, c('', '_readout'))
        }
      }
    }else{ ## user-defined rng function is received. It is user's responsibility
           ## to name all columns in data frame returned from their own function
      ## do nothing
      base_cols <- gsub('_event', '', names(dat))
      tte_cols <- base_cols[duplicated(base_cols)]
      non_tte_cols <- setdiff(base_cols, tte_cols)
      if(!setequal(non_tte_cols, names(readout))){
        stop('Readout may be missing for some endpoints, ',
             'or specified for some non-existing endpoints. ')
      }

      for(col in non_tte_cols){
        dat[, paste0(col, '_readout')] <- readout[col]
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

