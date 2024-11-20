#' A wrapper of event trigger condition function
#'
#' @return a function to determine whether the trigger condition is met,
#' based on `fn` and arguments in
#' `...`. Specified arguments will be fixed and cannot be changed when invoking
#' `DynamicTriggerConditionFunction(fn, ...)()`. For example,
#' if `foo <- DynamicTriggerConditionFunction(TriggerByEventNumbers, `
#' `endpoints = c('pfs', 'os'), target_n_events = c(150, 80), type = 'any')`,
#' then `foo()` will always trigger action when collecting either 150 PFS events,
#' or 80 OS events. `foo(target_n_events = c(150, 100))` will trigger an error.
#' However, if an argument is not specified in `DynamicTriggerConditionFunction`,
#' then it can be specified later.
#'
#' @examples
#' ##
#' @export
DynamicTriggerConditionFunction <- function(fn, ...) {

  # Capture fixed arguments
  fixed_args <- list(...)
  if(is.null(fixed_args$trigger_condition_name)){
    trigger_condition_name <- deparse(substitute(fn))
  }else{
    trigger_condition_name <- fixed_args$trigger_condition_name
    fixed_args$trigger_condition_name <- NULL
  }

  # Validate fixed arguments against fn
  unused_args <- setdiff(names(fixed_args), names(formals(fn)))
  if (length(unused_args) > 0) {
    stop('Some arguments in ... are not valid for the trigger condition function ',
         trigger_condition_name, ': \n',
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
  attr(wrapper, 'trigger_condition_name') <- trigger_condition_name

  # Define a custom print method for the wrapper
  class(wrapper) <- c('dynamic_trigger_condition_function', class(wrapper))
  wrapper
}

# Custom print method for objects of class 'dynamic_trigger_condition_function'
#' @export
print.dynamic_trigger_condition_function <- function(x, ...) {
  cat(attr(x, 'trigger_condition_name'), ':\n')
  print(attr(x, 'args'))
}

