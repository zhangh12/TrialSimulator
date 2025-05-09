% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DynamicTriggerConditionFunction.R
\name{DynamicTriggerConditionFunction}
\alias{DynamicTriggerConditionFunction}
\title{A wrapper of event trigger condition function}
\usage{
DynamicTriggerConditionFunction(fn, ...)
}
\arguments{
\item{fn}{a function.}

\item{...}{arguments for \code{fn}.}
}
\value{
a function to determine whether the trigger condition is met,
based on `fn` and arguments in
`...`. Specified arguments will be fixed and cannot be changed when invoking
`DynamicTriggerConditionFunction(fn, ...)()`. For example,
if `foo <- DynamicTriggerConditionFunction(TriggerByEventNumbers, `
`endpoints = c('pfs', 'os'), target_n_events = c(150, 80), type = 'any')`,
then `foo()` will always trigger action when collecting either 150 PFS events,
or 80 OS events. `foo(target_n_events = c(150, 100))` will trigger an error.
However, if an argument is not specified in `DynamicTriggerConditionFunction`,
then it can be specified later.
}
\description{
A wrapper of event trigger condition function
}
\examples{
##
}
