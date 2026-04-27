## Runtime toggle for the C++ lock-time fast path.
##
## When TRUE (the default), Trials$get_data_lock_time_by_event_number() and
## Trials$get_data_lock_time_by_enrollment() use the Rcpp helpers
## (find_event_lock_time_cpp, find_readout_lock_time_cpp,
## find_enrollment_lock_time_cpp) on the no-filter common path.
##
## When FALSE, both methods fall back to the original pure-R implementation
## that builds intermediate event-count data.frames via get_event_tables().
## This is provided as an escape hatch: it lets users disable C++ globally
## without re-installing, and lets future regression checks compare R and C++
## outputs side-by-side from a single installed package.
##
## Set with:  options(trialsimulator.use_cpp = FALSE)

.use_cpp_lock_time <- function(){
  isTRUE(getOption('trialsimulator.use_cpp', default = TRUE))
}
