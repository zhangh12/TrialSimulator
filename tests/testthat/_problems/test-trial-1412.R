# Extracted from test-trial.R:1412

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "TrialSimulator", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
ep <- endpoint(name = 'ep', type = 'non-tte', readout = c(ep = 0), generator = rnorm)
pbo <- arm(name = 'pbo')
pbo$add_endpoints(ep)
ep <- endpoint(name = 'ep', type = 'non-tte', readout = c(ep = 0), generator = rnorm, mean = .1)
trt <- arm(name = 'trt')
trt$add_endpoints(ep)
accrual_rate <- data.frame(end_time = c(10, Inf),
                             piecewise_rate = c(50, 50))
trial <- trial(
    name = 'test', n_patients = 500, duration = 50,
    enroller = StaggeredRecruiter, accrual_rate = accrual_rate,
    silent = TRUE
  )
trial$add_arms(sample_ratio = c(1, 2), pbo, trt)
interim1_action <- function(trial){

    locked_data <- trial$get_locked_data('interim1')
    trial$save(nrow(locked_data), name = 'interim1_size')
    trial$resize(700)

  }
interim1 <- milestone(name = 'interim1',
                        action = interim1_action,
                        when = enrollment(n = 400))
interim2_action <- function(trial){

    locked_data <- trial$get_locked_data('interim2')
    trial$save(nrow(locked_data), name = 'interim2_size')
    trial$resize(900)

  }
interim2 <- milestone(name = 'interim2',
                        action = interim2_action,
                        when = enrollment(n = 650))
final_action <- function(trial){

    locked_data <- trial$get_locked_data('final')
    trial$save(nrow(locked_data), name = 'final_size')

  }
final <- milestone(name = 'final',
                     action = final_action,
                     when = calendarTime(50))
listener <- listener(silent = TRUE)
listener$add_milestones(interim1, interim2, final)
controller <- controller(trial, listener)
controller$run(n = 10, plot_event = FALSE, silent = TRUE)
