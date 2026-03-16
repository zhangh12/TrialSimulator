# Extracted from test-trial.R:293

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "TrialSimulator", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
pfs <- endpoint(name = 'pfs', type = 'tte', generator = rexp, rate = log(2)/10)
os <- endpoint(name = 'os', type = 'tte', generator = rexp, rate = log(2)/17)
or <- endpoint(name = 'or', type = 'non-tte', readout = c(or = 1), generator = rnorm)
pbo <- arm(name = 'pbo')
pbo$add_endpoints(pfs, os, or)
pfs <- endpoint(name = 'pfs', type = 'tte', generator = rexp, rate = log(2)/12)
os <- endpoint(name = 'os', type = 'tte', generator = rexp, rate = log(2)/18)
or <- endpoint(name = 'or', type = 'non-tte', readout = c(or = 1), generator = rnorm)
trt <- arm(name = 'trt')
trt$add_endpoints(pfs, os, or)
accrual_rate <- data.frame(end_time = c(1, 2, 6, 12, Inf),
                             piecewise_rate = c(2, 8, 20, 25, 50))
trial <- trial(
    name = 'test', n_patients = 1000, duration = 30,
    enroller = StaggeredRecruiter, accrual_rate = accrual_rate,
    dropout = rweibull, shape = 1.32, scale = 114.4,
    seed = 808715505,
    silent = TRUE
  )
trial$add_arms(sample_ratio = c(1, 2), pbo, trt)
action_at_interim <- function(trial){
    trial$set_duration(duration = 40)
  }
interim <- milestone(name = 'interim',
                       when = eventNumber(endpoint = 'os', n = 150),
                       action = action_at_interim)
listener <- listener(silent = TRUE)
listener$add_milestones(interim)
controller <- controller(trial, listener)
controller$run(plot_event = FALSE, silent = TRUE)
