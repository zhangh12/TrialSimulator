# Extracted from test-trial.R:1161

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "TrialSimulator", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
ep <- endpoint(name = 'x', type = 'tte', generator = rexp, rate = log(2)/10)
pbo <- arm(name = 'pbo')
pbo$add_endpoints(ep)
ep <- endpoint(name = 'x', type = 'tte', generator = rexp, rate = log(2)/12)
trt <- arm(name = 'trt')
trt$add_endpoints(ep)
accrual_rate <- data.frame(end_time = c(6, 12, 16, Inf),
                             piecewise_rate = c(20, 30, 40, 60))
trial <- trial(
    name = 'test', n_patients = 580, duration = 25,
    enroller = StaggeredRecruiter, accrual_rate = accrual_rate,
    dropout = rexp, rate = -log(1 - .08)/2,
    silent = TRUE
  )
trial$add_arms(sample_ratio = c(1, 2), pbo, trt)
final <- milestone(name = 'final',
                     when = enrollment(n = 580, min_treatment_duration = 2))
listener <- listener(silent = TRUE)
listener$add_milestones(final)
controller <- controller(trial, listener)
controller$run(n = 10, plot_event = FALSE, silen = TRUE)
