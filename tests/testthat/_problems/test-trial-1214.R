# Extracted from test-trial.R:1214

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "TrialSimulator", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
ep <- endpoint(name = 'pfs', type = 'tte', generator = rexp, rate = log(2)/10)
pbo <- arm(name = 'pbo')
pbo$add_endpoints(ep)
ep <- endpoint(name = 'pfs', type = 'tte', generator = rexp, rate = log(2)/10)
trt3 <- arm(name = 'trt3')
trt3$add_endpoints(ep)
accrual_rate <- data.frame(end_time = c(7, Inf),
                             piecewise_rate = c(50, 80))
trial <- trial(name = '123', n_patients = 1000, duration = 20,
                 enroller = StaggeredRecruiter, accrual_rate = accrual_rate,
                 seed = 31415926, silent = TRUE)
trial$add_arms(sample_ratio = c(1, 1), pbo, trt3)
action_at_interim1 <- function(trial){
    locked_data <- trial$get_locked_data('interim')

    ep <- endpoint(name = 'pfs', type = 'tte', generator = rexp, rate = log(2)/12)
    trt1 <- arm(name = 'trt1')
    trt1$add_endpoints(ep)

    ep <- endpoint(name = 'pfs', type = 'tte', generator = rexp, rate = log(2)/14)
    trt2 <- arm(name = 'trt2')
    trt2$add_endpoints(ep)

    trial$add_arms(sample_ratio = 2, enforce = TRUE, trt1)
    trial$add_arms(sample_ratio = 3, enforce = TRUE, trt2)
  }
interim1 <- milestone(name = 'interim',
                        when = eventNumber(endpoint = 'pfs', n = 100),
                        action = action_at_interim1)
final <- milestone(name = 'final',
                     when = eventNumber(endpoint = 'pfs', n = 400))
listener <- listener(silent = TRUE)
listener$add_milestones(interim1, final)
controller <- controller(trial, listener)
controller$run(n = 10, silent = TRUE)
