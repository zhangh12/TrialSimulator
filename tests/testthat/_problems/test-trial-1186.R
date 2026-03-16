# Extracted from test-trial.R:1186

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
