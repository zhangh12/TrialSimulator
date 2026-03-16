# Extracted from test-trial.R:1300

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "TrialSimulator", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
gen <- function(n, rate1, rate2){
    data.frame(ep11 = rexp(n, rate = rate1), ep11_event = 1,
               ep12 = rexp(n, rate = rate2), ep12_event = 1)
  }
ep1 <- endpoint(name = c('ep11', 'ep12'), type = c('tte', 'tte'), generator = gen, rate1 = log(2)/5, rate2 = log(2)/8)
ep2 <- endpoint(name = 'ep2', type = 'non-tte', readout = c(ep2 = 0), generator = rnorm)
pbo <- arm(name = 'pbo')
pbo$add_endpoints(ep1, ep2)
ep1 <- endpoint(name = c('ep11', 'ep12'), type = c('tte', 'tte'), generator = gen, rate1 = log(2)/6, rate2 = log(2)/10)
ep2 <- endpoint(name = 'ep2', type = 'non-tte', readout = c(ep2 = 0), generator = rnorm)
trt <- arm(name = 'trt')
trt$add_endpoints(ep1, ep2)
accrual_rate <- data.frame(end_time = c(1, 2, 6, 12, Inf),
                             piecewise_rate = c(2, 8, 20, 25, 50) * 1000)
trial <- trial(
    name = 'test', n_patients = 1000000, duration = 100,
    enroller = StaggeredRecruiter, accrual_rate = accrual_rate,
    seed = 31415926,
    silent = TRUE
  )
trial$add_arms(sample_ratio = c(1, 1), pbo, trt)
