# Extracted from test-trial.R:400

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
trt1 <- arm(name = 'trt1')
trt1$add_endpoints(pfs, os, or)
pfs <- endpoint(name = 'pfs', type = 'tte', generator = rexp, rate = log(2)/13)
os <- endpoint(name = 'os', type = 'tte', generator = rexp, rate = log(2)/18.5)
or <- endpoint(name = 'or', type = 'non-tte', readout = c(or = 1), generator = rnorm)
trt2 <- arm(name = 'trt2')
trt2$add_endpoints(pfs, os, or)
accrual_rate <- data.frame(end_time = c(1, 2, 6, 12, Inf),
                             piecewise_rate = c(2, 8, 20, 25, 50))
trial <- trial(
    name = 'test', n_patients = 1000, duration = 40,
    enroller = StaggeredRecruiter, accrual_rate = accrual_rate,
    dropout = rweibull, shape = 1.32, scale = 114.4,
    silent = TRUE
  )
trial$add_arms(sample_ratio = c(1, 1, 2), pbo, trt1, trt2)
interim1 <- milestone(name = 'interim1',
                        when = eventNumber(endpoint = 'or', n = 200),
                        action = function(trial){trial$remove_arms('trt1')})
interim2 <- milestone(name = 'interim2',
                        when = eventNumber(endpoint = 'pfs', n = 240) &
                          eventNumber(endpoint = 'os', n = 170))
final <- milestone(name = 'final',
                 when = calendarTime(time = 40))
listener <- listener(silent = TRUE)
listener$add_milestones(interim1, interim2, final)
controller <- controller(trial, listener)
controller$run(n = 1, plot_event = FALSE, silent = TRUE)
