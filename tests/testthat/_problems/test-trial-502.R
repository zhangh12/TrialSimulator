# Extracted from test-trial.R:502

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "TrialSimulator", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
ep <- endpoint(name = 'ep', type = 'tte', generator = rexp, rate = .1)
pbo <- arm(name = 'pbo')
pbo$add_endpoints(ep)
accrual_rate <- data.frame(end_time = c(1, 2, 6, 12, Inf),
                             piecewise_rate = c(2, 8, 20, 25, 50))
trial <- trial(
    name = 'test', n_patients = 1000, duration = 40,
    enroller = StaggeredRecruiter, accrual_rate = accrual_rate,
    silent = TRUE
  )
trial$add_arms(sample_ratio = 1, pbo)
trial$save_custom_data(value = list(x = 1, y = 'a'), name = 'config')
final <- milestone(name = 'final',
                     action = function(trial) {
                       trial$get_custom_data('config')},
                     when = calendarTime(time = 40))
listener <- listener(silent = TRUE)
listener$add_milestones(final)
controller <- controller(trial, listener)
expect_no_error(controller$run(n = 10, plot_event = FALSE, silent = TRUE))
