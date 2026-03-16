# Extracted from test-trial.R:1346

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
action_at_interim <- function(trial){

    locked_data <- trial$get_locked_data('interim')

    trial$update_generator(arm_name = 'pbo', endpoint_name = c('ep11', 'ep12'), generator = gen, rate1 = log(2)/2, rate2 = log(2)/3)

    fit11 <- survival::survfit(Surv(ep11, ep11_event) ~ arm, data = locked_data)
    medians11 <- summary(fit11)$table[, 'median']

    expect_equal(unname(medians11['arm=pbo']), 5, tolerance = .1)
    expect_equal(unname(medians11['arm=trt']), 6, tolerance = .1)

    fit12 <- survival::survfit(Surv(ep12, ep12_event) ~ arm, data = locked_data)
    medians12 <- summary(fit12)$table[, 'median']

    expect_equal(unname(medians12['arm=pbo']), 8, tolerance = .1)
    expect_equal(unname(medians12['arm=trt']), 10, tolerance = .1)

  }
action_at_final <- function(trial){

    locked_data <- trial$get_locked_data('final') %>% filter(patient_id > 500000)
    fit11 <- survival::survfit(Surv(ep11, ep11_event) ~ arm, data = locked_data)
    medians11 <- summary(fit11)$table[, 'median']

    expect_equal(unname(medians11['arm=pbo']), 2, tolerance = .1)
    expect_equal(unname(medians11['arm=trt']), 6, tolerance = .1)

    fit12 <- survival::survfit(Surv(ep12, ep12_event) ~ arm, data = locked_data)
    medians12 <- summary(fit12)$table[, 'median']

    expect_equal(unname(medians12['arm=pbo']), 3, tolerance = .1)
    expect_equal(unname(medians12['arm=trt']), 10, tolerance = .1)

  }
interim <- milestone(name = 'interim', when = enrollment(n = 500000), action = action_at_interim)
final <- milestone(name = 'final', when = enrollment(n = 1000000, min_treatment_duration = 10), action = action_at_final)
listener <- listener(silent = TRUE)
listener$add_milestones(interim, final)
controller <- controller(trial, listener)
controller$run(n = 1, plot_event = FALSE, silent = TRUE)
