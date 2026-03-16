# Extracted from test-trial.R:1126

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "TrialSimulator", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
ep <- endpoint(name = 'os', type = 'non-tte', readout = c(os = 0), generator = rexp, rate = log(2)/10)
pbo <- arm(name = 'pbo')
pbo$add_endpoints(ep)
ep <- endpoint(name = 'os', type = 'non-tte', readout = c(os = 0), generator = rexp, rate = log(2)/11)
low <- arm(name = 'low')
low$add_endpoints(ep)
ep <- endpoint(name = 'os', type = 'non-tte', readout = c(os = 0), generator = rexp, rate = log(2)/12)
med <- arm(name = 'med')
med$add_endpoints(ep)
ep <- endpoint(name = 'os', type = 'non-tte', readout = c(os = 0), generator = rexp, rate = log(2)/13)
high <- arm(name = 'high')
high$add_endpoints(ep)
accrual_rate <- data.frame(end_time = c(10, Inf),
                             piecewise_rate = c(200, 200))
trial <- trial(
    name = 'test', n_patients = 1e4, duration = 52,
    enroller = StaggeredRecruiter, accrual_rate = accrual_rate,
    silent = TRUE
  )
trial$add_arms(sample_ratio = c(1, 1, 1, 1), pbo, low, med, high)
action1 <- function(trial){

    dat1 <- trial$get_locked_data('interim1')

    prob <- prop.table(trial$get_sample_ratio())

    trial$save(value = chisq.test(table(dat1$arm)[names(prob)], p = prob)$p.value,
               name = 'stage1')

    trial$update_sample_ratio(c('med', 'high'), c(2, 2))

  }
interim1 <- milestone(name = 'interim1',
                        when = eventNumber('os', n = 2000),
                        action = action1
  )
action2 <- function(trial){

    dat1 <- trial$get_locked_data('interim1')
    dat2 <- trial$get_locked_data('interim2') %>%
      filter(!(patient_id %in% dat1$patient_id))

    prob <- prop.table(trial$get_sample_ratio())

    trial$save(value = chisq.test(table(dat2$arm)[names(prob)], p = prob)$p.value,
               name = 'stage2')

    trial$update_sample_ratio(c('low', 'high'), c(.5, 2.5))

  }
interim2 <- milestone(name = 'interim2',
                        when = eventNumber('os', 4000),
                        action = action2
  )
action3 <- function(trial){

    dat2 <- trial$get_locked_data('interim2')
    dat3 <- trial$get_locked_data('interim3') %>%
      filter(!(patient_id %in% dat2$patient_id))

    prob <- prop.table(trial$get_sample_ratio())

    trial$save(value = chisq.test(table(dat3$arm)[names(prob)], p = prob)$p.value,
               name = 'stage3')

    trial$update_sample_ratio(c('low', 'high'), c(1.5, 3))
  }
interim3 <- milestone(name = 'interim3',
                        when = eventNumber('os', 6000),
                        action = action3
  )
action4 <- function(trial){

    dat3 <- trial$get_locked_data('interim3')
    dat4 <- trial$get_locked_data('final') %>%
      filter(!(patient_id %in% dat3$patient_id))

    prob <- prop.table(trial$get_sample_ratio())

    trial$save(value = chisq.test(table(dat4$arm)[names(prob)], p = prob)$p.value,
               name = 'stage4')

  }
final <- milestone(name = 'final', when = calendarTime(time = 52),
                     action = action4)
listener <- listener(silent = TRUE)
listener$add_milestones(interim1, interim2, interim3, final)
controller <- controller(trial, listener)
controller$run(n = 10, plot_event = FALSE, silen = TRUE)
