# Adaptive trial features
#
# Covers:
#   - update_sample_ratio: switching between permuted-block and sample()-based
#     randomization; balance checks at each stage of a response-adaptive design
#   - enrollment(min_treatment_duration): milestone triggered after all patients
#     have received treatment for a minimum duration
#   - add_arms(enforce): backward-compatible behavior of the enforce argument
#     when adding arms mid-trial
#   - update_generator: changing a data generator during a running trial and
#     verifying that only future patients are affected
#   - resize: increasing planned sample size at interim milestones

test_that('sample ratio can be updated to switch between permuted block and sample()', {

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
  set.seed(42)
  controller$run(n = 10, plot_event = FALSE, silen = TRUE)

  # p-values from chi-sq allocation checks; threshold 1e-4 to avoid flakiness
  expect_true(all(controller$get_output() %>% select(contains('stage')) > 1e-4))

})

test_that('milestone can be triggerd when all patients have received treatment for a while', {
  ep <- endpoint(name = 'x', type = 'tte', generator = rexp, rate = log(2)/10)
  pbo <- arm(name = 'pbo')
  pbo$add_endpoints(ep)
  ep <- endpoint(name = 'x', type = 'tte', generator = rexp, rate = log(2)/12)
  trt <- arm(name = 'trt')
  trt$add_endpoints(ep)

  accrual_rate <- data.frame(end_time = c(6, 12, 16, Inf),
                             piecewise_rate = c(20, 30, 40, 60))
  ## enrollment of 580 patients will be done by the 18th month
  ## if min treatment duration is 2 months, then triggering time would be 20

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

  expect_true(all(controller$get_output('milestone_time_<final>') - 19.98333 < .001))

})



test_that('enforce = TRUE is no longer needed in Trials$add_arms()', {

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
  op1 <- controller$get_output()

  ####################
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

  action_at_interim2 <- function(trial){
    locked_data <- trial$get_locked_data('interim')

    ep <- endpoint(name = 'pfs', type = 'tte', generator = rexp, rate = log(2)/12)
    trt1 <- arm(name = 'trt1')
    trt1$add_endpoints(ep)

    ep <- endpoint(name = 'pfs', type = 'tte', generator = rexp, rate = log(2)/14)
    trt2 <- arm(name = 'trt2')
    trt2$add_endpoints(ep)

    trial$add_arms(sample_ratio = 2, trt1)
    trial$add_arms(sample_ratio = 3, trt2)
  }

  interim2 <- milestone(name = 'interim',
                        when = eventNumber(endpoint = 'pfs', n = 100),
                        action = action_at_interim2)

  final <- milestone(name = 'final',
                     when = eventNumber(endpoint = 'pfs', n = 400))

  listener <- listener(silent = TRUE)
  listener$add_milestones(interim2, final)

  controller <- controller(trial, listener)
  controller$run(n = 10, silent = TRUE)
  op2 <- controller$get_output()

  expect_true(identical(op1, op2))

})


test_that('generator of endpoint can be updated in Trials$update_generator()', {


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

})



test_that('trial can be resized by Trials$resize()', {

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

  expect_true(all(controller$get_output('interim1_size') == 400))
  expect_true(all(controller$get_output('interim2_size') == 650))
  expect_true(all(controller$get_output('final_size') == 900))
  expect_true(all(controller$get_output('milestone_time_<final>') == 50))

})
