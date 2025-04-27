
test_that('trial event timing and endpoint event count work as expected', {

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
    name = 'test', n_patients = 1000, duration = 40,
    enroller = StaggeredRecruiter, accrual_rate = accrual_rate,
    dropout = rweibull, shape = 1.32, scale = 114.4,
    silent = TRUE
  )

  trial$add_arms(sample_ratio = c(1, 2), pbo, trt)

  interim1 <- event(name = 'interim1',
                    trigger_condition =
                      eventNumber(endpoint = 'or', n = 200))

  interim2 <- event(name = 'interim2',
                    trigger_condition =
                      eventNumber(endpoint = 'pfs', n = 240) &
                      eventNumber(endpoint = 'os', n = 170))

  final <- event(name = 'final',
                 trigger_condition = calendarTime(time = 40))

  listener <- listener(silent = TRUE)
  listener$add_events(interim1, interim2, final)

  controller <- controller(trial, listener)
  controller$run(n = 1, plot_event = FALSE, silent = TRUE)

  ## check data

  dat1 <- trial$get_locked_data('interim1')
  dat2 <- trial$get_locked_data('interim2')
  dat3 <- trial$get_locked_data('final')

  time1 <- trial$get_event_time('interim1')
  time2 <- trial$get_event_time('interim2')


  expect_equal(sum(!is.na(dat1$or)), 200)

  expect_equal(
    dat2 %>%
      filter(enroll_time + or_readout <= time1 &
               enroll_time + or_readout < dropout_time &
               !is.na(or)) %>%
      nrow(),
    200)

  expect_equal(
    dat3 %>%
      filter(enroll_time + or_readout <= time1 &
               enroll_time + or_readout < dropout_time &
               !is.na(or)) %>%
      nrow(),
    200)

  expect_true(sum(dat2$pfs_event %in% 1) == 240 || sum(dat2$os_event %in% 1) == 170)

  expect_true(sum(dat2$pfs_event %in% 1) >= 240 && sum(dat2$os_event %in% 1) >= 170)

  n_pfs_events <- dat3 %>%
    filter(enroll_time + pfs <= time2 &
             enroll_time + pfs < dropout_time &
             pfs_event %in% 1) %>% nrow()

  n_os_events <- dat3 %>%
    filter(enroll_time + os <= time2 &
             enroll_time + os < dropout_time &
             os_event %in% 1) %>% nrow()

  expect_true(n_pfs_events == 240 || n_os_events == 170)
  expect_true(n_pfs_events >= 240 && n_os_events >= 170)

})

test_that('endpoint event counts work as expected when duration is adapted', {

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

  action_at_interim <- function(trial, event_name){
    trial$set_duration(duration = 40)
  }

  interim <- event(name = 'interim',
                   trigger_condition =
                     eventNumber(endpoint = 'os', n = 150),
                   action = action_at_interim)

  listener <- listener(silent = TRUE)
  listener$add_events(interim)

  controller <- controller(trial, listener)
  controller$run(plot_event = FALSE, silent = TRUE)

  dat1_ <- trial$get_locked_data('interim')

  expect_equal(sum(dat1_$os_event %in% 1), 150)

  final <- event(name = 'final',
                 trigger_condition =
                   eventNumber(endpoint = 'os', n = 400) |
                   calendarTime(time = 40))


  listener$add_events(final)
  controller$run(plot_event = FALSE, silent = TRUE)

  dat1 <- trial$get_locked_data('interim')
  dat2 <- trial$get_locked_data('final')

  expect_identical(dat1_, dat1)

  expect_true(sum(dat2$os_event %in% 1) == 400 ||
                all((dat2$enroll_time + dat2$os_event)[dat2$os_event %in% 1] <= 40))

  tol <- 1e-6
  expect_true(
    dat2 %>%
      filter(patient_id %in% dat1$patient_id) %>%
      summarise(os = all((enroll_time + os < 30 &
                            enroll_time + os < dropout_time - tol) == (os_event == 1)),
                pfs = all((enroll_time + pfs < 30 &
                             enroll_time + pfs < dropout_time - tol) == (pfs_event == 1)),
                or = all((enroll_time + or_readout < 30 &
                            enroll_time + or_readout < dropout_time - tol) == !is.na(or))) %>%
      apply(1, function(x){all(x)})
  )

  expect_true(
    dat2 %>%
      filter(os_event %in% 1) %>%
      summarise(os = all(enroll_time + os <= 40)) %>%
      apply(1, function(x){all(x)}))

  expect_true(
    dat2 %>%
      filter(pfs_event %in% 1) %>%
      summarise(pfs = all(enroll_time + pfs <= 40)) %>%
      apply(1, function(x){all(x)}))

  expect_true(
    dat2 %>%
      filter(!is.na(or)) %>%
      summarise(or = all(enroll_time + or_readout <= 40)) %>%
      apply(1, function(x){all(x)}))

})

test_that('endpoint event counts work as expected when an arm is removed', {

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

  interim1 <- event(name = 'interim1',
                    trigger_condition =
                      eventNumber(endpoint = 'or', n = 200),
                    action = function(trial, event_name){trial$remove_arms('trt1')})

  interim2 <- event(name = 'interim2',
                    trigger_condition =
                      eventNumber(endpoint = 'pfs', n = 240) &
                      eventNumber(endpoint = 'os', n = 170))

  final <- event(name = 'final',
                 trigger_condition = calendarTime(time = 40))

  listener <- listener(silent = TRUE)
  listener$add_events(interim1, interim2, final)

  controller <- controller(trial, listener)
  controller$run(n = 1, plot_event = FALSE, silent = TRUE)

  dat1 <- trial$get_locked_data('interim1')
  dat2 <- trial$get_locked_data('interim2')
  dat3 <- trial$get_locked_data('final')

  remove_attr <- function(locked_data){
    attr(locked_data, 'lock_time') <- NULL
    attr(locked_data, 'n_enrolled_patients') <- NULL
    attr(locked_data, 'event_name') <- NULL
    locked_data
  }

  dat1 <- remove_attr(dat1)
  dat2 <- remove_attr(dat2)
  dat3 <- remove_attr(dat3)

  expect_equal(sum(!is.na(dat1$or)), 200)

  expect_true(sum(dat2$pfs_event %in% 1 & dat2$arm %in% c('pbo', 'trt2')) >= 240 &&
                sum(dat2$os_event %in% 1 & dat2$arm %in% c('pbo', 'trt2')) >= 170 &&
                (sum(dat2$pfs_event %in% 1 & dat2$arm %in% c('pbo', 'trt2')) == 240 ||
                   sum(dat2$os_event %in% 1 & dat2$arm %in% c('pbo', 'trt2')) == 170))

  dat1_ <- dat2 %>%
    filter(patient_id %in% dat1$patient_id) %>%
    arrange(patient_id)

  expect_equal(dat1 %>% filter(arm == 'trt1'), dat1_ %>% filter(arm == 'trt1'))

  dat1__ <- dat3 %>%
    filter(patient_id %in% dat1$patient_id) %>%
    arrange(patient_id)

  expect_equal(dat1 %>% filter(arm == 'trt1'), dat1__ %>% filter(arm == 'trt1'))

})
