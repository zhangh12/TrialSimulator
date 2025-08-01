
test_that('trial milestone timing and endpoint event count work as expected', {

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

  interim1 <- milestone(name = 'interim1',
                        when = eventNumber(endpoint = 'or', n = 200))

  interim2 <- milestone(name = 'interim2',
                        when = eventNumber(endpoint = 'pfs', n = 240) &
                          eventNumber(endpoint = 'os', n = 170))

  final <- milestone(name = 'final',
                     when = calendarTime(time = 40))

  listener <- listener(silent = TRUE)
  listener$add_milestones(interim1, interim2, final)

  controller <- controller(trial, listener)
  controller$run(n = 1, plot_event = FALSE, silent = TRUE)

  ## check data

  dat1 <- trial$get_locked_data('interim1')
  dat2 <- trial$get_locked_data('interim2')
  dat3 <- trial$get_locked_data('final')

  time1 <- trial$get_milestone_time('interim1')
  time2 <- trial$get_milestone_time('interim2')


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

test_that('filters are supported when defining milestones', {

  #' random number generator (RNG) for OS, PFS, and PRO
  rng <- function(n, medians, prop_high){

    ## os
    median_os_low <- TrialSimulator::solveMixtureExponentialDistribution(
      weight1 = prop_high,
      median1 = medians['os_high'],
      overall_median = medians['os_all']
    )

    median_pfs_low <- TrialSimulator::solveMixtureExponentialDistribution(
      weight1 = prop_high,
      median1 = medians['pfs_high'],
      overall_median = medians['pfs_all']
    )

    median_pro_low <- TrialSimulator::solveMixtureExponentialDistribution(
      weight1 = prop_high,
      median1 = medians['pro_high'],
      overall_median = medians['pro_all']
    )

    dll3 <- sample(c('high', 'low'), size = n, replace = TRUE, prob = c(prop_high, 1 - prop_high))

    os_high <- rexp(n, rate = log(2) / medians['os_high'])
    os_low <- rexp(n, rate = log(2) / median_os_low)

    pfs_high <- rexp(n, rate = log(2) / medians['pfs_high'])
    pfs_low <- rexp(n, rate = log(2) / median_pfs_low)

    pro_high <- rexp(n, rate = log(2) / medians['pro_high'])
    pro_low <- rexp(n, rate = log(2) / median_pro_low)

    data.frame(dll3,
               x = rnorm(n),
               os = ifelse(dll3 %in% 'high', os_high, os_low),
               pfs = ifelse(dll3 %in% 'high', pfs_high, pfs_low),
               pro = ifelse(dll3 %in% 'high', pro_high, pro_low),
               os_event = 1,
               pfs_event = 1,
               pro_event = 1)
  }


  simulate <- function(fwer,
                       sample_size,
                       final_event_number,
                       interim_info_fraction,
                       prop_dll3_high,
                       median_soc,
                       median_trt){


    ep_soc <- endpoint(name = c('pfs', 'os', 'pro', 'dll3', 'x'),
                       type = c('tte', 'tte', 'tte', 'non-tte', 'non-tte'),
                       readout = c(dll3 = 0, x = 0),
                       generator = rng,
                       medians = median_soc,
                       prop_high = prop_dll3_high)

    soc <- arm(name = 'soc')
    soc$add_endpoints(ep_soc)

    ep_trt <- endpoint(name = c('pfs', 'os', 'pro', 'dll3', 'x'),
                       type = c('tte', 'tte', 'tte', 'non-tte', 'non-tte'),
                       readout = c(dll3 = 0, x = 0),
                       generator = rng,
                       medians = median_trt,
                       prop_high = prop_dll3_high)

    trt <- arm(name = 'trt')
    trt$add_endpoints(ep_trt)


    accrual_rate <- data.frame(end_time = c(1:17, Inf),
                               piecewise_rate = c(1,2,5,8,12,17,22,28,32,37,40,43,46,47,50,50,51,52))

    trial <- trial(
      name = '1438', n_patients = sample_size, duration = 200,
      enroller = StaggeredRecruiter, accrual_rate = accrual_rate,
      dropout = rexp, rate = -log(1 - .15)/12,
      silent = TRUE
    )

    trial$add_arms(sample_ratio = c(1, 1), soc, trt)

    interim <- milestone(name = 'interim', when = enrollment(n = 100, dll3 == 'high', x > -2),
                         action = interim_action)

    final <- milestone(name = 'final',
                       when =
                         eventNumber('pfs', n = 100, dll3 == 'high', x > -2) &
                         eventNumber('pfs', n = 150) &
                         eventNumber('os', n = 25, dll3 != 'high'),
                       action = final_action)

    listener <- listener(silent = TRUE)
    listener$add_milestones(interim, final)

    controller <- controller(trial, listener)
    controller$run(n = 10, plot_event = FALSE, silent = TRUE)

    controller$get_output()

  }

  interim_action <- function(trial, milestone_name){

    locked_data <- trial$get_locked_data(milestone_name)
    trial$save(value = locked_data %>% filter(dll3 == 'high' & x > -2) %>% nrow(),
               name = 'n_at_lock')

    invisible(NULL)
  }

  final_action <- function(trial, milestone_name){

    locked_data <- trial$get_locked_data(milestone_name)
    tmp1 <- locked_data %>% filter(dll3 == 'high' & x > -2)
    trial$save(value = sum(tmp1$pfs_event), name = 'n_pfs_high')
    trial$save(value = sum(locked_data$pfs_event), name = 'n_pfs_all')

    tmp2 <- locked_data %>% filter(dll3 != 'high')
    trial$save(value = sum(tmp2$os_event), name = 'n_os_low')

    invisible(NULL)
  }


  sims <-
    simulate(fwer = c(0.025), ## family-wise error rate for one-sided test
             sample_size = c(702), ## total planned sample size
             final_event_number = list(os_high = 343, os_all = 512),
             interim_info_fraction = c(.68),
             prop_dll3_high = c(.67),
             median_soc = list(c(os_high = 13,
                                 os_all = 13,
                                 pfs_high = 5,
                                 pfs_all = 5,
                                 pro_high = 7.5,
                                 pro_all = 7.5))[[1]],
             median_trt = list(c(os_high = 19,
                                 os_all = 17.6,
                                 pfs_high = 7,
                                 pfs_all = 6.5,
                                 pro_high = 10,
                                 pro_all = 9.7))[[1]])

  expect_true(all(sims$n_at_lock == 100))

  expect_true(all(sims$n_pfs_high >= 100))

  expect_true(all(sims$n_pfs_all >= 150))

  expect_true(all(sims$n_os_low >= 25))

  expect_true(all(sims$`milestone_time_<final>` > sims$`milestone_time_<interim>`))


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

  action_at_interim <- function(trial, milestone_name){
    trial$set_duration(duration = 40)
  }

  interim <- milestone(name = 'interim',
                       when = eventNumber(endpoint = 'os', n = 150),
                       action = action_at_interim)

  listener <- listener(silent = TRUE)
  listener$add_milestones(interim)

  controller <- controller(trial, listener)
  controller$run(plot_event = FALSE, silent = TRUE)

  dat1_ <- trial$get_locked_data('interim')

  expect_equal(sum(dat1_$os_event %in% 1), 150)

  final <- milestone(name = 'final',
                     when = eventNumber(endpoint = 'os', n = 400) |
                       calendarTime(time = 40))


  listener$add_milestones(final)
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

  interim1 <- milestone(name = 'interim1',
                        when = eventNumber(endpoint = 'or', n = 200),
                        action = function(trial, milestone_name){trial$remove_arms('trt1')})

  interim2 <- milestone(name = 'interim2',
                        when = eventNumber(endpoint = 'pfs', n = 240) &
                          eventNumber(endpoint = 'os', n = 170))

  final <- milestone(name = 'final',
                 when = calendarTime(time = 40))

  listener <- listener(silent = TRUE)
  listener$add_milestones(interim1, interim2, final)

  controller <- controller(trial, listener)
  controller$run(n = 1, plot_event = FALSE, silent = TRUE)

  dat1 <- trial$get_locked_data('interim1')
  dat2 <- trial$get_locked_data('interim2')
  dat3 <- trial$get_locked_data('final')

  remove_attr <- function(locked_data){
    attr(locked_data, 'lock_time') <- NULL
    attr(locked_data, 'n_enrolled_patients') <- NULL
    attr(locked_data, 'milestone_name') <- NULL
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

test_that('inclusion criteria of arm work as expected', {

  rng <- function(n){

    data.frame(
      x = rnorm(n),
      y = rbinom(n, 1, .5),
      z = sample(LETTERS[1:3], n, replace = TRUE),
      w = rexp(n, rate = .01),
      w_event = rbinom(n, 1, .3)
    )

  }

  ep <- endpoint(name = c('x', 'y', 'z', 'w'),
                 type = c('non-tte', 'non-tte', 'non-tte', 'tte'),
                 readout = c(x = 0, y = 0, z = 0),
                 generator = rng)

  test <- arm(name = 'test', x > 0)
  test$add_endpoints(ep)

  dat <- test$generate_data(1e4)
  expect_true(all(dat$x > 0))

  test <- arm(name = 'test', x > 1, z %in% c('A', 'C'))
  test$add_endpoints(ep)
  dat <- test$generate_data(1e4)
  expect_true(all(dat$x > 1) && all(dat$z %in% c('A', 'C')))

  test <- arm(name = 'test', x > -0.5 | z %in% c('B', 'C'))
  test$add_endpoints(ep)
  dat <- test$generate_data(1e4)
  expect_true(all(dat$x > -0.5 | dat$z %in% c('B', 'C')))

})

test_that('custom data can be re-used in multiple trials', {

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
                     action = function(trial, milestone_name) {
                       trial$get_custom_data('config')},
                     when = calendarTime(time = 40))

  listener <- listener(silent = TRUE)
  listener$add_milestones(final)

  controller <- controller(trial, listener)
  expect_no_error(controller$run(n = 10, plot_event = FALSE, silent = TRUE))

})

test_that('trial data can be replicated', {

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

  act <- function(trial, milestone_name){

    locked_data <- trial$get_locked_data(milestone_name)
    trial$save(value = median(locked_data$ep), name = 'median')
    trial$save(value = mean(locked_data$ep), name = 'mean')
    trial$save(value = sd(locked_data$ep), name = 'sd')

  }

  final <- milestone(name = 'final',
                     action = act,
                     when = calendarTime(time = 40))

  listener <- listener(silent = TRUE)
  listener$add_milestones(final)

  controller <- controller(trial, listener)
  controller$run(n = 10, plot_event = FALSE, silent = TRUE)
  op10 <- controller$get_output()
  op10 <- op10[order(op10$seed), ]
  rownames(op10) <- NULL

  ## feed seeds in op10 one by one for testing purpose

  seeds <- sort(op10$seed)
  ops <- NULL
  for(seed in seeds){
    trial <- trial(
      name = 'test', n_patients = 1000, duration = 40,
      seed = seed,
      enroller = StaggeredRecruiter, accrual_rate = accrual_rate,
      silent = TRUE
    )

    trial$add_arms(sample_ratio = 1, pbo)

    final <- milestone(name = 'final',
                       action = act,
                       when = calendarTime(time = 40))

    listener <- listener(silent = TRUE)
    listener$add_milestones(final)

    controller <- controller(trial, listener)
    controller$run(plot_event = FALSE, silent = TRUE)
    ops <- rbind(ops, controller$get_output())

  }

  expect_identical(op10, ops)

})

test_that('fitLinear works as expected', {


  ep <- endpoint(name = 'ep', type = 'tte', generator = rnorm)
  pbo <- arm(name = 'pbo')
  pbo$add_endpoints(ep)

  ep <- endpoint(name = 'ep', type = 'tte', generator = rnorm, mean = .1)
  trt <- arm(name = 'trt')
  trt$add_endpoints(ep)

  accrual_rate <- data.frame(end_time = c(1, 2, 6, 12, Inf),
                             piecewise_rate = c(2, 8, 20, 25, 50))

  trial <- trial(
    name = 'test', n_patients = 1000, duration = 40,
    enroller = StaggeredRecruiter, accrual_rate = accrual_rate,
    silent = TRUE
  )

  trial$add_arms(sample_ratio = c(1, 2), pbo, trt)

  act <- function(trial, milestone_name){

    locked_data <- trial$get_locked_data(milestone_name)

    n <- nrow(locked_data)
    locked_data$covar1 <- rnorm(n)
    locked_data$covar2 <- rbinom(n, 1, .4)

    ## ATE is equivalent to model coefficient when no interaction term in linear regression
    fit <- fitLinear(ep ~ arm + covar1 + covar2, placebo = 'pbo', data = locked_data, alternative = 'greater')
    trial$save(value = fit, name = 'fitLinear_output')
    fit_ <- lm(ep ~ I(arm != 'pbo') + covar1 + covar2, data =locked_data)
    trial$save(value = data.frame(z = summary(fit_)$coef[2, 't value']) %>%
                 mutate(p = 1 - pt(z, df = fit_$df.residual - 2)) %>%
                 mutate(info = fit_$df.residual + fit_$rank),
               name = 'lm_output')

    invisible(NULL)

  }

  final <- milestone(name = 'final',
                     action = act,
                     when = calendarTime(time = 40))

  listener <- listener(silent = TRUE)
  listener$add_milestones(final)

  controller <- controller(trial, listener)
  controller$run(n = 10, plot_event = FALSE, silent = TRUE)

  op <- controller$get_output()

  expect_equal(op$`fitLinear_output_<p>`, op$`lm_output_<p>`, tolerance = 1e-3)
  expect_equal(op$`fitLinear_output_<z>`, op$`lm_output_<z>`, tolerance = 1e-3)
  expect_equal(op$`fitLinear_output_<info>`, op$`lm_output_<info>`)
  expect_true(all(op$`fitLinear_output_<arm>` == 'trt'))
  expect_true(all(op$`fitLinear_output_<placebo>` == 'pbo'))

})

test_that('fitLinear can compute ATE as expected in additive model', {

  ep <- endpoint(name = 'ep', type = 'non-tte', readout = c(ep = 0), generator = rnorm)
  pbo <- arm(name = 'pbo')
  pbo$add_endpoints(ep)

  ep <- endpoint(name = 'ep', type = 'non-tte', readout = c(ep = 0), generator = rnorm, mean = .1)
  trt <- arm(name = 'trt')
  trt$add_endpoints(ep)

  accrual_rate <- data.frame(end_time = c(1, 2, 6, 12, Inf),
                             piecewise_rate = c(2, 8, 20, 25, 50))

  trial <- trial(
    name = 'test', n_patients = 1000, duration = 40,
    enroller = StaggeredRecruiter, accrual_rate = accrual_rate,
    silent = TRUE
  )

  trial$add_arms(sample_ratio = c(1, 2), pbo, trt)

  act <- function(trial, milestone_name){

    locked_data <- trial$get_locked_data(milestone_name)

    n <- nrow(locked_data)
    locked_data$covar1 <- rnorm(n)
    locked_data$covar2 <- rbinom(n, 1, .4)

    ## ATE is equivalent to model coefficient when no interaction term in linear regression
    fit <- fitLinear(ep ~ arm + covar1 + covar2, placebo = 'pbo', data = locked_data, alternative = 'greater')
    trial$save(value = fit, name = 'fitLinear_output')

    fit_ <- lm(ep ~ I(arm != 'pbo') + covar1 + covar2, data = locked_data)
    trial$save(value = data.frame(z = summary(fit_)$coef[2, 't value'],
                                  estimate = summary(fit_)$coef[2, 'Estimate']) %>%
                 mutate(p = 1 - pt(z, df = fit_$df.residual - 2)) %>%
                 mutate(info = fit_$df.residual + fit_$rank),
               name = 'lm_output')

    invisible(NULL)

  }

  final <- milestone(name = 'final',
                     action = act,
                     when = calendarTime(time = 40))

  listener <- listener(silent = TRUE)
  listener$add_milestones(final)

  controller <- controller(trial, listener)
  controller$run(n = 10, plot_event = FALSE, silent = TRUE)

  op <- controller$get_output()

  expect_equal(op$`fitLinear_output_<p>`, op$`lm_output_<p>`, tolerance = 1e-3)
  expect_equal(op$`fitLinear_output_<z>`, op$`lm_output_<z>`, tolerance = 1e-3)
  expect_equal(op$`fitLinear_output_<estimate>`, op$`lm_output_<estimate>`, tolerance = 1e-3)
  expect_equal(op$`fitLinear_output_<info>`, op$`lm_output_<info>`)
  expect_true(all(op$`fitLinear_output_<arm>` == 'trt'))
  expect_true(all(op$`fitLinear_output_<placebo>` == 'pbo'))

})

test_that('fitLogistic can compute ATE as expected in model without covariates', {

  ep <- endpoint(name = 'ep', type = 'non-tte', readout = c(ep = 0), generator = rnorm)
  pbo <- arm(name = 'pbo')
  pbo$add_endpoints(ep)

  ep <- endpoint(name = 'ep', type = 'non-tte', readout = c(ep = 0), generator = rnorm, mean = .1)
  trt <- arm(name = 'trt')
  trt$add_endpoints(ep)

  accrual_rate <- data.frame(end_time = c(1, 2, 6, 12, Inf),
                             piecewise_rate = c(2, 8, 20, 25, 50))

  trial <- trial(
    name = 'test', n_patients = 1000, duration = 40,
    enroller = StaggeredRecruiter, accrual_rate = accrual_rate,
    silent = TRUE
  )

  trial$add_arms(sample_ratio = c(1, 2), pbo, trt)

  act <- function(trial, milestone_name){

    locked_data <- trial$get_locked_data(milestone_name)

    trial$save(value = nrow(locked_data), name = 'n')

    ## ATE is equivalent to log OR estimate when no covariate in logistic regression
    fit_logOR <- fitLogistic(I(ep > 0) ~ arm, placebo = 'pbo',
                             data = locked_data, alternative = 'greater',
                             scale = 'log odds ratio')

    trial$save(value = fit_logOR, name = 'fit_logOR')

    fit_OR <- fitLogistic(I(ep > 0) ~ arm, placebo = 'pbo',
                          data = locked_data, alternative = 'greater',
                          scale = 'odds ratio')

    trial$save(value = fit_OR, name = 'fit_OR')

    ## ATE is equivalent to ratio of arm probabilities when no covariate in logistic regression
    fit_RR <- fitLogistic(I(ep > 0) ~ arm, placebo = 'pbo',
                          data = locked_data, alternative = 'greater',
                          scale = 'risk ratio')

    trial$save(value = fit_RR, name = 'fit_RR')

    ## ATE is equivalent to difference of arm probabilities when no covariate in logistic regression
    fit_RD <- fitLogistic(I(ep > 0) ~ arm, placebo = 'pbo',
                          data = locked_data, alternative = 'greater',
                          scale = 'risk difference')

    trial$save(value = fit_RD, name = 'fit_RD')

    fit <- glm(I(ep > 0) ~ I(arm != 'pbo'), data = locked_data, family = 'binomial')

    trial$save(value = data.frame(estimate = summary(fit)$coef[2, 'Estimate'],
                                  z = summary(fit)$coef[2, 'z value']) %>%
                 mutate(p = 1 - pnorm(z)) %>%
                 mutate(info = fit$df.residual + fit$rank),
               name = 'glm_logOR')

    probs <- locked_data %>%
      group_by(arm) %>%
      summarise(prob = mean(ep > 0))

    trial$save(value = exp(summary(fit)$coef[2, 'Estimate']),
               name = 'glm_OR')

    trial$save(value = probs$prob[probs$arm == 'trt'] / probs$prob[probs$arm == 'pbo'],
               name = 'glm_RR')

    trial$save(value = probs$prob[probs$arm == 'trt'] - probs$prob[probs$arm == 'pbo'],
               name = 'glm_RD')

    invisible(NULL)

  }

  final <- milestone(name = 'final',
                     action = act,
                     when = calendarTime(time = 40))

  listener <- listener(silent = TRUE)
  listener$add_milestones(final)

  controller <- controller(trial, listener)
  controller$run(n = 10, plot_event = FALSE, silent = TRUE)

  op <- controller$get_output()

  #################

  expect_true(all(op$`fit_logOR_<arm>` == 'trt'))
  expect_true(all(op$`fit_OR_<arm>` == 'trt'))
  expect_true(all(op$`fit_RR_<arm>` == 'trt'))
  expect_true(all(op$`fit_RD_<arm>` == 'trt'))

  expect_true(all(op$`fit_logOR_<placebo>` == 'pbo'))
  expect_true(all(op$`fit_OR_<placebo>` == 'pbo'))
  expect_true(all(op$`fit_RR_<placebo>` == 'pbo'))
  expect_true(all(op$`fit_RD_<placebo>` == 'pbo'))

  expect_equal(op$`fit_logOR_<estimate>`, op$`glm_logOR_<estimate>`, tolerance = 1e-3)

  expect_equal(op$`fit_logOR_<p>`, op$`glm_logOR_<p>`, tolerance = 1e-3)
  expect_equal(op$`fit_OR_<p>`, op$`glm_logOR_<p>`, tolerance = 1e-3)

  expect_equal(op$`fit_logOR_<z>`, op$`glm_logOR_<z>`, tolerance = 1e-3)
  expect_equal(op$`fit_OR_<z>`, op$`glm_logOR_<z>`, tolerance = 1e-3)

  expect_equal(op$`fit_OR_<estimate>`, op$`glm_OR`, tolerance = 1e-3)
  expect_equal(op$`fit_RR_<estimate>`, op$`glm_RR`, tolerance = 1e-3)
  expect_equal(op$`fit_RD_<estimate>`, op$`glm_RD`, tolerance = 1e-3)

  expect_identical(op$`fit_logOR_<info>`, op$n)
  expect_identical(op$`fit_logOR_<info>`, op$`fit_OR_<info>`)
  expect_identical(op$`fit_logOR_<info>`, op$`fit_RR_<info>`)
  expect_identical(op$`fit_logOR_<info>`, op$`fit_RD_<info>`)


})

test_that('fitLogistic can compute regression coefficient as expected in model with covariates', {

  ep <- endpoint(name = 'ep', type = 'non-tte', readout = c(ep = 0), generator = rnorm)
  pbo <- arm(name = 'pbo')
  pbo$add_endpoints(ep)

  ep <- endpoint(name = 'ep', type = 'non-tte', readout = c(ep = 0), generator = rnorm, mean = .1)
  trt <- arm(name = 'trt')
  trt$add_endpoints(ep)

  accrual_rate <- data.frame(end_time = c(1, 2, 6, 12, Inf),
                             piecewise_rate = c(2, 8, 20, 25, 50))

  trial <- trial(
    name = 'test', n_patients = 1000, duration = 40,
    enroller = StaggeredRecruiter, accrual_rate = accrual_rate,
    silent = TRUE
  )

  trial$add_arms(sample_ratio = c(1, 2), pbo, trt)

  act <- function(trial, milestone_name){

    locked_data <- trial$get_locked_data(milestone_name)
    locked_data$x <- rnorm(nrow(locked_data))
    locked_data$y <- rnorm(nrow(locked_data))
    locked_data$z <- rbinom(nrow(locked_data), 1, .5)

    trial$save(value = nrow(locked_data), name = 'n')

    fit_coef <- fitLogistic(I(ep > 0) ~ x*arm + z + arm:y, placebo = 'pbo',
                            data = locked_data, alternative = 'greater',
                            scale = 'coefficient')
    fit <- glm(I(ep > 0) ~ arm*x + z + arm:y, data = locked_data,
               family = binomial)

    trial$save(value = fit_coef, name = 'fit_coef')
    trial$save(value = data.frame(estimate = summary(fit)$coef['armtrt', 'Estimate'],
                                  z = summary(fit)$coef['armtrt', 'z value']) %>%
                 mutate(p = 1 - pnorm(z)) %>%
                 mutate(info = fit$df.residual + fit$rank),
               name = 'glm_coef')

    invisible(NULL)

  }

  final <- milestone(name = 'final',
                     action = act,
                     when = calendarTime(time = 40))

  listener <- listener(silent = TRUE)
  listener$add_milestones(final)

  controller <- controller(trial, listener)
  controller$run(n = 10, plot_event = FALSE, silent = TRUE)

  op <- controller$get_output()

  #################

  expect_true(all(op$`fit_coef_<arm>` == 'trt'))

  expect_true(all(op$`fit_coef_<placebo>` == 'pbo'))

  expect_equal(op$`fit_coef_<estimate>`, op$`glm_coef_<estimate>`, tolerance = 1e-3)

  expect_equal(op$`fit_coef_<p>`, op$`glm_coef_<p>`, tolerance = 1e-3)

  expect_equal(op$`fit_coef_<z>`, op$`glm_coef_<z>`, tolerance = 1e-3)

  expect_identical(op$`fit_coef_<info>`, op$n)

})

test_that('fitLogrank works as expected', {

  ep <- endpoint(name = 'ep', type = 'tte', generator = rexp, rate = log(2)/10)
  pbo <- arm(name = 'pbo')
  pbo$add_endpoints(ep)

  ep <- endpoint(name = 'ep', type = 'tte', generator = rexp, rate = log(2)/12)
  trt <- arm(name = 'trt')
  trt$add_endpoints(ep)

  accrual_rate <- data.frame(end_time = c(1, 2, 6, 12, Inf),
                             piecewise_rate = c(2, 8, 20, 25, 50))

  trial <- trial(
    name = 'test', n_patients = 1000, duration = 40,
    enroller = StaggeredRecruiter, accrual_rate = accrual_rate,
    silent = TRUE
  )

  trial$add_arms(sample_ratio = c(1, 2), pbo, trt)

  act <- function(trial, milestone_name){

    locked_data <- trial$get_locked_data(milestone_name)

    fit <- fitLogrank(Surv(ep, ep_event) ~ arm, placebo = 'pbo', data = locked_data, alternative = 'less')

    lr <- survdiff(Surv(ep, ep_event) ~ arm, data = locked_data)
    fit_ <- coxph(Surv(ep, ep_event) ~ arm, data = locked_data)

    p <- fit$p
    z <- sqrt(lr$chisq) * ifelse(coef(fit_)['armtrt'] > 0, 1, -1)
    p_ <- pnorm(z)

    trial$save(value = p, name = 'logrank_p')
    trial$save(value = p_, name = 'survdiff_p')
    invisible(NULL)

  }

  final <- milestone(name = 'final',
                     action = act,
                     when = calendarTime(time = 40))

  listener <- listener(silent = TRUE)
  listener$add_milestones(final)

  controller <- controller(trial, listener)
  controller$run(n = 10, plot_event = FALSE, silent = TRUE)

  op <- controller$get_output()

  expect_equal(op$logrank_p, op$survdiff_p, tolerance = 1e-3)

})

test_that('fitCoxph can compute main effect of arm', {

  ep <- endpoint(name = 'ep', type = 'tte', generator = rexp, rate = log(2)/10)
  pbo <- arm(name = 'pbo')
  pbo$add_endpoints(ep)

  ep <- endpoint(name = 'ep', type = 'tte', generator = rexp, rate = log(2)/12)
  trt <- arm(name = 'trt')
  trt$add_endpoints(ep)

  accrual_rate <- data.frame(end_time = c(1, 2, 6, 12, Inf),
                             piecewise_rate = c(2, 8, 20, 25, 50))

  trial <- trial(
    name = 'test', n_patients = 1000, duration = 40,
    enroller = StaggeredRecruiter, accrual_rate = accrual_rate,
    silent = TRUE
  )

  trial$add_arms(sample_ratio = c(1, 2), pbo, trt)

  act <- function(trial, milestone_name){

    locked_data <- trial$get_locked_data(milestone_name)

    n <- nrow(locked_data)
    locked_data$covar1 <- rnorm(n)
    locked_data$covar2 <- rbinom(n, 1, .4)

    fit <- fitCoxph(Surv(ep, ep_event) ~ covar2 + arm*covar1 + covar1:covar2,
                    placebo = 'pbo', data = locked_data, alternative = 'less',
                    scale = 'hazard ratio')
    trial$save(value = fit, name = 'fitCoxph_output')

    fit_ <- coxph(Surv(ep, ep_event) ~ I(arm != 'pbo')*covar1 + covar1:covar2 + covar2, data = locked_data)

    trial$save(value = data.frame(z = summary(fit_)$coef[1, 'z'],
                                  estimate = exp(summary(fit_)$coef[1, 'coef'])) %>%
                 mutate(p = pnorm(z)) %>%
                 mutate(info = sum(locked_data$ep_event)),
               name = 'coxph_output')

    invisible(NULL)

  }

  final <- milestone(name = 'final',
                     action = act,
                     when = calendarTime(time = 40))

  listener <- listener(silent = TRUE)
  listener$add_milestones(final)

  controller <- controller(trial, listener)
  controller$run(n = 10, plot_event = FALSE, silent = TRUE)

  op <- controller$get_output()

  expect_equal(op$`fitCoxph_output_<p>`, op$`coxph_output_<p>`, tolerance = 1e-3)
  expect_equal(op$`fitCoxph_output_<z>`, op$`coxph_output_<z>`, tolerance = 1e-3)
  expect_equal(op$`fitCoxph_output_<estimate>`, op$`coxph_output_<estimate>`, tolerance = 1e-3)
  expect_equal(op$`fitCoxph_output_<info>`, op$`coxph_output_<info>`)
  expect_true(all(op$`fitCoxph_output_<arm>` == 'trt'))
  expect_true(all(op$`fitCoxph_output_<placebo>` == 'pbo'))

})



