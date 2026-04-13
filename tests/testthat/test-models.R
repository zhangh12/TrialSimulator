# Statistical model fitting
#
# Covers:
#   - fitLinear: coefficient and ATE in additive model with and without covariates
#   - fitLogistic: log OR, OR, RR, RD scales without covariates; regression
#                  coefficient with covariates and interaction terms
#   - fitLogrank: agreement with survdiff/coxph for one-sided test
#   - fitCoxph: main effect of arm with covariates and interaction terms

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

  act <- function(trial){

    locked_data <- trial$get_locked_data('final')

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

  act <- function(trial){

    locked_data <- trial$get_locked_data('final')

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

  act <- function(trial){

    locked_data <- trial$get_locked_data('final')

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

  act <- function(trial){

    locked_data <- trial$get_locked_data('final')
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

  act <- function(trial){

    locked_data <- trial$get_locked_data('final')

    fit <- fitLogrank(Surv(ep, ep_event) ~ arm, placebo = 'pbo', data = locked_data, alternative = 'less')

    lr <- survdiff(Surv(ep, ep_event) ~ arm, data = locked_data)
    fit_ <- coxph(Surv(ep, ep_event) ~ arm, data = locked_data)

    p <- fit$p
    z <- sqrt(lr$chisq) * ifelse(coef(fit_)['armtrt'] > 0, 1, -1)
    p_ <- pnorm(z)

    trial$save(value = p, name = 'logrank_p')
    trial$save(value = p_, name = 'survdiff_p')

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

  act <- function(trial){

    locked_data <- trial$get_locked_data('final')

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
