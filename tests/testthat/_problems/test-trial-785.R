# Extracted from test-trial.R:785

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "TrialSimulator", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
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
