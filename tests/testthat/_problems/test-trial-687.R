# Extracted from test-trial.R:687

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
