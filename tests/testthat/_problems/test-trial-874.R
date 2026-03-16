# Extracted from test-trial.R:874

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
