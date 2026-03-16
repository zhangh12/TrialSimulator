# Extracted from test-trial.R:1001

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "TrialSimulator", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
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
