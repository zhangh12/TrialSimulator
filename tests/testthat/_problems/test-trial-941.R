# Extracted from test-trial.R:941

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
