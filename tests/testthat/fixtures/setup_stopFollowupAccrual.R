## Self-contained setup exercising the v1.25/v1.26 adaptation methods
## stop_followup() and update_accrual_rate() (with a leading pause), so the
## C++ lock-time fast path and the pure-R fallback are compared on designs
## that use them. Sourcing this file leaves a global `controller` ready for
## `$run()`.

library(TrialSimulator)
library(survival)

pfs <- endpoint(name = 'pfs', type = 'tte', generator = rexp, rate = log(2) / 8)
resp <- endpoint(name = 'resp', type = 'non-tte', readout = c(resp = 2),
                 generator = rbinom, size = 1, prob = .25)
pbo <- arm(name = 'pbo')
pbo$add_endpoints(pfs, resp)

pfs <- endpoint(name = 'pfs', type = 'tte', generator = rexp, rate = log(2) / 11)
resp <- endpoint(name = 'resp', type = 'non-tte', readout = c(resp = 2),
                 generator = rbinom, size = 1, prob = .40)
trt <- arm(name = 'trt')
trt$add_endpoints(pfs, resp)

trial <- trial(
  name = 'stopFollowupAccrual', n_patients = 200, duration = 30,
  seed = 20260723,
  enroller = StaggeredRecruiter,
  accrual_rate = data.frame(end_time = Inf, piecewise_rate = 20),
  dropout = rweibull, shape = 1.2, scale = 80
)

trial$add_arms(sample_ratio = c(1, 1), pbo, trt)

adapt_action <- function(trial){
  ## stop following the placebo cohort at the milestone, then pause
  ## recruitment for 2 months before resuming at a higher rate
  stop_followup(trial, arm == 'pbo')
  update_accrual_rate(trial,
                      data.frame(end_time = c(2, Inf),
                                 piecewise_rate = c(0, 30)))
}

adapt <- milestone(name = 'adapt',
                   when = enrollment(n = 100) &
                     eventNumber(endpoint = 'resp', n = 20),
                   action = adapt_action)

final_action <- function(trial){
  d <- trial$get_locked_data('final')
  fit <- fitLogrank(Surv(pfs, pfs_event) ~ arm, placebo = 'pbo',
                    data = d, alternative = 'less')
  trial$save(fit$z[1], 'FA_z')
  trial$save(sum(!is.na(d$resp)), 'FA_n_resp')
}

final <- milestone(name = 'final',
                   when = calendarTime(time = 28),
                   action = final_action)

listener <- listener()
listener$add_milestones(adapt, final)

controller <- controller(trial, listener)
