## Self-contained setup exercising update_milestone() (v1.28): at an interim,
## a data-dependent decision raises the final analysis event target or leaves
## it as designed, so both the update path and the as-designed path (and the
## between-replicate restoration) are covered under the C++/R identical-path
## check. Sourcing this file leaves a global `controller` ready for `$run()`.

library(TrialSimulator)
library(survival)

pfs <- endpoint(name = 'pfs', type = 'tte', generator = rexp, rate = log(2) / 8)
pbo <- arm(name = 'pbo')
pbo$add_endpoints(pfs)

pfs <- endpoint(name = 'pfs', type = 'tte', generator = rexp, rate = log(2) / 11)
trt <- arm(name = 'trt')
trt$add_endpoints(pfs)

trial <- trial(
  name = 'updateMilestone', n_patients = 250, duration = 30,
  seed = 20260724,
  enroller = StaggeredRecruiter,
  accrual_rate = data.frame(end_time = Inf, piecewise_rate = 25),
  dropout = rweibull, shape = 1.2, scale = 90
)

trial$add_arms(sample_ratio = c(1, 1), pbo, trt)

interim_action <- function(trial){
  d <- trial$get_locked_data('interim')
  fit <- fitLogrank(Surv(pfs, pfs_event) ~ arm, placebo = 'pbo',
                    data = d, alternative = 'less')
  if(fit$z[1] > -1.2){
    ## conditional power unimpressive: postpone the final analysis
    update_milestone(trial, 'final',
                     when = eventNumber(endpoint = 'pfs', n = 150))
    trial$save('raised', 'im_decision')
  }else{
    trial$save('kept', 'im_decision')
  }
}

interim <- milestone(name = 'interim',
                     when = eventNumber(endpoint = 'pfs', n = 60),
                     action = interim_action)

final_action <- function(trial){
  d <- trial$get_locked_data('final')
  fit <- fitLogrank(Surv(pfs, pfs_event) ~ arm, placebo = 'pbo',
                    data = d, alternative = 'less')
  trial$save(fit$z[1], 'FA_z')
  trial$save(sum(d$pfs_event), 'FA_events')
}

final <- milestone(name = 'final',
                   when = eventNumber(endpoint = 'pfs', n = 110),
                   action = final_action)

listener <- listener()
listener$add_milestones(interim, final)

controller <- controller(trial, listener)
