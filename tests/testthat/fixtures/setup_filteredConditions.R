## Self-contained setup whose milestone conditions all carry subset
## conditions in ..., so the C++ lock-time fast path (which reduces them to
## a logical mask via filter_mask()) is compared with the dplyr-backed
## pure-R fallback on: a cohort filter (patient_id), a baseline-covariate
## subgroup filter on eventNumber() for a tte and a non-tte endpoint, a
## filter on enrollment(), the .data pronoun, and a condition that is NA
## for some patients (NA rows must be dropped, as dplyr::filter() does).
## Sourcing this file leaves a global `controller` ready for `$run()`.

library(TrialSimulator)
library(survival)

rng <- function(n, med_pfs, prev){
  biomarker <- rbinom(n, 1, prev)
  data.frame(
    biomarker = biomarker,
    ## NA for a few patients so that a subset condition on it is NA
    score     = ifelse(runif(n) < .05, NA_real_, rnorm(n)),
    pfs       = rexp(n, log(2) / ifelse(biomarker == 1, med_pfs * 1.3, med_pfs)),
    pfs_event = 1
  )
}

make_arm <- function(name, med_pfs, prev, resp_prob){
  a <- arm(name = name)
  a$add_endpoints(
    endpoint(name = c('biomarker', 'score', 'pfs'),
             type = c('baseline', 'baseline', 'tte'),
             generator = rng, med_pfs = med_pfs, prev = prev),
    endpoint(name = 'resp', type = 'non-tte', readout = c(resp = 3),
             generator = rbinom, size = 1, prob = resp_prob)
  )
  a
}

pbo <- make_arm('pbo', 6, .5, .20)
trt <- make_arm('trt', 9, .5, .35)

trial <- trial(
  name = 'filteredConditions', n_patients = 240, duration = 40,
  seed = 20260903,
  enroller = StaggeredRecruiter,
  accrual_rate = data.frame(end_time = Inf, piecewise_rate = 15),
  dropout = rweibull, shape = 1.2, scale = 80
)

trial$add_arms(sample_ratio = c(1, 1), pbo, trt)

cohort_action <- function(trial){
  locked <- trial$get_locked_data('cohort')
  trial$save(nrow(locked), 'n_locked_cohort')
  stop_followup(trial, patient_id <= 80)
}

subgroup_action <- function(trial){
  locked <- trial$get_locked_data('subgroup')
  fit <- fitLogrank(Surv(pfs, pfs_event) ~ arm, placebo = 'pbo',
                    data = locked, alternative = 'less', biomarker == 1)
  trial$save(fit$z, 'z_subgroup')
}

## each condition is guarded by the previous one (& = both reached) so the
## milestones trigger in registration order whatever the data
c_cohort   <- eventNumber(endpoint = 'pfs', n = 30, patient_id <= 80)
c_subgroup <- (eventNumber(endpoint = 'pfs', n = 40, biomarker == 1) &
                 eventNumber(endpoint = 'resp', n = 60, .data$biomarker == 1)) &
  c_cohort
c_scored   <- (enrollment(n = 150, score > -1) |
                 eventNumber(endpoint = 'pfs', n = 120, score > 0)) &
  c_subgroup
c_final    <- (eventNumber(endpoint = 'pfs', n = 150) | calendarTime(time = 38)) &
  c_scored

listener <- listener(silent = TRUE)
listener$add_milestones(
  milestone(name = 'cohort',   when = c_cohort,   action = cohort_action),
  milestone(name = 'subgroup', when = c_subgroup, action = subgroup_action),
  milestone(name = 'scored',   when = c_scored),
  milestone(name = 'final',    when = c_final)
)

controller <- controller(trial, listener)
