knitr::opts_chunk$set(
  collapse = TRUE,
  cache.path = 'cache/enrichmentDesign/',
  comment = '#>',
  dpi = 300,
  out.width = '100%'
)

library(dplyr)
library(kableExtra)
library(survival)
library(ggplot2)
library(TrialSimulator)

rng <- function(n, median0, median1, prevalence){
  biomarker <- rbinom(n, size = 1, prob = prevalence)
  pfs <- rexp(n, rate = log(2) / ifelse(biomarker == 1, median1, median0))
  data.frame(biomarker = biomarker, pfs = pfs, pfs_event = 1)
}

pbo_endpoints <- endpoint(name = c('biomarker', 'pfs'),
                          type = c('baseline', 'tte'),
                          generator = rng, median0 = 6, median1 = 6,
                          prevalence = .4)
pbo <- arm(name = 'pbo')
pbo$add_endpoints(pbo_endpoints)

trt_endpoints <- endpoint(name = c('biomarker', 'pfs'),
                          type = c('baseline', 'tte'),
                          generator = rng, median0 = 6.5, median1 = 10,
                          prevalence = .4)
trt <- arm(name = 'trt')
trt$add_endpoints(trt_endpoints)

accrual_rate <- data.frame(end_time = c(6, Inf), piecewise_rate = c(6, 12))
trial <- trial(name = 'enrichment', n_patients = 320, seed = 97,
               enroller = StaggeredRecruiter, accrual_rate = accrual_rate,
               dropout = rexp, rate = -log(1 - .05) / 12, ## 5% by month 12
               silent = TRUE)
trial$add_arms(sample_ratio = c(1, 1), pbo, trt)

interim_action <- function(trial){

  locked_data <- trial$get_locked_data('interim')

  cp_full <- trial$conditionalPower(
    milestone = 'interim', Surv(pfs, pfs_event) ~ arm, placebo = 'pbo',
    alternative = 'less', alpha = .025, D = 240, effect = 'trend',
    patient_id <= 80)$cp

  cp_sub <- trial$conditionalPower(
    milestone = 'interim', Surv(pfs, pfs_event) ~ arm, placebo = 'pbo',
    alternative = 'less', alpha = .025, D = 150, effect = 'trend',
    patient_id <= 80 & biomarker == 1)$cp

  decision <- if(cp_full < .01 && cp_sub < .01){
    'futility'
  }else if(cp_full < .2 && cp_sub < .6){
    'unfavorable'
  }else if(cp_full < .2){
    'enrichment'
  }else if(cp_full < .9){
    'promising'
  }else{
    'favorable'
  }

  if(decision == 'futility'){

    ## stop the trial now: both remaining milestones are triggered at the
    ## current time, and their action functions skip the analyses
    now <- trial$get_current_time()
    trial$update_milestone(name = 'stage 1 analysis',
                           when = calendarTime(time = now))
    trial$update_milestone(name = 'final',
                           when = calendarTime(time = now))

  }else if(decision == 'enrichment'){

    en <- trial$eventNumberReestimationFromConditionalPower(
      milestone = 'interim', Surv(pfs, pfs_event) ~ arm, placebo = 'pbo',
      alternative = 'less', alpha = .025, target_cp = .95, effect = 'trend',
      patient_id <= 80 & biomarker == 1, D_cap = 200)

    max_events_in_subgroup <- ifelse(en$target_reached, en$D, 200)
    max_patients_in_subgroup <- 280
    enrolled_in_subgroup <- sum(locked_data$biomarker)

    trial$resize(n_patients = nrow(locked_data) +
                   max_patients_in_subgroup - enrolled_in_subgroup)
    trial$update_accrual_rate(
      accrual_rate = data.frame(end_time = Inf, piecewise_rate = 5))

    trial$update_generator(
      arm_name = 'pbo', endpoint_name = c('pfs', 'biomarker'),
      generator = rng, median0 = 6, median1 = 6, prevalence = 1.0)
    trial$update_generator(
      arm_name = 'trt', endpoint_name = c('pfs', 'biomarker'),
      generator = rng, median0 = 6.5, median1 = 10, prevalence = 1.0)

    trial$update_milestone(
      name = 'final',
      when = eventNumber(endpoint = 'pfs', n = max(max_events_in_subgroup, 50),
                         biomarker == 1) &
        eventNumber(endpoint = 'pfs', n = 65, patient_id <= 80))

  }else if(decision == 'promising'){

    en <- trial$eventNumberReestimationFromConditionalPower(
      milestone = 'interim', Surv(pfs, pfs_event) ~ arm, placebo = 'pbo',
      alternative = 'less', alpha = .025, target_cp = .95, effect = 'trend',
      patient_id <= 80, D_cap = 400)

    max_events <- ifelse(en$target_reached, en$D, 400)

    trial$resize(n_patients = 480)
    trial$update_milestone(
      name = 'final',
      when = eventNumber(endpoint = 'pfs', n = max_events) &
        eventNumber(endpoint = 'pfs', n = 65, patient_id <= 80))

  }
  ## 'unfavorable' and 'favorable': no adaptation

  trial$save(value = decision, name = 'decision')
  trial$save(value = cp_full, name = 'cp_full')
  trial$save(value = cp_sub, name = 'cp_subgroup')
}

stage1_action <- function(trial){

  if(trial$get_output('decision') == 'futility'){
    ## no NA placeholders: columns a replicate never saves are filled
    ## with NA automatically when outputs are combined (see the vignette)
    return(invisible(NULL))
  }

  trial$stop_followup(patient_id <= 80, additional_followup = 0)

  locked_data <- trial$get_locked_data('stage 1 analysis')

  pf1 <- fitLogrank(Surv(pfs, pfs_event) ~ arm, placebo = 'pbo',
                    data = locked_data, alternative = 'less',
                    patient_id <= 80)$p
  ps1 <- fitLogrank(Surv(pfs, pfs_event) ~ arm, placebo = 'pbo',
                    data = locked_data, alternative = 'less',
                    patient_id <= 80 & biomarker == 1)$p

  trial$save(value = pf1, name = 'pf1')
  trial$save(value = ps1, name = 'ps1')
  trial$save(value = min(2 * min(pf1, ps1), max(pf1, ps1)), name = 'simes1')
}

final_action <- function(trial){

  locked_data <- trial$get_locked_data('final')
  decision <- trial$get_output('decision')

  trial$save(value = sum(locked_data$biomarker), name = 'n_positive')

  if(decision == 'futility'){
    ## FALSE is the true value here (a stopped trial rejects nothing),
    ## not a placeholder: it keeps mean(reject_*) unconditional
    trial$save(value = FALSE, name = 'reject_full')
    trial$save(value = FALSE, name = 'reject_subgroup')
    return(invisible(NULL))
  }

  pf1 <- trial$get_output('pf1')
  ps1 <- trial$get_output('ps1')
  simes1 <- trial$get_output('simes1')

  if(decision == 'enrichment'){
    ps2 <- fitLogrank(Surv(pfs, pfs_event) ~ arm, placebo = 'pbo',
                      data = locked_data, alternative = 'less',
                      patient_id > 80 & biomarker == 1)$p
    pf2 <- 1
    simes2 <- ps2
  }else{
    pf2 <- fitLogrank(Surv(pfs, pfs_event) ~ arm, placebo = 'pbo',
                      data = locked_data, alternative = 'less',
                      patient_id > 80)$p
    ps2 <- fitLogrank(Surv(pfs, pfs_event) ~ arm, placebo = 'pbo',
                      data = locked_data, alternative = 'less',
                      patient_id > 80 & biomarker == 1)$p
    simes2 <- min(2 * min(pf2, ps2), max(pf2, ps2))
  }

  trial$save(value = pf2, name = 'pf2')
  trial$save(value = ps2, name = 'ps2')
  trial$save(value = simes2, name = 'simes2')

  w1 <- sqrt(65 / 240)
  w2 <- sqrt(175 / 240)
  combine <- function(p1, p2){
    1 - pnorm(w1 * qnorm(1 - p1) + w2 * qnorm(1 - p2))
  }
  pf <- combine(pf1, pf2)
  ps <- combine(ps1, ps2)
  simes <- combine(simes1, simes2)

  trial$save(value = (simes <= .025 && pf <= .025), name = 'reject_full')
  trial$save(value = (simes <= .025 && ps <= .025), name = 'reject_subgroup')
}

interim <- milestone(name = 'interim', action = interim_action,
                     when = eventNumber(endpoint = 'pfs', n = 45, patient_id <= 80))
stage1 <- milestone(name = 'stage 1 analysis', action = stage1_action,
                    when = eventNumber(endpoint = 'pfs', n = 65, patient_id <= 80))
final <- milestone(name = 'final', action = final_action,
                   when = eventNumber(endpoint = 'pfs', n = 240) &
                     eventNumber(endpoint = 'pfs', n = 65, patient_id <= 80))

listener <- listener(silent = TRUE)
listener$add_milestones(interim, stage1, final)
controller <- controller(trial, listener)
