

## ----setup, echo = FALSE, message = FALSE---------------------------------------
library(dplyr)
library(R6)
library(kableExtra)
library(rlang)
library(survival)
library(survminer)
library(ggplot2)
library(TrialSimulator)


## ----daoif, class.source='fold-show'--------------------------------------------
weibullDropout(time = c(12, 18), dropout_rate = c(.08, .18))


## ----ljgai, class.source="fold-show"--------------------------------------------
#' define three arms
pbo <- arm(name = 'placebo')
low <- arm(name = 'low dose')
high <- arm(name = 'high dose')

#' define endpoints in placebo
pfs <- endpoint(name = 'pfs', type = 'tte',
                generator = rexp, rate = log(2) / 5)

os <- endpoint(name = 'os', type = 'tte',
               generator = rexp, rate = log(2) / 14)

five_weeks <- 5 / 52 * 12 ## convert it in months
surrogate <- endpoint(name = 'surrogate', type = 'non-tte',
                      readout = c(surrogate = five_weeks),
                      generator = rbinom, size = 1, prob = .05)
pbo$add_endpoints(pfs, os, surrogate)

#' define endpoints in low dose arm
pfs <- endpoint(name = 'pfs', type = 'tte',
                generator = rexp, rate = log(2) / 6.7)

os <- endpoint(name = 'os', type = 'tte',
               generator = rexp, rate = log(2) / 17.5)

surrogate <- endpoint(name = 'surrogate', type = 'non-tte',
                      readout = c(surrogate = five_weeks),
                      generator = rbinom, size = 1, prob = .12)
low$add_endpoints(pfs, os, surrogate)

#' define endpoints in high dose arm
pfs <- endpoint(name = 'pfs', type = 'tte',
                generator = rexp, rate = log(2) / 7.1)

os <- endpoint(name = 'os', type = 'tte',
               generator = rexp, rate = log(2) / 18.2)

surrogate <- endpoint(name = 'surrogate', type = 'non-tte',
                      readout = c(surrogate = five_weeks),
                      generator = rbinom, size = 1, prob = .13)
high$add_endpoints(pfs, os, surrogate)


## ----lagieg, class.source="fold-show"-------------------------------------------
accrual_rate <- data.frame(end_time = c(10, Inf),
                           piecewise_rate = c(30, 50))
trial <- trial(
  name = 'Trial-3415', n_patients = 1000,
  seed = 1727811904, duration = 40,
  enroller = StaggeredRecruiter, accrual_rate = accrual_rate,
  dropout = rweibull, shape = 2.139, scale = 38.343
)

trial$add_arms(sample_ratio = c(1, 1, 1), low, high, pbo)


## ----ieaong, class.source='fold-show'-------------------------------------------
action1 <- function(trial, milestone_name){

  locked_data <- trial$get_locked_data(milestone_name)

  fit <- fitFarringtonManning(endpoint = 'surrogate', placebo = 'placebo',
                              data = locked_data, alternative = 'greater')

  # browser() ## if you want to see what does fit look like
  z_l <- fit$z[fit$arm == 'low dose']
  z_h <- fit$z[fit$arm == 'high dose']
  if(z_l > 1.28){
    trial$remove_arms('high dose')
    trial$save(value = 'low', name = 'kept_arm')
  }else if(z_h > 1.28){
    trial$remove_arms('low dose')
    trial$save(value = 'high', name = 'kept_arm')
  }else{
    trial$save(value = 'both', name = 'kept_arm')
  }

  invisible(NULL)

}


## ----aotel, class.source="fold-show"--------------------------------------------
action2 <- function(trial, milestone_name){

  locked_data <- trial$get_locked_data(milestone_name)

  fit <- fitLogrank(Surv(pfs, pfs_event) ~ arm, placebo = 'placebo',
                    data = locked_data, alternative = 'less')

  ## futility analysis
  if(max(fit$z) < .5){
    trial$save(value = 'negative', name = 'futility')

    ## extend duration
    ## trial$set_duration(45)
  }else{
    trial$save(value = 'positive', name = 'futility')
  }

  invisible(NULL)

}


## ----alkdae, class.source="fold-show"-------------------------------------------
action3 <- function(trial, milestone_name){

  locked_data <- trial$get_locked_data(milestone_name)

  browser()
  ## test PFS
  dt_pfs <- trial$dunnettTest(Surv(pfs, pfs_event) ~ arm, placebo = 'placebo',
                              treatments = c('high dose', 'low dose'),
                              milestones = c('dose selection', 'interim', 'final'),
                              planned_info = 'default',
                              alternative = 'less')

  ct_pfs <- trial$closedTest(dt_pfs, treatments = c('high dose', 'low dose'),
                             milestones = c('interim', 'final'),
                             alpha = .005, alpha_spending = 'asOF')

  ## test OS
  dt_os <- trial$dunnettTest(endpoint = 'os', placebo = 'placebo',
                             treatments = c('high dose', 'low dose'),
                             milestones = c('dose selection', 'final'),
                             planned_info = 'default')

  ct_os <- trial$closedTest(dt_pfs, treatments = c('high dose', 'low dose'),
                            milestones = c('final'),
                            alpha = .02, alpha_spending = 'asOF')

  ## we only save testing decision here
  ## You can save whatever you want for summarizing things later, e.g. reject time
  trial$save(value = ct_pfs$decision[ct_pfs$arm == 'high dose'],
             name = 'pfs_high_dose_decision')

  trial$save(value = ct_pfs$decision[ct_pfs$arm == 'low dose'],
             name = 'pfs_low_dose_decision')

  trial$save(value = ct_os$decision[ct_os$arm == 'high dose'],
             name = 'os_high_dose_decision')

  trial$save(value = ct_os$decision[ct_os$arm == 'low dose'],
             name = 'os_low_dose_decision')

  invisible(NULL)
}


## -------------------------------------------------------------------------------
dose_selection <- milestone(name = 'dose selection', action = action1,
                            when = eventNumber(endpoint = 'surrogate', n = 300)
                            )

interim <- milestone(name = 'interim', action = action2,
                     when = eventNumber(endpoint = 'pfs', n = 300)
                     )

final <- milestone(name = 'final', action = action3,
                   when = enrollment(n = 1000, arms = c('placebo', 'low dose', 'high dose')) &
                     eventNumber(endpoint = 'os', n = 300) & (
                       calendarTime(time = 28) |
                         eventNumber(endpoint = 'pfs', n = 520)
                       )
                   )

listener <- listener()
#' register milestones with listener
listener$add_milestones(
  dose_selection,
  interim,
  final
)


## ----eiaaf, dpi = 1200----------------------------------------------------------
controller <- controller(trial, listener)
controller$run(plot_event = TRUE)


## ----alojfqoitl, class.source="fold-show"---------------------------------------
controller$get_output() %>%
  kable(escape = FALSE) %>%
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                position = "left") %>%
  scroll_box(width = "100%")


## ----aief, class.source='fold-show'---------------------------------------------
## test PFS
dt_pfs <- trial$dunnettTest(endpoint = 'pfs', placebo = 'placebo',
                            treatments = c('high dose', 'low dose'),
                            milestones = c('dose selection', 'interim', 'final'),
                            planned_info = 'default')
ct_pfs <- trial$closedTest(dt_pfs, treatments = c('high dose', 'low dose'),
                           milestones = c('interim', 'final'),
                           alpha = .005, alpha_spending = 'asOF')

## test OS
dt_os <- trial$dunnettTest(endpoint = 'os', placebo = 'placebo',
                           treatments = c('high dose', 'low dose'),
                           milestones = c('dose selection', 'final'),
                           planned_info = 'default')
ct_os <- trial$closedTest(dt_pfs, treatments = c('high dose', 'low dose'),
                          milestones = c('final'),
                          alpha = .02, alpha_spending = 'asOF')

print(ct_pfs)
print(ct_os)


## ----ioeinaf, eval=FALSE, cache=TRUE, message=FALSE, warning=FALSE, results='hide'----
# ## reset a controller if $run has been executed before
# controller$reset()
# controller$run(n = 1000, plot_event = FALSE, silent = TRUE)
# output <- controller$get_output()


## ----eaiofj, echo=FALSE---------------------------------------------------------
output <- TrialSimulator:::getAdaptiveDesignOutput()


## ----liefa----------------------------------------------------------------------
output %>%
  head(10) %>%
  kable(escape = FALSE) %>%
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                position = "left") %>%
  scroll_box(width = "100%")


## ----iojalf---------------------------------------------------------------------
output %>%
  summarise(
    time_dose_selection = mean(`milestone_time_<dose selection>`),
    time_interim = mean(`milestone_time_<interim>`),
    time_final = mean(`milestone_time_<final>`),
    n_dose_selection = mean(`n_events_<dose selection>_<patient_id>`),
    n_interim = mean(`n_events_<interim>_<patient_id>`),
    n_final = mean(`n_events_<final>_<patient_id>`),
    low = mean(kept_arm == 'low') * 100,
    high = mean(kept_arm == 'high') * 100,
    both = mean(kept_arm == 'both') * 100
  ) %>%
  kable(col.names = NULL, digits = 1, align = 'r',
        caption = 'Number of Randomized Patients at Stages') %>%
  add_header_above(c(rep(c('Dose Selection', 'Interim', 'Final'), 2),
                     'Low Dose', 'High Dose', 'Both'), align = 'r') %>%
  add_header_above(c('Time' = 3, 'Number of Patients' = 3, 'Selected Dose (%)' = 3)) %>%
  kable_styling(full_width = TRUE)


## ----adalf----------------------------------------------------------------------
output %>%
  summarise(
    n_pfs_dose_selection = mean(`n_events_<dose selection>_<pfs>`),
    n_pfs_interim = mean(`n_events_<interim>_<pfs>`),
    n_pfs_final = mean(`n_events_<final>_<pfs>`),
    n_os_dose_selection = mean(`n_events_<dose selection>_<os>`),
    n_os_interim = mean(`n_events_<interim>_<os>`),
    n_os_final = mean(`n_events_<final>_<os>`)
  ) %>%
  kable(col.names = NULL, digits = 1, align = 'r',
        caption = 'Number of Events of PFS and OS at Stages') %>%
  add_header_above(rep(c('Dose Selection', 'Interim', 'Final'), 2), align = 'r') %>%
  add_header_above(c('PFS' = 3, 'OS' = 3)) %>%
  kable_styling(full_width = TRUE)


## ----dgslja---------------------------------------------------------------------
output %>%
  summarise(
    power_pfs_low = mean(pfs_low_dose_decision == 'reject') * 100,
    power_pfs_high = mean(pfs_high_dose_decision == 'reject') * 100,
    power_pfs_or = mean(pfs_low_dose_decision == 'reject' | pfs_high_dose_decision == 'reject') * 100,
    power_pfs_and = mean(pfs_low_dose_decision == 'reject' & pfs_high_dose_decision == 'reject') * 100,
    power_os_low = mean(os_low_dose_decision == 'reject') * 100,
    power_os_high = mean(os_high_dose_decision == 'reject') * 100,
    power_os_or = mean(os_low_dose_decision == 'reject' | os_high_dose_decision == 'reject') * 100,
    power_os_and = mean(os_low_dose_decision == 'reject' & os_high_dose_decision == 'reject') * 100
  ) %>%
  kable(col.names = NULL, digits = 1, align = 'r',
        caption = 'Power of Testing PFS and OS') %>%
  add_header_above(rep(c('Low Dose', 'High Dose', 'Low or High', 'Low and High'), 2), align = 'r') %>%
  add_header_above(c('PFS (%)' = 4, 'OS (%)' = 4)) %>%
  kable_styling(full_width = TRUE)


## ----eioajf---------------------------------------------------------------------
output %>%
  summarise(
    power_pfs_not_os = mean((pfs_low_dose_decision == 'reject' | pfs_high_dose_decision == 'reject') &
                              os_low_dose_decision == 'accept' & os_high_dose_decision == 'accept') * 100,
    power_os_not_pfs = mean((os_low_dose_decision == 'reject' | os_high_dose_decision == 'reject') &
                              pfs_low_dose_decision == 'accept' & pfs_high_dose_decision == 'accept') * 100,
    power_pfs_and_os = mean((pfs_low_dose_decision == 'reject' | pfs_high_dose_decision == 'reject') &
                              (os_low_dose_decision == 'reject' | os_high_dose_decision == 'reject')) * 100,
    power_pfs_or_os = mean((pfs_low_dose_decision == 'reject' | pfs_high_dose_decision == 'reject') |
                              (os_low_dose_decision == 'reject' | os_high_dose_decision == 'reject')) * 100
  ) %>%
  kable(col.names = NULL, digits = 1, align = 'r',
        caption = 'Power of Testing PFS and OS (Cont.)') %>%
  add_header_above(c('Reject PFS and Accept OS', 'Accept PFS and Reject OS',
                     'Reject PFS and OS', 'Reject PFS or OS'), align = 'r') %>%
  kable_styling(full_width = TRUE)

