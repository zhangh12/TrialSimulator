knitr::opts_chunk$set(
  collapse = TRUE,
  cache.path = 'cache/fixedDesign/',
  comment = '#>',
  dpi = 300,
  out.width = '100%'
)

library(dplyr)
library(kableExtra)
library(TrialSimulator)
set.seed(12345)

pars_soc <- solveThreeStateModel(median_pfs = 7, median_os = 15, corr = .68, 
                                 h12 = seq(.07, .10, length.out = 50))
pars_soc

pars_low <- solveThreeStateModel(median_pfs = 9, median_os = 18.5, corr = .65, 
                                 h12 = seq(.04, .07, length.out = 50))
pars_low

pars_high <- solveThreeStateModel(median_pfs = 10, median_os = 20, corr = .60, 
                                  h12 = seq(.02, .06, length.out = 50))
pars_high

rbind(pars_soc, pars_low, pars_high) %>% 
  structure(class = 'data.frame') %>% 
  mutate(arm = c('soc', 'low', 'high')) %>% 
  select(arm, h01, h02, h12) %>% 
  kable(format = 'html', digits = 3, 
        caption = 'Transition hazards in three treatment arms')

#' define SoC
pfs_os_in_soc <- endpoint(name = c('pfs', 'os'), 
                          type = c('tte', 'tte'), 
                          generator = CorrelatedPfsAndOs3, 
                          h01 = 0.075, h02 = 0.024, h12 = 0.090)

soc <- arm(name = 'soc')
soc$add_endpoints(pfs_os_in_soc)

#' define low dose arm
pfs_os_in_low <- endpoint(name = c('pfs', 'os'), 
                          type = c('tte', 'tte'), 
                          generator = CorrelatedPfsAndOs3, 
                          h01 = 0.051, h02 = 0.026, h12 = 0.062)

low <- arm(name = 'low')
low$add_endpoints(pfs_os_in_low)

#' define high dose arm
pfs_os_in_high <- endpoint(name = c('pfs', 'os'), 
                           type = c('tte', 'tte'), 
                           generator = CorrelatedPfsAndOs3, 
                          h01 = 0.040, h02 = 0.030, h12 = 0.047)

high <- arm(name = 'high')
high$add_endpoints(pfs_os_in_high)

high

accrual_rate <- data.frame(end_time = c(10, Inf),
                           piecewise_rate = c(30, 50))
trial <- trial(
  name = 'Trial-3415', n_patients = 1000,
  seed = 1727811904, duration = 500,
  enroller = StaggeredRecruiter, accrual_rate = accrual_rate,
  dropout = rexp, rate = -log(1 - 0.1)/18, ## 10% by month 18
  silent = TRUE
)

trial$add_arms(sample_ratio = c(1, 1, 1), soc, low, high) ## 1:1:1
trial

action <- function(trial){
  
  locked_data <- trial$get_locked_data('final')
  
  pfs <- fitCoxph((Surv(pfs, pfs_event) ~ arm), placebo = 'soc', 
                  data = locked_data, alternative = 'less', 
                  scale = 'hazard ratio')
  
  os <- fitLogrank((Surv(os, os_event) ~ arm), placebo = 'soc', 
                   data = locked_data, alternative = 'less')
  
  ## Bonferroni test is applied to four hypotheses: 
  ## PFS_low, PFS_high, OS_low, and OS_high
  pfs$decision <- ifelse(pfs$p < .05/4, 'reject', 'accept')
  os$decision <- ifelse(os$p < .05/4, 'reject', 'accept')
  
  trial$save(
    value = pfs %>% filter(arm == 'low') %>% select(estimate, decision, info), 
    name = 'pfs_low')
  
  trial$save(
    value = pfs %>% filter(arm == 'high') %>% select(estimate, decision, info), 
    name = 'pfs_high')
  
  trial$save(
    value = os %>% filter(arm == 'low') %>% select(decision, info), 
    name = 'os_low')
  
  trial$save(
    value = os %>% filter(arm == 'high') %>% select(decision, info), 
    name = 'os_high')
  
}

final <- milestone(name = 'final', action = action, 
                   when = eventNumber(endpoint = 'pfs', n = 450, 
                                      arms = c('soc', 'high')) & 
                     eventNumber(endpoint = 'os', n = 550)
                   )

listener <- listener()
listener$add_milestones(final)

controller <- controller(trial, listener)
