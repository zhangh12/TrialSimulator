

pfs_final_action <- function(trial, event_name){

  locked_data <- trial$get_locked_data(event_name)
  ## do something with locked_data, e.g. estimate, test, etc.,
  ## to support further actions
  ## ...

  ## this is just for illustration
  pfs_final_pval <- runif(1, 0, 0.1)
  os_interim_pval <- runif(1, 0, 0.3)

  ## only compute p-values for pfs and os, no further action
  action <- paste0('No action. P-values: <pfs final = ',
                   signif(pfs_final_pval, 2),
                   '>, <os interim = ', signif(os_interim_pval, 2), '>.')

  ## return for displaying purpose
  action

}


## meet = 'any' means that final analysis for PFS is triggered
## when pfs event reaches 300 or os event reaches 100.
## if meet = 'all', then final analysis for PFS is carried out only if
## both conditions are met.
pfs_final_event <- Event$new(name = 'pfs final', type = 'final analysis',
                               trigger_condition = TriggerByEventNumbers,
                               action = pfs_final_action,
                               endpoints = c('pfs', 'os'),
                               target_n_events = c(600, 500), meet = 'any')

