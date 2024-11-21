

os_final_action <- function(trial, event_name){

  locked_data <- trial$get_locked_data(event_name)
  ## do something with locked_data, e.g. estimate, test, etc.,
  ## to support further actions
  ## ...

  ## this is just for illustration
  os_final_pval <- runif(1, 1e-3, .1)

  ## action 1: end the trial, no futher action
  action <- paste0('No action. P-value: <os final = ',
                   signif(os_final_pval, 2), '>. ')

  ## return for displaying purpose
  action

}


os_final_event <- Event$new(name = 'os final', type = 'final analysis',
                               trigger_condition = TriggerByEventNumbers,
                               action = os_final_action,
                               endpoints = 'orr',
                               target_n_events = n_patients)

