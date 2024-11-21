

pfs_interim_action <- function(trial, event_name){

  locked_data <- trial$get_locked_data(event_name)
  ## do something with locked_data, e.g. estimate, test, etc.,
  ## to support further actions
  ## ...

  ## this is just for illustration
  pfs_interim_pval <- runif(1, 1e-3, .05)

  ## action 1: if pfs is significant, increase its sample ratio
  ##           and reduce sample ratio of pbo.
  ##           otherwise no action.
  action <- paste0('No action. P-value: <pfs interim = ',
                   signif(pfs_interim_pval, 2), '>. ')
  if(pfs_interim_pval < .05){
    active_arm <- setdiff(trial$get_arms_name(), 'pbo')
    trial$update_sample_ratio(active_arm, 2)
    trial$update_sample_ratio('pbo', 1)

    action <- data.frame(
      order = c(1, 2),
      type = 'update sample ratio',
      value = c(paste0(active_arm, ' -> 2'), 'pbo -> 1')
    )
  }

  ## return for displaying purpose
  action

}


pfs_interim_event <- Event$new(name = 'pfs interim', type = 'interim analysis',
                            trigger_condition = TriggerByEventNumbers,
                            action = pfs_interim_action,
                            endpoints = 'pfs',
                            target_n_events = 100)

