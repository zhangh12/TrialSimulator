dose_selection_action <- function(trial, event_name){

  locked_data <- trial$get_locked_data(event_name)
  ## do something with locked_data, e.g. estimate, Dunnett test, etc.,
  ## to support further actions
  ## ...

  ## this is just for illustration
  ## pick an arm to drop
  affected_arm <- sample(c('high dose', 'low dose'), size = 1, prob = c(.6, .4))

  ## action 1: remove an arm <affected_arm>
  trial$remove_arms(affected_arm)

  ## action 2: update sample ratio for the remaining arms
  trial$update_sample_ratio('pbo', 1)
  if(affected_arm == 'high dose'){
    ## if low dose arm is kept, increase its ratio
    trial$update_sample_ratio('low dose', 2)
  }else{
    ## if high dose arm is kept, reduce its ratio for safety
    trial$update_sample_ratio('high dose', 1)
  }

  # action 3: non-binding futility analysis
  z_pfs <- rnorm(1)

  ## return for displaying purpose (print on screen later)
  ## user can return NULL
  action <- data.frame(
    order = c(1, 2, 3),
    type = c('remove', 'update sample ratio', 'non-binding futility'),
    value = c(affected_arm,
              ifelse(affected_arm == 'high dose', 'low dose -> 2', 'high dose -> 1'),
              paste0('z (pfs) = ', round(z_pfs, 2)))
  )

  action

}

## dose selection is carried out at month 14
dose_selection_event <- Event$new(name = 'dose selection', type = 'dose selection',
                                  trigger_condition = TriggerByCalendarTime,
                                  action = dose_selection_action,
                                  calendar_time = 14)
