#' A trigger_condition function
#' @description
#' A trigger_condition function to find data lock time when target event numbers
#' are met for specified endpoints. This can be passed to trigger_condition
#' when creating an `Event`.
#' @return data lock time (calendar).
#' @export
TriggerByEventNumbers <-
  function(trial = NULL, event_name, endpoints, target_n_events, meet = c('all', 'any')){

    if(is.null(trial)){
      stop('trial is needed in TriggerByEventNumbers to trigger an event in. ')
    }
    meet <- match.arg(meet)
    data_lock_time <- trial$get_data_lock_time(endpoints, target_n_events, type = meet)

    attr(data_lock_time, 'event_name') <- event_name
    return(data_lock_time)

}
