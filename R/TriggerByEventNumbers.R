#' A trigger_condition function
#' @description
#' A trigger_condition function to find data lock time when target event numbers
#' are met for specified endpoints. This can be passed to trigger_condition
#' when creating an `Event`.
#' @param trial a \code{Trial} object.
#' @param event_name character. Name of event. It should be the same as
#' \code{Event$get_name()}.
#' @param endpoints character vectors. Endpoints to compute event numbers.
#' @param target_n_events target event numbers for \code{endpoints} to trigger
#' the event.
#' @param meet \code{'all'} if all target event numbers need to be met before
#' triggering the event. \code{'any'} if triggering the event when the first
#' target is met for corresponding endpoint.
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
