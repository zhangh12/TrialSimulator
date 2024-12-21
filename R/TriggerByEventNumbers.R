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
#' @param arms a vector of arms' name on which the number of events is counted.
#' By default it is \code{NULL} if all arms are used. Sometimes one may want to
#' count number of events on a subset of arms in a trial, for example, after
#' an arm is removed, number of events to trigger the next event/action may be
#' based on the remaining arms only.
#' @param meet \code{'all'} if all target event numbers need to be met before
#' triggering the event. \code{'any'} if triggering the event when the first
#' target is met for corresponding endpoint.
#' @return data lock time (calendar), so that the `Event` class can lock data.
#' @export
TriggerByEventNumbers <-
  function(trial = NULL, event_name, endpoints, target_n_events,
           arms = NULL, meet = c('all', 'any')){

    if(is.null(trial)){
      stop('trial is needed in TriggerByEventNumbers to trigger an event in. ')
    }

    if(is.null(arms)){
      arms <- trial$get_arms_name()
    }

    if(!all(arms %in% trial$get_arms_name())){
      stop('Arm(s) <',
           paste0(setdiff(arms, trial$get_arms_name()), collapse = ', '),
           '> cannot be found in the trial, ',
           'thus cannot be used to define the event <', event_name, '>. ')
    }

    meet <- match.arg(meet)
    data_lock_time <-
      trial$get_data_lock_time_by_event_number(endpoints, arms,
                                               target_n_events, type = meet)

    attr(data_lock_time, 'event_name') <- event_name
    return(data_lock_time)

}
