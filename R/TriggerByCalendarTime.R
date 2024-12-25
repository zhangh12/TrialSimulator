#' A trigger_condition function
#' @description
#' A trigger_condition function to find data lock time at specified calendar
#' time. This can be passed to trigger_condition when creating an `Event`.
#' @param trial a \code{Trial} object.
#' @param event_name character. Name of event. It should be the same as
#' \code{Event$get_name()}.
#' @param calendar_time numeric. Calendar time to trigger the event.
#' @param arms a vector of arms' name on which triggering condition is tested.
#' @return data lock time (calendar), so that the `Event` class can lock data.
#' @export
TriggerByCalendarTime <-
  function(trial = NULL, event_name, calendar_time, arms = NULL){

    if(is.null(trial)){
      stop('trial is needed in TriggerByCalendarTime to trigger an event in. ')
    }
    stopifnot(is.numeric(calendar_time) && length(calendar_time) == 1)

    if(is.null(arms)){
      arms <- trial$get_arms_name()
    }

    if(!all(arms %in% trial$get_arms_name())){
      stop('Arm(s) <',
           paste0(setdiff(arms, trial$get_arms_name()), collapse = ', '),
           '> cannot be found in the trial, ',
           'thus cannot be used to define the event <', event_name, '>. ')
    }

    data_lock_time <-
      trial$get_data_lock_time_by_calendar_time(calendar_time, arms)

    attr(data_lock_time, 'event_name') <- event_name
    return(data_lock_time)

  }
