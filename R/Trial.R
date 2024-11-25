#' Class of Trial
#' @description
#' Create a class of trial.
#'
#' @docType class
#' @examples
#' risk1 <- data.frame(
#'   end_time = c(1, 10, 26.0, 52.0),
#'   piecewise_risk = c(1, 1.01, 0.381, 0.150) * exp(-3.01)
#' )
#'
#' pfs1 <- Endpoint$new(name = 'pfs', type='tte', method='piecewise_const_exp',
#'   risk = risk1)
#' orr1 <- Endpoint$new(
#'   name = 'orr', type = 'binary', generator = rbinom,
#'   size = 1, prob = .4)
#' placebo <- Arm$new(
#'   name = 'pbo', description = 'Placebo arm')
#'
#' placebo$add_endpoints(pfs1, orr1)
#'
#' risk2 <- risk1
#' risk2$hazard_ratio <- .8
#' pfs2 <- Endpoint$new(name = 'pfs', type='tte', method='piecewise_const_exp',
#'   risk = risk2)
#' orr2 <- Endpoint$new(
#'   name = 'orr', type = 'binary', generator = rbinom,
#'   size = 1, prob = .6)
#' active <- Arm$new(
#'   name = 'ac', description = 'Active arm')
#'
#' active$add_endpoints(pfs2, orr2)
#'
#' ## Plan a trial, Trial-3415, of up to 100 patients.
#' ## Enrollment time follows an exponential distribution, with median 5
#' trial <- Trial$new(
#'   name = 'Trial-3415', n_patients = 100,
#'   seed = 31415926,
#'   enroller = rexp, rate = log(2) / 5)
#' trial$add_arms(sample_ratio = c(1, 2), placebo, active)
#'
#' @export
Trial <- R6::R6Class(
  'Trial',

  public = list(

    #' @description
    #' initialize a trial
    #' @param name character. Name of trial.
    #' @param n_patients integer. Maximum number of patients could be enrolled
    #' to the trial.
    #' @param description character. Optional for description of the trial. By
    #' default it is set to be trial's \code{name}.
    #' @param seed random seed. If \code{NULL}, \code{set.seed()} will not be
    #' called, which uses seed set outside.
    #' @param enroller a function returning a vector enrollment time for
    #' patients. Its first argument is the number of enrolled patients.
    #' @param ... arguments of \code{enroller}.
    initialize =
      function(
        name,
        n_patients,
        description = name,
        seed,
        enroller,
        ...
      ){

        private$validate_arguments(
          name, n_patients, description, seed, enroller, ...)

        if(!is.null(seed)){
          set.seed(seed)
        }

        private$arms <- list()
        private$name <- name
        private$description <- description
        private$n_patients <- n_patients
        private$now <- 0
        private$trial_data <- NULL
        private$locked_data <- list()

        private$enroller <- DynamicRNGFunction(enroller, simplify = TRUE, ...)
        stopifnot(is.vector(private$enroller(2)))

        ## sort enrollment time
        private$enroll_time <-
          sort(private$enroller(n_patients), decreasing = FALSE)

      },

    #' @description
    #' return trial data of enrolled patients at the time of this
    #' function is called
    get_trial_data = function(){
      private$trial_data
    },

    #' @description
    #' roll back data to current time of trial. By doing so,
    #' \code{Trial$trial_data} will be cut at current time, and data after then
    #' are deleted. However, \code{Trial$enroll_time} after current time are
    #' kept unchanged because that is planned enrollment curve.
    roll_back = function(){

      current_time <- self$get_current_time()

      private$enroll_time <- (self$get_trial_data() %>%
        dplyr::filter(enroll_time > current_time) %>%
        arrange(enroll_time))$enroll_time

      private$trial_data <- self$get_trial_data() %>%
        dplyr::filter(enroll_time <= current_time) %>%
        arrange(enroll_time)
      message('Trial data is rolling back to time = ', current_time, '. \n',
              'Randomization will be carried out again for unenrolled patients. \n')

    },

    #' @description
    #' remove arms from a trial. \code{enroll_patients()} will be always called
    #' at the end to enroll all remaining patients after
    #' \code{Trial$get_current_time()}. This function may be used with futility
    #' analysis, dose selection, enrichment analysis (sub-population) or
    #' interim analysis (early stop for efficacy)
    #' @param arms_name character vector. Name of arms to be removed.
    remove_arms = function(arms_name){
      stopifnot(is.character(arms_name))
      stopifnot(all(arms_name %in% self$get_arms_name()))

      private$sample_ratio <-
        private$sample_ratio[!(names(private$sample_ratio) %in% arms_name)]

      for(arm_name in arms_name){
        private$arms[[arm_name]] <- NULL
      }

      message('Arm <', paste0(arms_name, collapse = ', '), '> is removed. \n')

      message('Sample ratio is updated to be <',
              paste0(paste0(names(self$get_sample_ratio()),
                            ': ', self$get_sample_ratio()), collapse = ', '),
              '>. \n')

      ## with an arm is removed, unenrolled patient at current time should be
      ## randomized again.
      self$roll_back()

      ## update randomization plan for unenrolled patients
      private$permuted_block_randomization()

      ## update data for unrolled patients based on new arms and possibly
      ## new sample ratio.
      self$enroll_patients()


    },

    #' @description
    #' update sample ratio of an arm. This could happen after an arm is added
    #' or removed. We may want to update sample ratio of unaffected arms as
    #' well. This function can only update sample ratio for one arm at a time.
    #' Once sample ratio is updated, trial data should be rolled back with
    #' updated randomization queue. Data of unenrolled patients should be
    #' re-sampled as well.
    #' @param arm_name character. Name of an arm of length 1.
    #' @param sample_ratio integer. Sample ratio of the arm.
    update_sample_ratio = function(arm_name, sample_ratio){

      stopifnot(is.character(arm_name))
      stopifnot(length(arm_name) == 1)
      stopifnot(is.numeric(sample_ratio) && all(is.wholenumber(sample_ratio)))
      stopifnot(length(sample_ratio) == 1)
      if(is.null(private$arms[[arm_name]])){
        stop('Arm ', arm_name, ' is not in the trial. ')
      }

      if(!(arm_name %in% names(self$get_sample_ratio()))){
        stop('Sample ratio of arm ', arm_name, ' is not in the trial. \n',
             'Usually this means issue in the package. Debug it. ')
      }

      private$sample_ratio[arm_name] <- sample_ratio
      message('Sample ratio has been udpated to be <',
              paste0(paste0(names(self$get_sample_ratio()),
                            ': ', self$get_sample_ratio()), collapse = ', '),
              '>. ')

      ## with sample ratio of an arm is updated, unenrolled patient at current
      ## time should be randomized again.
      self$roll_back()

      ## update randomization plan for unenrolled patients
      private$permuted_block_randomization()

      ## update data for unrolled patients based on new arms and possibly
      ## new sample ratio.
      self$enroll_patients()
    },


    #' @description
    #' add one or more arms to the trial. \code{enroll_patients()} will be
    #' called at the end to enroll all remaining patients in
    #' \code{private$randomization_queue}. This function can be used in two
    #' scenarios.
    #' (1) add arms right after a trial is created (i.e., \code{Trial$new(...)}).
    #' \code{sample_ratio} and arms added through \code{...} should be of same
    #' length.
    #' (2) add arms to a trial already with arm(s)
    #' @param sample_ratio integer vector. Sample ratio for permuted block
    #' randomization. It will be appended to existing sample ratio in the trial.
    #' @param ... one or more objects of class \code{Arm}. One exception in
    #' \code{...} is an argument \code{enforce}. When \code{enforce = TRUE},
    #  it makes sure randomization is carried out with updated
    #' sample ratio of newly added arm. It rolls back all patients after
    #' \code{Trial$get_current_time()}, i.e. redo randomization for those
    #' patients. This can be useful to add arms one by one when creating a trial.
    #' Note that we can run \code{Trial$add_arm(sample_ratio1, arm1)} followed
    #' by \code{Trial$add_arm(sample_ratio2, enforce = TRUE, arm2)}.
    #' We would expected similar result with
    #' \code{Trial$add_arms(c(sample_ratio1, sample_ratio2), arm1, arm2)}. Note
    #' that these two method won't return exactly the same trial because
    #' randomization_queue were generated twice in the first approach but only
    #' once in the second approach. But statistically, they are equivalent and
    #' of the same distribution.
    add_arms = function(sample_ratio, ...){

      stopifnot(is.numeric(sample_ratio) && all(is.wholenumber(sample_ratio)))

      arm_list <- list(...)
      enforce <- arm_list$enforce
      if(is.null(enforce)){
        enforce <- FALSE
      }

      arm_list$enforce <- NULL
      stopifnot(length(arm_list) == length(sample_ratio))

      arm_names <- NULL
      for(arm in arm_list){
        stopifnot(inherits(arm, 'Arm'))

        if(arm$get_name() %in% self$get_arms_name()){
          stop('Arm ', arm$get_name(), ' already exists in the trial. ',
               'Do you want to update it instead? ',
               'Currently this is not supported. ')
        }
        arm_names <- c(arm_names, arm$get_name())
      }

      for(arm in arm_list){
        private$arms[[arm$get_name()]] <- arm
      }

      message('Arm(s) <', paste0(arm_names, collapse = ', '),
              '> are added to the trial. \n')

      names(sample_ratio) <- arm_names
      private$sample_ratio <- c(private$sample_ratio, sample_ratio)
      rm(sample_ratio)

      if(enforce){
        self$roll_back()
      }

      ## update randomization plan for unenrolled patients
      private$permuted_block_randomization()

      self$enroll_patients()

    },

    #' @description
    #' return name of trial
    get_name = function(){
      private$name
    },

    #' @description
    #' return description of trial
    get_description = function(){
      private$description
    },

    #' @description
    #' return a list of arms in the trial
    get_arms = function(){
      private$arms
    },

    #' @description
    #' return arms' name of trial
    get_arms_name = function(){
      lapply(private$arms, function(arm){arm$get_name()}) %>% unlist() %>% unname()
    },

    #' @description
    #' get number of arms in the trial
    get_number_arms = function(){
      length(private$arms)
    },

    #' @description
    #' return an arm
    #' @param arm_name character, name of arm to be extracted
    get_an_arm = function(arm_name){
      if(!(arm_name %in% self$get_arms_name())){
        stop(arm_name, ' is not in the trial \'', self$get_name(), '\'')
      }

      self$get_arms()[[arm_name]]
    },

    #' @description
    #' return current sample ratio of the trial. The ratio can probably change
    #' during the trial (e.g., arm is removed or added)
    #' @param arm_names character vector of arms.
    get_sample_ratio = function(arm_names = NULL){
      if(is.null(arm_names)){
        arm_names <- names(private$sample_ratio)
      }
      stopifnot(all(arm_names %in% names(private$sample_ratio)))
      private$sample_ratio[arm_names]
    },

    #' @description
    #' return number of patients when planning the trial
    get_number_patients = function(){
      private$n_patients
    },

    #' @description
    #' return number of enrolled (randomized) patients
    get_number_enrolled_patients = function(){
      if(is.null(self$get_trial_data())){
        return(0)
      }
      nrow(self$get_trial_data())
    },

    #' @description
    #' return number of unenrolled patients
    get_number_unenrolled_patients = function(){
      self$get_number_patients() - self$get_number_enrolled_patients()
    },

    #' @description
    #' return randomization queue of planned but not yet enrolled patients.
    #' This function does not update randomization_queue, just return its value
    #' for debugging purpose.
    #' @param index index to be extracted. Return all queue if \code{NULL}.
    get_randomization_queue = function(index = NULL){
      if(length(private$randomization_queue) == 0){
        private$randomization_queue <- NULL
      }

      if(!is.null(index) && is.null(private$randomization_queue)){
        stop('Cannot randomize patients from empty list. ')
      }

      if(is.null(index)){ # return all
        return(private$randomization_queue)
      }
      stopifnot(max(abs(index)) <= length(private$randomization_queue))

      private$randomization_queue[index]

    },

    #' @description
    #' return enrollment time of planned but not yet enrolled patients.
    #' This function does not update enroll_time, just return its value
    #' for debugging purpose.
    #' @param index index to extract. Return all enroll time if \code{NULL}.
    get_enroll_time = function(index = NULL){

      if(length(private$enroll_time) == 0){
        private$enroll_time <- NULL
      }

      if(!is.null(index) && is.null(private$enroll_time)){
        stop('Cannot enroll patients from empty list. ')
      }

      if(is.null(index)){ # return all
        return(private$enroll_time)
      }
      stopifnot(max(abs(index)) <= length(private$enroll_time))

      private$enroll_time[index]
    },

    #' @description
    #' assign a new patient to an arm based on planned randomization queue
    #' I may consider make this function deprecated. Instead, I'd like to
    #' have a function to sample the remaining n patients, with n as an argument.
    #' Reason: if an action (analysis, add/remove arm, early stop) is triggered
    #' based on number of event of TTE, we may need to sample ALL patients to
    #' know the accurate time point. With an enrollment function of more than
    #' one patient, we can find out the time point and roll back to then, and
    #' recover randomization queue and enrollment time for the remaining patients.
    enroll_a_patient = function(){

      if(length(self$get_arms()) == 0){
        stop('No arm is added in the trial yet. Patient cannot be enrolled. ')
      }

      if(self$get_number_unenrolled_patients() == 0){
        stop('Maximum planned sample size has been reached. Patient cannot be enrolled. ')
      }

      next_enroll_arm <- self$get_randomization_queue(1)
      ## update randomization_queue after enrolling a new patient.
      ## randomization_queue only keep randomization queue for future patients
      private$randomization_queue <- self$get_randomization_queue(-1)

      next_enroll_time <- self$get_enroll_time(1)
      private$enroll_time <- self$get_enroll_time(-1)

      patient_data <-
        data.frame(
          patient_id = self$get_number_enrolled_patients() + 1,
          arm = next_enroll_arm,
          enroll_time = next_enroll_time
        )

      n_ <- 1 # sample data for one patient
      for(ep in self$get_an_arm(next_enroll_arm)$get_endpoints()){
        patient_data <- cbind(patient_data, ep$get_generator()(n_))
      }

      private$trial_data <- bind_rows(private$trial_data, patient_data)
      private$n_enrolled_patients <- private$n_enrolled_patients + 1

    },

    #' @description
    #' assign new patients to pre-planned randomization queue at pre-specified
    #' enrollment time.
    #' @param n_patients number of new patients to be enrolled. If \code{NULL},
    #' all remaining patients in plan are enrolled. Error may be triggered if
    #' n_patients is greater than remaining patients as planned.
    enroll_patients = function(n_patients = NULL){

      if(length(self$get_arms()) == 0){
        stop('No arm is added in the trial yet. Patient cannot be enrolled. ')
      }

      if(self$get_number_unenrolled_patients() == 0){
        stop('Maximum planned sample size has been reached. Patient cannot be enrolled. ')
      }

      if(is.null(n_patients)){
        n_patients <- self$get_number_unenrolled_patients()
      }

      if(n_patients > self$get_number_unenrolled_patients()){
        stop('Cannot enroll ', n_patients, ' patients for the trial. ',
             'Only ', self$get_number_unenrolled_patients(), ' left. ')
      }

      next_enroll_arms <- self$get_randomization_queue(1:n_patients)
      ## update randomization_queue after enrolling a new patient.
      ## randomization_queue only keep randomization queue for future patients
      private$randomization_queue <- self$get_randomization_queue(-c(1:n_patients))

      next_enroll_time <- self$get_enroll_time(1:n_patients)
      private$enroll_time <- self$get_enroll_time(-c(1:n_patients))

      patient_data <- NULL
      arms_data <- list()
      arms_in_trial <- sort(unique(next_enroll_arms))
      for(i in seq_along(arms_in_trial)){
        arm <- arms_in_trial[i]
        patients_index <- which(next_enroll_arms %in% arm)
        n_patients_in_arm <- length(patients_index)
        arms_data[[arm]] <-
          data.frame(
            patient_id = self$get_number_enrolled_patients() + patients_index,
            arm = arm,
            enroll_time = next_enroll_time[patients_index]
          )

        for(ep in self$get_an_arm(arm)$get_endpoints()){
          arms_data[[arm]] <- cbind(arms_data[[arm]], ep$get_generator()(n_patients_in_arm))
        }

        arm_data <- arms_data[[arm]]
        if(!is.null(patient_data)){
          diff_cols1 <- setdiff(names(arm_data), names(patient_data))
          diff_cols2 <- setdiff(names(patient_data), names(arm_data))
          if(length(diff_cols1) > 0){
            stop('Arm <', arm, '> may have endpoints different from other arms: <',
                 paste0(diff_cols1, collapse = ', '), '>.')
          }

          if(length(diff_cols2) > 0){
            stop('Arm <', arm, '> may have endpoints different from other arms: <',
                 paste0(diff_cols2, collapse = ', '), '>.')
          }
        }

        patient_data <- bind_rows(patient_data, arm_data)
      }


      for(arm in arms_in_trial){
        patient_data[which(next_enroll_arms %in% arm), ] <- arms_data[[arm]]
      }

      private$trial_data <- bind_rows(self$get_trial_data(), patient_data)

      message('Data of ', n_patients,
              ' potential patients are generated for the trial with ',
              self$get_number_arms(), ' arm(s) <',
              paste0(self$get_arms_name(), collapse = ", "), '>. \n')#,
              # 'Depending on the scenarios, ',
              # 'some of those patients may be eventually enrolled \n',
              # 'and used in data lock, \n',
              # 'while some will be abandoned and re-generated ',
              # '(e.g. arm is removed or added). \n')

    },

    #' @description
    #' set current time of a trial. Any data collected before could not be
    #' changed. private$now should be set after an event is triggered
    #' (through Event class, futility, interim, etc), an arm is added or
    #' removed as a result of an event
    #' @param time current calendar time of a trial.
    set_current_time = function(time){
      stopifnot(time >= 0)
      private$now <- time
    },

    #' @description
    #' return current time of a trial
    get_current_time = function(){
      private$now
    },

    #' @description
    #' count accumulative number of events (for TTE) or samples (otherwise) over
    #' calendar time (enroll time + tte for TTE, or enroll time otherwise)
    get_event_tables = function(){

      trial_data <- self$get_trial_data()

      event_counts <- list()

      event_cols <- grep('_event$', names(trial_data), value = TRUE)
      for(event_col in event_cols){
        tte_col <- gsub('_event$', '', event_col)
        event_counts[[tte_col]] <- trial_data %>%
          dplyr::select(all_of(c('patient_id', 'arm', 'enroll_time', tte_col, event_col))) %>%
          mutate(calendar_time := enroll_time + !!sym(tte_col)) %>%
          arrange(calendar_time) %>%
          mutate(n_events = cumsum(get(event_col)))

      }

      other_cols <-
        setdiff(
          names(trial_data),
          c('patient_id', 'arm', 'enroll_time', event_cols, gsub('_event$', '', event_cols)))

      for(event_col in other_cols){
        event_counts[[event_col]] <- trial_data %>%
          dplyr::select(all_of(c('patient_id', 'arm', 'enroll_time', event_col))) %>%
          mutate(calendar_time = enroll_time) %>%
          arrange(calendar_time) %>%
          mutate(n_events = row_number())
      }

      event_counts

    },

    #' @description
    #' given a set of endpoints and target number of events, determine the data
    #' lock time for Event (futility, interim, final (?)). This function does
    #' not change trial object (e.g. rolling back not yet randomized patients after
    #' the found data lock time).
    #' @param endpoints character vector. Data lock time is determined by a set
    #' of endpoints.
    #' @param target_n_events target number of events for each of the
    #' \code{endpoints}.
    #' @param type \code{all} if all target number of events are reached.
    #' \code{any} if the any target number of events is reached.
    #' @return data lock time
    #' @examples
    #' ## dat <- trial$get_trial_data()
    #' ## trial$get_data_lock_time(c('pfs','orr'), c(200,500), 'any')
    get_data_lock_time = function(endpoints, target_n_events, type = c('all', 'any')){

      type <- match.arg(type)

      stopifnot(is.character(endpoints))
      stopifnot(all(is.wholenumber(target_n_events)))
      stopifnot(length(endpoints) == length(target_n_events))

      event_counts <- self$get_event_tables()
      stopifnot(all(endpoints %in% names(event_counts)))

      event_times <- NULL
      for(i in seq_along(endpoints)){
        if(max(event_counts[[endpoints[i]]]$n_events) < target_n_events[i]){
          stop('No enough events/samples for endpoint ', endpoints[i],
               ' to reach the target number. ')
        }

        event_times <-
          c(event_times,
            min(event_counts[[endpoints[i]]]$calendar_time[
              event_counts[[endpoints[i]]]$n_events >= target_n_events[i]
            ]))
      }

      lock_time <-
        case_when(
          type %in% 'all' ~ max(event_times),
          type %in% 'any' ~ min(event_times),
          TRUE ~ -Inf
        )


      attr(lock_time, 'n_events') <- list()
      for(i in seq_along(event_counts)){
        ec <- event_counts[[i]]
        attr(lock_time, 'n_events')[[names(event_counts)[i]]] <-
          ifelse(any(ec$calendar_time <= lock_time),
                 max(ec$n_events[ec$calendar_time <= lock_time]),
                 0)

      }

      lock_time

    },

    #' @description
    #' return locked data for an event
    #' @param event_name character, event name of which the locked data to be
    #' extracted.
    get_locked_data = function(event_name){
      private$locked_data[[event_name]]
    },

    #' @description
    #' lock data at specific calendar time.
    #' For time-to-event endpoints, their event indicator *_event should be
    #' updated accordingly. Locked data should be stored separately.
    #' DO NOT OVERWRITE/UPDATE self$trial_data! which can lose actual
    #' time-to-event information. For example, a patient may be censored at
    #' the first data lock. However, he may have event being observed in a
    #' later data lock.
    #' @param at_calendar_time time point to lock trial data
    #' @param event_name assign event name as the name of locked data for
    #' future reference.
    lock_data = function(at_calendar_time, event_name){

      trial_data <- self$get_trial_data()

      event_cols <- grep('_event$', names(trial_data), value = TRUE)

      for(event_col in event_cols){
        tte_col <- gsub('_event$', '', event_col)
        trial_data <- trial_data %>%
          mutate(calendar_time := enroll_time + !!sym(tte_col)) %>%
          mutate(!!event_col := ifelse(calendar_time > at_calendar_time, 0, !!sym(event_col))) %>%
          mutate(!!tte_col :=
                   ifelse(calendar_time > at_calendar_time,
                          at_calendar_time - enroll_time,
                          !!sym(tte_col)
                          )
                 )
      }

      locked_data <- trial_data %>%
        dplyr::filter(enroll_time <= at_calendar_time) %>%
        arrange(enroll_time) %>%
        dplyr::select(-calendar_time)

      unenrolled_data <- trial_data %>%
        dplyr::filter(enroll_time > at_calendar_time) %>%
        arrange(enroll_time) %>%
        dplyr::select(enroll_time, arm)
      rm(trial_data)

      attr(locked_data, 'lock_time') <- at_calendar_time
      attr(locked_data, 'n_enrolled_patients') <- length(unique(locked_data$patient_id))
      attr(locked_data, 'event_name') <- event_name
      private$locked_data[[event_name]] <- locked_data
      self$set_current_time(at_calendar_time)
      message('Data is locked at time = ', at_calendar_time, ' for event <',
              event_name, '>.\n',
              'Locked data can be accessed in Trial$get_locked_data(\'',
              event_name, '\'). \n',
              'Number of events at lock time: \n')
      print(as.data.frame(attr(at_calendar_time, 'n_events')))
      cat('\n')

      ## I am not sure about this part yet.
      ## Once data is locked for an event, it is not always necessary to
      ## roll back. For example, all arms are keeping moving without anything
      ## change is possible. This could happen except for futility analysis
      ## (early stop), or adding/removing arms. It seems like this part should
      ## be done in action() depending on the type of event.
      ##
      ## updated note: Yes, should be done by users in action function of event
      ## Actually, $add_arms, $remove_arms, and $update_sample_ratio can be
      ## called by users in action function. All these three functions will
      ## do randomization, and patient enrollment again. We possibly support
      ## update enrollment curve in the future.
      if(0){
      private$enroll_time <- unenrolled_data$enroll_time
      private$randomization_queue <- unenrolled_data$arm


      private$trial_data <- self$get_trial_data() %>%
        dplyr::filter(enroll_time <= at_calendar_time)

      self$enroll_patients()
      }

      NULL
    }

  ),

  private = list(
    seed = NULL,
    name = NULL,
    description = NULL,
    n_patients = NULL,
    n_enrolled_patients = NULL,
    sample_ratio = NULL,

    arms = list(),
    now = 0, # current time point of a trial. Change to the data of patients
             # enrolled before are not allowed. When a trial is created,
             # now = 0. If an event triggers a data lock (in Event class),
             # now will be set to the time of data lock (e.g. futility, interim).
             # When adding or removing a arm at an event,
             # private$randomizatioon_queue[private$enroll_time > now] will be
             # regenerated. This is important because randomization needs to be
             # done with possibly changed sample ratio. enroll_patients() is
             # then executed with updated randomizatioon_queue.
    randomization_queue = NULL,
    enroller = NULL,
    enroll_time = NULL,

    trial_data = NULL,
    locked_data = list(),

    validate_arguments =
      function(name, n_patients, description, seed, enroller, ...){

      stopifnot(is.null(seed) || is.wholenumber(seed))
      stopifnot(is.character(name))
      stopifnot(is.character(description))

      stopifnot(is.numeric(n_patients) &&
                  (length(n_patients) == 1) &&
                  is.wholenumber(n_patients))

      stopifnot(is.function(enroller))

      # Check that the first argument of enroller is "n"
      arg_names <- names(formals(enroller))

      n_ <- 2
      enroller_ <- DynamicRNGFunction(
        enroller, rng = deparse(substitute(enroller)), simplify = TRUE, ...)
      example_data <- enroller_(n = n_)
      if(!is.vector(example_data)){
        stop('enroller must return a vector.')
      }

      if(length(example_data) != n_){
        stop('\'n\' in enroller does not work correctly.')
      }

    },

    ## Whenever arms are added or removed from the trial,
    ## permuted_block_randomization should be called to update the randomization
    ## plan for unenrolled patients, up to the maximum planned sample size.
    ## This function will be called by add_arms, remove_arms, etc.
    permuted_block_randomization = function(block_size = NULL){

      if(!is.null(self$get_randomization_queue())){
        message('condition check is triggered in permuted_block_randomization. ',
        ' Debug this.\n')
        stopifnot(
          length(self$get_randomization_queue()) ==
            self$get_number_unenrolled_patients())
      }

      if(is.null(block_size)){
        block_size <- sum(self$get_sample_ratio())
      }

      if(self$get_number_unenrolled_patients() == 0){
        stop('All patients are enrolled. No further randomization is needed. \n',
             'If you see this message, there is probably an unexpected issue with your code. \n',
             'One known reason is that arms are added into the trial one right after one, \n',
             'e.g., calling $add_arms twice and no event happen in between. \n',
             'However, whenever an event is triggered during a trial, ',
             'patients being enrolled after the event time will be rolled back, ',
             'so that a new arm can be removed or added. \n',
             'Those patients will be randomized again. \n')
      }
      block <- rep(seq(private$sample_ratio), times = private$sample_ratio)
      blocks <- rep(block, length.out = self$get_number_unenrolled_patients())
      randomization_queue <-
        lapply(
          split(blocks, ceiling(seq_along(blocks) / block_size)),
          sample
        ) %>%
        unlist() %>%
        unname()

      arm_names <- names(private$sample_ratio)
      private$randomization_queue <- arm_names[randomization_queue]
      message('Randomization is done for ', length(randomization_queue),
              ' potential patients. \n') #,
              # 'Make sure that you only see this message when initializing a trial, \n',
              # 'or after adding/removing an arm from the trial. \n',
              # 'Otherwise it may indicator a potential issue. \n')
    }


  )
)
