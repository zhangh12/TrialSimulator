#' Class of Trial
#' @description
#' Create a class of trial.
#'
#' @docType class
#' @examples
#' # Instead of using Trial$new, please use trial(), a user-friendly
#' # wrapper. See examples in ?trial.
#'
#' @export
Trials <- R6::R6Class(
  'Trials',

  public = list(

    #' @description
    #' initialize a trial
    #' @param name character. Name of trial.
    #' @param n_patients integer. Maximum number of patients could be enrolled
    #' to the trial.
    #' @param duration Numeric. Trial duration.
    #' @param description character. Optional for description of the trial. By
    #' default it is set to be trial's \code{name}.
    #' @param seed random seed. If \code{NULL}, \code{set.seed()} will not be
    #' called, which uses seed set outside.
    #' @param enroller a function returning a vector enrollment time for
    #' patients. Its first argument is the number of enrolled patients.
    #' @param dropout a function returning a vector of dropout time for
    #' patients. Its first argument is the number of enrolled patients.
    #' @param silent logical. \code{TRUE} to mute messages.
    #' @param ... arguments of \code{enroller} and \code{dropout}.
    initialize =
      function(
        name,
        n_patients,
        duration,
        description = name,
        seed = NULL,
        enroller,
        dropout = NULL,
        silent = FALSE,
        ...
      ){

        private$validate_arguments(
          name, n_patients, duration, description, seed, enroller, dropout, silent, ...)

        private$silent <- silent

        if(is.null(seed)){
          seed <- sample(.Machine$integer.max, 1)
          if(!private$silent){
            message('Seed is not specified. TrialSimulator sets it to ', seed)
          }
        }

        private$arms <- list()
        private$name <- name
        private$description <- description
        private$n_patients <- n_patients
        private$duration <- duration
        private$now <- 0
        private$trial_data <- NULL
        private$locked_data <- list()
        private$output <- data.frame(trial = self$get_name())
        private$custom_data <- list()

        private$seed <- seed
        self$save(seed, 'seed')
        self$save('', 'error_message')

        if(is.null(dropout)){
          self$set_dropout(rconst, value = Inf)
        }else{
          self$set_dropout(dropout, ...)
        }

        self$set_enroller(enroller, ...)

        self$make_snapshot()

        set.seed(private$seed)
        ## sort enrollment time
        private$enroll_time <-
          sort(self$get_enroller()(n = n_patients), decreasing = FALSE)

      },

    #' @description
    #' return trial data of enrolled patients at the time of this
    #' function is called
    get_trial_data = function(){
      private$trial_data
    },

    #' @description
    #' return maximum duration of a trial
    get_duration = function(){
      private$duration
    },

    #' @description
    #' set trial duration in an adaptive designed trial. All patients enrolled
    #' before resetting the duration are truncated (non-tte endpoints) or
    #' censored (tte endpoints) at the original duration. Remaining patients
    #' are re-randomized. Now new duration must be longer than the old one.
    #' @param duration new duration of a trial. It must be longer than the
    #' current duration.
    set_duration = function(duration){

      if(duration <= self$get_duration()){
        stop('Trial duration can only be set to be longer. <', duration,
             '> is shorter than <', self$get_duration(), '>. ')
      }

      old_duration <- self$get_duration()

      ## update the duration
      private$duration <- duration

      if(!private$silent){
        message('Trial duration is updated <', old_duration,
                '> -> <', self$get_duration(), '>. ')
      }

      ## all patients enrolled before current milestone should be censored
      ## or truncated at old duration
      self$censor_trial_data(censor_at = old_duration,
                             enrolled_before = self$get_current_time())

      ## with trial duration is extended, unenrolled patient at current time
      ## should be randomized again.
      self$roll_back()

      ## update data for unrolled patients based on new trial duration
      self$enroll_patients()

    },

    #' @description
    #' set recruitment curve when initialize a
    #' trial.
    #' @param func function to generate enrollment time. It can be built-in
    #' function like `rexp` or customized functions like `StaggeredRecruiter`.
    #' @param ... arguments for \code{func}.
    set_enroller = function(func, ...){

      # Check that the first argument of enroller is "n"
      arg_names <- names(formals(func))
      if (length(arg_names) == 0 || arg_names[1] != "n") {
        stop("The first argument of enroller must be 'n'.")
      }

      n_ <- 2

      suppressWarnings(
        enroller_ <- DynamicRNGFunction(
          func, rng = deparse(substitute(func)), simplify = TRUE, ...)
      )

      example_data <- enroller_(n = n_)
      if(!is.vector(example_data)){
        stop('enroller must return a vector.')
      }

      if(length(example_data) != n_){
        stop('\'n\' in enroller does not work correctly.')
      }

      private$enroller <- enroller_
    },

    #' @description
    #' get function of recruitment curve
    get_enroller = function(){
      private$enroller
    },

    #' @description
    #' set distribution of drop out time. This can be done when initialize a
    #' trial, or when updating a trial in adaptive design.
    #' @param func function to generate dropout time. It can be built-in
    #' function like `rexp` or customized functions.
    #' @param ... arguments for \code{func}.
    set_dropout = function(func, ...){

      arg_names <- names(formals(func))
      if (length(arg_names) == 0 || arg_names[1] != "n") {
        stop("The first argument of random number generator for dropout time must be 'n'.")
      }

      suppressWarnings(
        dropout_ <- DynamicRNGFunction(func, rng = deparse(substitute(func)),
                                       simplify = TRUE, ...)
      )

      n_ <- 2
      example_data <- dropout_(n = n_)
      if(!is.vector(example_data)){
        stop('dropout must return a vector.')
      }

      if(length(example_data) != n_){
        stop('\'n\' in dropout does not work correctly.')
      }

      private$dropout <- dropout_

    },

    #' @description
    #' get generator of dropout time
    get_dropout = function(){
      private$dropout
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

      if(!private$silent){
        message('Trial data is rolling back to time = ', current_time, '. \n',
                'Randomization will be carried out again for unenrolled patients. \n')
      }

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

        ## I have to assume that the function Trials$remove_arms() can only be called in an action function.
        ## Thus, the time an arm is removed from the trial is the triggering time of the most recent milestone
        ## This assumption can be problematic but for now I do not have other solution.
        ## In Trials$dunnettTest(), I need to know at a given milestone whether an arm is still in the trial.
        ##
        existing_milestone_time <- self$get_milestone_time()
        if(is.null(existing_milestone_time)){
          latest_milestone_time <- 0
        }else{
          latest_milestone_time <- max(existing_milestone_time)
        }
        self$set_arm_removal_time(arm = arm_name,
                                  time = latest_milestone_time)
      }

      if(!private$silent){
        message('Arm <', paste0(arms_name, collapse = ', '), '> is removed. \n')

        message('Sample ratio is updated to be <',
                paste0(paste0(names(self$get_sample_ratio()),
                              ': ', self$get_sample_ratio()), collapse = ', '),
                '>. \n')
      }

      ## data of removed arms should be censored at milestone time
      ## so that number of events of those arms are fixed.
      ## Otherwise, number of events can possibly increase later and affect
      ## calculation of triggering condition based on event numbers.
      ## Ideally, number of events in removed arms should be flatten afterward,
      ## and can be seen through Trial$event_plot().
      self$censor_trial_data(censor_at = self$get_current_time(),
                             selected_arms = arms_name)
      ## with an arm is removed, unenrolled patient at current time should be
      ## randomized again.
      self$roll_back()

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

      if(!private$silent){
        message('Sample ratio has been udpated to be <',
                paste0(paste0(names(self$get_sample_ratio()),
                              ': ', self$get_sample_ratio()), collapse = ', '),
                '>. ')
      }

      ## with sample ratio of an arm is updated, unenrolled patient at current
      ## time should be randomized again.
      self$roll_back()

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
        stopifnot(inherits(arm, 'Arms'))

        if(!arm$has_endpoint()){
          stop('No endpoint in the arm <', arm$get_name(), '>. ',
               'Make sure that Arm$add_endpoints() has been executed before adding this arm into the trial. ')
        }

        if(arm$get_name() %in% self$get_arms_name()){
          stop('Arm <', arm$get_name(), '> already exists in the trial. ',
               'Do you want to update it instead? \n',
               'If so you need to revise your code, ',
               'currently updating an arm is not yet supported. ')
        }

        private$arms[[arm$get_name()]] <- arm
        arm_names <- c(arm_names, arm$get_name())

        ## I have to assume that the function Trials$add_arms() can only be called in an action function.
        ## Thus, the time an arm is added to the trial is the triggering time of the most recent milestone
        ## This assumption can be problematic but for now I do not have other solution.
        ## In Trials$dunnettTest(), I need to know at a given milestone whether an arm is still in the trial.
        ##
        existing_milestone_time <- self$get_milestone_time()
        if(is.null(existing_milestone_time)){
          latest_milestone_time <- 0
        }else{
          latest_milestone_time <- max(existing_milestone_time)
        }
        self$set_arm_added_time(arm = arm$get_name(),
                                time = latest_milestone_time)

      }

      if(!private$silent){
        message('Arm(s) <', paste0(arm_names, collapse = ', '),
                '> are added to the trial. \n')
      }

      names(sample_ratio) <- arm_names
      private$sample_ratio <- c(private$sample_ratio, sample_ratio)
      rm(sample_ratio)

      if(enforce){
        self$roll_back()
      }

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
    #' check if the trial has any arm. Return \code{TRUE} or \code{FALSE}.
    has_arm = function(){
      self$get_number_arms() > 0
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
        if(!private$silent){
          message('Maximum planned sample size has been reached. No more patient to be enrolled. ')
        }
        return(invisible(NULL))
      }

      if(is.null(n_patients)){
        n_patients <- self$get_number_unenrolled_patients()
      }

      if(n_patients > self$get_number_unenrolled_patients()){
        stop('Cannot enroll ', n_patients, ' patients for the trial. ',
             'Only ', self$get_number_unenrolled_patients(), ' left. ')
      }

      ## update randomization plan for unenrolled patients
      private$permuted_block_randomization()

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
            enroll_time = next_enroll_time[patients_index],
            dropout_time = self$get_dropout()(n = n_patients_in_arm)
          )

        arms_data[[arm]] <- cbind(arms_data[[arm]], self$get_an_arm(arm)$generate_data(n_patients_in_arm))

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
      ## newly updated trial data should be always censored at trial duration
      ## also, non-tte endpoints would be NA if readout time is after dropout time,
      ## and tte endpoints should be censored at dropout time.
      self$censor_trial_data()

      if(!private$silent){
        message('Data of ', n_patients,
                ' potential patients are generated for the trial with ',
                self$get_number_arms(), ' arm(s) <',
                paste0(self$get_arms_name(), collapse = ", "), '>. \n')#,
                # 'Depending on the scenarios, ',
                # 'some of those patients may be eventually enrolled \n',
                # 'and used in data lock, \n',
                # 'while some will be abandoned and re-generated ',
                # '(e.g. arm is removed or added). \n')
      }

    },

    #' @description
    #' set current time of a trial. Any data collected before could not be
    #' changed. private$now should be set after a milestone is triggered
    #' (through Milestones class, futility, interim, etc), an arm is added or
    #' removed at a milestone
    #' @param time current calendar time of a trial.
    set_current_time = function(time){
      stopifnot(time >= 0)
      attributes(time) <- NULL
      private$now <- time
    },

    #' @description
    #' return current time of a trial
    get_current_time = function(){
      private$now
    },

    #' @description
    #' count accumulative number of events (for TTE) or non-missing samples (otherwise) over
    #' calendar time (enroll time + tte for TTE, or enroll time + readout otherwise)
    #'
    #' @param arms a vector of arms' name on which the event tables are created.
    #' if \code{NULL}, all arms in the trial will be used.
    #' @param ... subset conditions compatible with \code{dplyr::filter}.
    #' Event tables will be counted on subset of trial data only.
    get_event_tables = function(arms = NULL, ...){

      if(is.null(arms)){
        arms <- self$get_arms_name()
      }

      if(!all(arms %in% c(self$get_arms_name(), names(private$.snapshot[['arms']])))){
        stop('Arm(s) <',
             paste0(setdiff(arms, self$get_arms_name()), collapse = ', '),
             '> cannot be found in the trial, debug Trial$get_event_tables. ')
      }

      trial_data <- self$get_trial_data() %>%
        dplyr::filter(arm %in% arms)

      trial_data <- if(...length() == 0){
        trial_data
      }else{
        tryCatch({
          trial_data %>% dplyr::filter(...)
        },
        error = function(e){
          stop('Error in filtering data for table of event count. ',
               'Please check condition in ..., ',
               'which should be compatible with dplyr::filter. ')
        })
      }

      event_counts <- list()

      ## add event count for patient_id
      event_counts[['patient_id']] <- trial_data %>%
        dplyr::select(all_of(c('patient_id', 'arm', 'enroll_time'))) %>%
        mutate(calendar_time := enroll_time) %>%
        arrange(calendar_time) %>%
        mutate(n_events = row_number())

      ## add event counts for time-to-event endpoints
      event_cols <- grep('_event$', names(trial_data), value = TRUE)
      for(event_col in event_cols){
        tte_col <- gsub('_event$', '', event_col)
        event_counts[[tte_col]] <- trial_data %>%
          dplyr::select(all_of(c('patient_id', 'arm', 'enroll_time', tte_col, event_col))) %>%
          mutate(calendar_time := enroll_time + !!sym(tte_col)) %>%
          arrange(calendar_time) %>%
          mutate(n_events = cumsum(get(event_col)))

      }

      ## add event counts for non-time-to-event endpoints
      readout_cols <- grep('_readout$', names(trial_data), value = TRUE)
      for(readout_col in readout_cols){
        ep_col <- gsub('_readout$', '', readout_col)
        event_counts[[ep_col]] <- trial_data %>%
          dplyr::select(all_of(c('patient_id', 'arm', 'enroll_time', ep_col, readout_col))) %>%
          mutate(calendar_time := enroll_time + !!sym(readout_col)) %>%
          dplyr::filter(!is.na(!!sym(ep_col))) %>%
          arrange(calendar_time) %>%
          mutate(n_events = row_number())
      }

      event_counts

    },

    #' @description
    #' given a set of endpoints and target number of events, determine the data
    #' lock time for a milestone (futility, interim, final, etc.). This function does
    #' not change trial object (e.g. rolling back not yet randomized patients after
    #' the found data lock time).
    #' @param endpoints character vector. Data lock time is determined by a set
    #' of endpoints.
    #' @param target_n_events target number of events for each of the
    #' \code{endpoints}.
    #' @param arms a vector of arms' name on which number of events will be
    #' counted.
    #' @param type \code{all} if all target number of events are reached.
    #' \code{any} if the any target number of events is reached.
    #' @param ... subset conditions compatible with \code{dplyr::filter}. Number
    #' Time of milestone is based on event counts on the subset of trial data.
    #' @return data lock time
    #' @examples
    #' ## trial$get_data_lock_time_by_event_number(c('pfs','orr'), c(200,500), 'any')
    get_data_lock_time_by_event_number = function(endpoints, arms,
                                                  target_n_events,
                                                  type = c('all', 'any'),
                                                  ...){

      type <- match.arg(type)

      stopifnot(is.character(endpoints))
      stopifnot(all(is.wholenumber(target_n_events)))
      stopifnot(length(endpoints) == length(target_n_events))

      if(is.null(arms)){
        arms <- self$get_arms_name()
      }

      event_counts <- self$get_event_tables(arms, ...)

      missed_endpoints <- setdiff(endpoints, names(event_counts))
      if(length(missed_endpoints) > 0){
        Stop('Endpoints <',
             paste0(missed_endpoints, collapse = ', '),
             '> are missing in event_counts when determining data lock time. ')
      }

      milestone_times <- NULL
      for(i in seq_along(endpoints)){
        if(max(event_counts[[endpoints[i]]]$n_events) < target_n_events[i]){
          warning('No enough events/samples for endpoint <', endpoints[i],
               '> to reach the target number <', target_n_events[i], '>. ',
               immediate. = TRUE)
          milestone_times <- c(milestone_times, Inf)
        }else{

          milestone_times <-
            c(milestone_times,
              min(event_counts[[endpoints[i]]]$calendar_time[
                event_counts[[endpoints[i]]]$n_events >= target_n_events[i]
              ]))
        }
      }

      lock_time <-
        case_when(
          type %in% 'all' ~ max(milestone_times),
          type %in% 'any' ~ min(milestone_times),
          TRUE ~ -Inf
        )

      if(is.infinite(lock_time)){
        stop('None of the endpoints can reach target event number during the trial. ')
      }


      attr(lock_time, 'n_events') <- list()

      ## count events on all trial data in trial output
      event_counts <- self$get_event_tables(arms)

      for(i in seq_along(event_counts)){
        ec <- event_counts[[i]]
        attr(lock_time, 'n_events')[[names(event_counts)[i]]] <-
          ifelse(any(ec$calendar_time <= lock_time),
                 max(ec$n_events[ec$calendar_time <= lock_time]),
                 0)

      }

      event_count_per_arm <- list()
      for(arm in arms){
        ec <- self$get_event_tables(arms = arm)
        for(endpoint in names(ec)){
          count <- ifelse(any(ec[[endpoint]]$calendar_time <= lock_time),
                          max(ec[[endpoint]]$n_events[ec[[endpoint]]$calendar_time <= lock_time]),
                          0) %>% setNames(arm)

          event_count_per_arm[[endpoint]] <- c(event_count_per_arm[[endpoint]], count)
        }
      }

      event_count <- NULL
      for(ep in names(event_count_per_arm)){
        event_count <- bind_rows(event_count, data.frame(t(event_count_per_arm[[ep]])) %>% mutate(endpoint = ep))
      }

      attr(lock_time, 'n_events') <-
        data.frame(attr(lock_time, 'n_events'))
      attr(lock_time, 'n_events')$arms <- I(list(event_count))


      lock_time

    },


    #' @description
    #' given the calendar time to lock the data, return it with event counts of
    #' each of the endpoints.
    #' @param calendar_time numeric. Calendar time to lock the data
    #' @param arms a vector of arms' name on which number of events will be
    #' counted.
    #' @return data lock time
    #' @examples
    #' ## trial$get_data_lock_time_by_calendar_time(20)
    get_data_lock_time_by_calendar_time = function(calendar_time, arms){

      stopifnot(is.numeric(calendar_time) && length(calendar_time) && calendar_time >= 0)

      if(is.null(arms)){
        arms <- self$get_arms_name()
      }
      event_counts <- self$get_event_tables(arms)

      lock_time <- calendar_time

      attr(lock_time, 'n_events') <- list()
      for(i in seq_along(event_counts)){
        ec <- event_counts[[i]]
        attr(lock_time, 'n_events')[[names(event_counts)[i]]] <-
          ifelse(any(ec$calendar_time <= lock_time),
                 max(ec$n_events[ec$calendar_time <= lock_time]),
                 0)

      }

      event_count_per_arm <- list()
      for(arm in arms){
        ec <- self$get_event_tables(arms = arm)
        for(endpoint in names(ec)){
          count <- ifelse(any(ec[[endpoint]]$calendar_time <= lock_time),
                          max(ec[[endpoint]]$n_events[ec[[endpoint]]$calendar_time <= lock_time]),
                          0) %>% setNames(arm)

          event_count_per_arm[[endpoint]] <- c(event_count_per_arm[[endpoint]], count)
        }
      }

      event_count <- NULL
      for(ep in names(event_count_per_arm)){
        event_count <- bind_rows(event_count, data.frame(t(event_count_per_arm[[ep]])) %>% mutate(endpoint = ep))
      }

      attr(lock_time, 'n_events') <-
        data.frame(attr(lock_time, 'n_events'))
      attr(lock_time, 'n_events')$arms <- I(list(event_count))


      lock_time

    },

    #' @description
    #' return locked data for a milestone
    #' @param milestone_name character, milestone name of which the locked data to be
    #' extracted.
    get_locked_data = function(milestone_name){
      if(!(milestone_name %in% names(private$locked_data))){
        stop('Locked data for milestone <', milestone_name, '> cannot be found. ')
      }

      private$locked_data[[milestone_name]]
    },

    #' @description
    #' return names of locked data
    get_locked_data_name = function(){
      names(private$locked_data)
    },

    #' @description
    #' return number of events at lock time of milestones
    #' @param milestone_name names of triggered milestones. Use all triggered milestones
    #' if \code{NULL}.
    get_event_number = function(milestone_name = NULL){
      if(is.null(milestone_name)){
        milestone_name <- self$get_locked_data_name()
      }

      n_events <- NULL
      lock_time <- NULL
      for(milestone in milestone_name){
        lock_time <- c(lock_time,
                       attr(self$get_locked_data(milestone), 'lock_time')[1])
        n_events <- bind_rows(n_events,
                              attr(attr(self$get_locked_data(milestone), 'lock_time'), 'n_events'))
      }

      n_events <- n_events %>%
        mutate(lock_time = lock_time) %>%
        mutate(milestone_name = milestone_name) %>%
        arrange(lock_time)

      n_events
    },

    #' @description
    #' save time of a new milestone.
    #' @param milestone_time numeric. Time of new milestone.
    #' @param milestone_name character. Name of new milestone.
    save_milestone_time = function(milestone_time, milestone_name){
      if(milestone_name %in% names(private$milestone_time)){
        stop('Time of milestone <', milestone_name, '> has already been saved before. ')
      }

      if(length(private$milestone_time) > 0){
        if(any(private$milestone_time > milestone_time)){
          en <- names(private$milestone_time)[private$milestone_time > milestone_time]
          et <- private$milestone_time[private$milestone_time > milestone_time]
          stop('New milestone <', milestone_name, '> (time = ', round(milestone_time, 2),
               ') happens before milestones <',
               paste0(en, ' (time = ', round(et, 2), ')', collapse = ', '), '>. \n',
               'A possible reason is mis-specification of milestone order or triggering conditions. \n',
               'Use seed = <', self$get_seed(), '> to debug it. ')
        }
      }

      private$milestone_time[milestone_name] <- milestone_time
    },

    #' @description
    #' return milestone time when triggering a given milestone
    #' @param milestone_name character. Name of milestone. If \code{NULL},
    #' time of all triggered milestones are returned.
    get_milestone_time = function(milestone_name = NULL){

      if(is.null(milestone_name)){
        return(private$milestone_time)
      }

      if(!all(milestone_name %in% names(private$milestone_time))){
        stop('Milestone(s) <',
             paste0(setdiff(milestone_name, names(private$milestone_time)), collapse = ', '),
             '> cannot be found. ',
             'Make sure that milestone(s) have be triggered ',
             'and their triggering time has been saved by calling get_milestone_time. ',
             'Usually this function is called automatically while locking a data. ')
      }

      private$milestone_time[milestone_name]
    },

    #' @description
    #' lock data at specific calendar time.
    #' For time-to-event endpoints, their event indicator \code{*_event} should be
    #' updated accordingly. Locked data should be stored separately.
    #' DO NOT OVERWRITE/UPDATE private$trial_data! which can lose actual
    #' time-to-event information. For example, a patient may be censored at
    #' the first data lock. However, he may have event being observed in a
    #' later data lock.
    #' @param at_calendar_time time point to lock trial data
    #' @param milestone_name assign milestone name as the name of locked data for
    #' future reference.
    lock_data = function(at_calendar_time, milestone_name){

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

      readout_cols <- grep('_readout$', names(trial_data), value = TRUE)
      for(readout_col in readout_cols){
        ep_col <- gsub('_readout$', '', readout_col)
        trial_data <- trial_data %>%
          mutate(calendar_time := enroll_time + !!sym(readout_col)) %>%
          ## in locked data, some patients may have been enrolled, but
          ## their non-tte endpoints have no readout, thus set to be NA
          mutate(!!ep_col := ifelse(calendar_time > at_calendar_time, NA, !!sym(ep_col)))
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
      attr(locked_data, 'milestone_name') <- milestone_name
      private$locked_data[[milestone_name]] <- locked_data
      self$set_current_time(at_calendar_time)
      self$save_milestone_time(at_calendar_time, milestone_name)

      self$save(value = at_calendar_time, name = paste0('milestone_time_<', milestone_name, '>'))

      self$save(value = attr(at_calendar_time, 'n_events'),
                name = paste0('n_events_<', milestone_name, '>'))

      if(!private$silent){
        message('Data is locked at time = ', at_calendar_time, ' for milestone <',
                milestone_name, '>.\n',
                'Locked data can be accessed in Trial$get_locked_data(\'',
                milestone_name, '\'). \n',
                'Number of events at lock time: \n')
        out <- as.data.frame(attr(at_calendar_time, 'n_events'))
        if('patient_id' %in% names(out)){
          colnames(out)[names(out) == 'patient_id'] <- 'patient'
        }
        message(paste0(capture.output(out), collapse = "\n"))
        message('\n')
      }

      ## I am not sure about this part yet.
      ## Once data is locked for a milestone, it is not always necessary to
      ## roll back. For example, all arms are keeping moving without anything
      ## change is possible. This could happen except for futility analysis
      ## (early stop), or adding/removing arms. It seems like this part should
      ## be done in action() depending on the type of milestone.
      ##
      ## updated note: Yes, should be done by users in action function of milestone
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
    },


    #' @description
    #' plot of cumulative number of events/samples over calendar time.
    event_plot = function(){

      if(private$silent){
        return(invisible(NULL))
      }

      trial_data <- self$get_trial_data()

      event_number <- self$get_event_number()
      event_number$event_name <- paste0(1:nrow(event_number), ': ',
                                        event_number$event_name)

      event_cols <- grep('_event$', names(trial_data), value = TRUE)
      readout_cols <- grep('_readout$', names(trial_data), value = TRUE)

      all_data_list <- NULL
      for(col in c(event_cols, readout_cols)){
        tte_col <- gsub('_event$', '', col)
        ep_col <- gsub('_readout$', '', col)

        stopifnot((tte_col != col) || (ep_col != col))

        data_list <- list()

        if(ep_col == col){ ## a tte endpoint
          event_counts <- trial_data %>%
            dplyr::select(all_of(c('patient_id', 'arm', 'enroll_time', tte_col, col))) %>%
            mutate(calendar_time := enroll_time + !!sym(tte_col)) %>%
            arrange(calendar_time)
          col_ <- tte_col

          data_list[['0: overall']] <- event_counts %>%
            mutate(n_events = cumsum(get(col)))

          idx <- 0
          for(arm_ in sort(unique(trial_data$arm))){
            idx <- idx + 1
            data_list[[paste0(idx, ': ', arm_)]] <- event_counts %>%
              dplyr::filter(arm %in% arm_) %>%
              arrange(calendar_time) %>%
              mutate(n_events = cumsum(get(col)))
          }

        }else{ ## a non-tte endpoint
          event_counts <- trial_data %>%
            dplyr::select(all_of(c('patient_id', 'arm', 'enroll_time', ep_col, col))) %>%
            dplyr::filter(!is.na(!!sym(ep_col))) %>%
            mutate(calendar_time := enroll_time + !!sym(col)) %>%
            arrange(calendar_time)
          col_ <- ep_col

          data_list[['0: overall']] <- event_counts %>%
            mutate(n_events = row_number())

          idx <- 0
          for(arm_ in sort(unique(trial_data$arm))){
            idx <- idx + 1
            data_list[[paste0(idx, ': ', arm_)]] <- event_counts %>%
              dplyr::filter(arm %in% arm_) %>%
              arrange(calendar_time) %>%
              mutate(n_events = row_number())
          }

        }

        all_data_list <- bind_rows(
          all_data_list,
          lapply(names(data_list), function(name){
            data_list[[name]] %>%
              mutate(arm = name) %>%
              mutate(endpoint = col_)
          })
        )

      }


      ## prepare stacked area chart
      all_data <- all_data_list %>%
        dplyr::filter(!(arm %in% '0: overall'))

      endpoints <- sort(unique(all_data$endpoint))
      arms <- sort(unique(all_data$arm))
      ct <- sort(unique(all_data$calendar_time))

      new_data <- NULL
      for(col in c(event_cols, readout_cols)){
        tte_col <- gsub('_event$', '', col)
        ep_col <- gsub('_readout$', '', col)
        is_tte <- (ep_col == col)
        ep <- ifelse(is_tte, tte_col, ep_col)

        for(arm_ in arms){
          dat <- all_data %>%
            dplyr::filter(endpoint %in% ep & arm %in% arm_) %>%
            dplyr::select(c('arm', col, 'calendar_time', 'endpoint')) %>%
            rename(has_event = !!sym(col))

          if(!is_tte){
            dat$has_event <- 1
          }

          time <- sort(setdiff(ct, dat$calendar_time))
          if(length(time) == 0){
            new_data <- bind_rows(new_data, dat)
            next
          }

          dat <- bind_rows(dat,
                           data.frame(
                             arm = arm_,
                             has_event = 0,
                             calendar_time = time,
                             endpoint = ep
                           ))
          dat <- dat[!duplicated(dat), ] %>%
            arrange(calendar_time) %>%
            mutate(n_events = cumsum(has_event)) %>%
            dplyr::select(-has_event)

          new_data <- bind_rows(new_data, dat)
        }
      }

      new_data$arm <- factor(new_data$arm, levels = arms)

      soft_colors <- function(n) {
        hcl(h = seq(0, 360 * (n-1)/n, length.out = n), c = 60, l = 70)
      }

      p <- ggplot(new_data, aes(x = calendar_time, y = n_events, fill = arm)) +
        xlim(0, self$get_duration() * 1.05) +
        labs(
          x = 'Calendar Time',
          y = 'Cumulative N',
          color = ''
        ) +
        geom_area() +
        scale_fill_manual(
          values = soft_colors(length(arms)),
          name = "Arm"
        ) +
        geom_vline(
          data = event_number,
          aes(xintercept = lock_time),
          linetype = 'dashed'
        ) +
        facet_wrap(~ endpoint, scales = 'free_x') +
        theme_minimal() +
        theme(legend.position = 'bottom')

      plot(p)

    },

    #' @description
    #' censor trial data at calendar time
    #' @param censor_at time of censoring. It is set to trial duration if
    #' \code{NULL}.
    #' @param selected_arms censoring is applied to selected arms (e.g.,
    #' removed arms) only. If \code{NULL}, it will be set to all available arms
    #' in trial data. Otherwise, censoring is applied to user-specified arms only.
    #' This is necessary because number of events/sample size in removed arms
    #' should be fixed unchanged since corresponding milestone is triggered. In that
    #' case, one can update trial data by something like
    #' \code{censor_trial_data(censor_at = milestone_time, selected_arms = removed_arms)}.
    #' @param enrolled_before censoring is applied to patients enrolled before
    #' specific time. This argument would be used when trial duration is
    #' updated by \code{set_duration}. Adaptation happens when \code{set_duration}
    #' is called so we fix duration for patients enrolled before adaptation
    #' to maintain independent increment. This should work when trial duration
    #' is updated for multiple times.
    censor_trial_data = function(censor_at = NULL, selected_arms = NULL, enrolled_before = Inf){

      if(is.null(censor_at)){
        censor_at <- self$get_duration()
      }

      trial_data <- self$get_trial_data()

      if(is.null(selected_arms)){
        selected_arms <- unique(trial_data$arm)
      }

      event_cols <- grep('_event$', names(trial_data), value = TRUE)
      readout_cols <- grep('_readout$', names(trial_data), value = TRUE)

      for(event_col in event_cols){
        tte_col <- gsub('_event$', '', event_col)
        trial_data <- trial_data %>%
          mutate(!!event_col := ifelse((!!sym(tte_col) + enroll_time > dropout_time) &
                                         (arm %in% selected_arms) &
                                         (enroll_time <= enrolled_before),
                                       0, !!sym(event_col))) %>%
          mutate(!!tte_col := ifelse((!!sym(tte_col) + enroll_time > dropout_time) &
                                       (arm %in% selected_arms) &
                                       (enroll_time <= enrolled_before),
                                     dropout_time - enroll_time, !!sym(tte_col))) %>%
          mutate(calendar_time := enroll_time + !!sym(tte_col)) %>%
          mutate(!!event_col := ifelse((calendar_time > censor_at) &
                                         (arm %in% selected_arms) &
                                         (enroll_time <= enrolled_before),
                                       0, !!sym(event_col))) %>%
          mutate(!!tte_col := ifelse((calendar_time > censor_at) &
                                       (arm %in% selected_arms) &
                                       (enroll_time <= enrolled_before),
                                     censor_at - enroll_time, !!sym(tte_col))) %>%
          mutate(!!tte_col := ifelse(!!sym(tte_col) < 0, 0, !!sym(tte_col))) %>%
          dplyr::select(-calendar_time) %>%
          arrange(enroll_time)
      }

      for(readout_col in readout_cols){
        ep_col <- gsub('_readout$', '', readout_col)
        trial_data <- trial_data %>%
          mutate(!!ep_col := ifelse((!!sym(readout_col) + enroll_time > dropout_time) &
                                      (arm %in% selected_arms) &
                                      (enroll_time <= enrolled_before),
                                    NA, !!sym(ep_col))) %>%
          mutate(calendar_time := enroll_time + !!sym(readout_col)) %>%
          mutate(!!ep_col := ifelse((calendar_time > censor_at) &
                                      (arm %in% selected_arms) &
                                      (enroll_time <= enrolled_before),
                                    NA, !!sym(ep_col))) %>%
          dplyr::select(-calendar_time) %>%
          arrange(enroll_time)
      }

      private$trial_data <- trial_data
    },

    #' @description
    #' save a single value or a one-row data frame to trial's output
    #' for further analysis/summary later.
    #' @param value value to be saved. It can be a vector (of length 1) or
    #' a data frame (of one row).
    #' @param name character to name the saved object. It will be used to
    #' name a column in trial's output if \code{value} is a vector.
    #' If \code{value} is a data frame, \code{name} will be the prefix pasted
    #' with the column name of \code{value} in trial's output.
    #' If user want to use
    #' \code{value}'s column name as is in trial's output, set \code{name}
    #' to be \code{''} as default. Otherwise, column name would be, e.g.,
    #' \code{"{name}_<{names(value)}>"}.
    #' @param overwrite logic. \code{TRUE} if overwriting existing entries
    #' with warning, otherwise, throwing an error and stop.
    save = function(value, name = '', overwrite = FALSE){

      is_vector_length1 <- function(x) {
        length(x) == 1 && is.atomic(unclass(x))
      }

      is_vector <- function(x) {
        is.atomic(unclass(x))
      }

      if(!(is.character(name) && length(name) == 1)){
        stop('The argument `name` should be a character of length 1',
             ' when calling Trial$save(). You specify it to be <',
             name, '> however, which is of class <',
             paste0(class(name), collapse = ', '), '>. ')
      }

      if(is.null(private$output)){
        private$output <- data.frame(trial = self$get_name())
      }

      if(!is_vector_length1(value) && !is.data.frame(value)){
        stop('For now only vector or data.frame can be saved during a trial. ')
      }

      if(is_vector(value)){
        if(length(value) > 1){
          stop('A vector object to be saved can only be of length one. ')
        }
        value <- data.frame(col = as.vector(value))
        colnames(value) <- name
      }else{ ## value is a data frame
        if(nrow(value) > 1){
          stop('A data frame to be saved can only contain one row. ')
        }

        if(name != ''){
          colnames(value) <- paste0(name, '_<', colnames(value), '>')
        }
      }

      for(cname in names(value)){
        if(cname %in% names(private$output)){
          if(!overwrite){
            stop(cname, ' has been used to name something in the output. ',
                 'Pick another name and try again. ')
          }else{
            warning(cname, ' exists in the output and is overwritten. ',
                    'Set overwrite = FALSE in save() if it is not intended. ',
                    immediate. = TRUE)
          }
        }

        ## this is flexible to assign scale value or a data frame to a cell in output
        private$output[, cname] <- I(list(value[, cname]))

      }

      ## move error message to the last column for a better display
      error_message <- private$output$error_message
      private$output$error_message <- NULL
      private$output$error_message <- error_message


    },

    #' @description
    #' row bind a data frame to existing data frame. If \code{name} is not
    #' existing in \code{Trial}, then it is equivalent to \code{Trial$save}.
    #' Extra columns in \code{value} are ignored. Columns in
    #' \code{Trial$custom_data[[name]]} but not in \code{value} are filled
    #' with \code{NA}.
    #' @param value a data frame to be saved. It can consist of one or
    #' multiple rows.
    #' @param name character. Name of object to be saved.
    bind = function(value, name){
      if(!(is.character(name) && length(name) == 1)){
        stop('name should be a character of length 1')
      }

      if(!is.data.frame(value)){
        stop('value should be a data frame. ')
      }

      if(is.null(private$custom_data[[name]])){
        private$custom_data[[name]] <- value
        return(invisible(NULL))
      }

      new_cols <- names(value)
      old_cols <- names(private$custom_data[[name]])

      if(!any(new_cols %in% old_cols)){
        stop('None of columns in <value> can be found in <', name, '>. ',
             'Cannot bind data to <', name, '>. \n',
             'Columns in <value>: \n<', paste0(new_cols, collapse = ', '), '>. \n',
             'Columns in <', name, '>: \n<', paste0(old_cols, collapse = ', '), '>. ')
      }

      miss_cols <- setdiff(old_cols, new_cols)
      extra_cols <- setdiff(new_cols, old_cols)

      if(length(extra_cols) > 0){
        warning('Extra columns <', paste0(extra_cols, collapse = ', '),
                '> present in <', name, '>, and are omitted. ')
        value <- value %>%
          dplyr::select(-all_of(extra_cols))
        print(value)
      }

      if(length(miss_cols) > 0){
        warning('Columns <', paste0(miss_cols, collapse = ', '),
                '> are missing when binding to <', name, '>. ',
                'Entries are filled with NA')
      }

      private$custom_data[[name]] <- bind_rows(private$custom_data[[name]], value)
      invisible(NULL)
    },

    #' @description
    #' save arbitrary (number of) objects into a trial so that users can use
    #' those to control the workflow. Most common use case is to store
    #' simulation parameters to be used in action functions.
    #' @param value value to be saved. Any type.
    #' @param name character. Name of the value to be accessed later.
    #' @param overwrite logic. \code{TRUE} if overwriting existing entries
    #' with warning, otherwise, throwing an error and stop.
    save_custom_data = function(value, name, overwrite = FALSE){

      if(name == ''){
        stop('name in custom_data cannot be empty. ')
      }

      if(name %in% names(private$custom_data)){
        if(!overwrite){
          stop(name, ' has been used to name something in custom data ',
               'Pick another name and try again. ')
        }else{
          warning(name, ' exists in custom_data and is overwritten. ',
                  'Set overwrite = FALSE in save_custom_data() ',
                  'if it is not intended. ',
                  immediate. = TRUE)
        }
      }

      private$custom_data[[name]] <- value
      private$.snapshot[['custom_data']][[name]] <- value
    },

    #' @description
    #' return saved custom data of specified name.
    #' @param name character. Name of custom data to be accessed.
    get_custom_data = function(name){
      if(!(name %in% names(private$custom_data))){
        stop(name, ' cannot be found in custom_data. ',
             'Check for bug or typo in data name. ',
             'Did you really save ', name,
             'by using Trial$save_custom_data(value, name) before? ')
      }

      private$custom_data[[name]]
    },

    #' @description
    #' alias of function \code{get_custom_data} to make it short and cool.
    #' @param name character. Name of custom data to be accessed.
    get = function(name){
      self$get_custom_data(name)
    },

    #' @description
    #' return a data frame of all current outputs saved by calling \code{save}.
    #' @param cols columns to be returned from \code{Trial$output}. If
    #' \code{NULL}, all columns are returned.
    #' @param simplify logical. Return value rather than a data frame of one
    #' column when \code{length(col) == 1} and \code{simplify == TRUE}.
    get_output = function(cols = NULL, simplify = TRUE){
      if(is.null(cols)){
        cols <- colnames(private$output)
      }

      if(!all(cols %in% names(private$output))){
        stop('Columns <', paste0(setdiff(cols, names(private$output)), collapse = ', '),
             '> are not found in trial$output. Check if there is a typo. ')
      }
      ret <- private$output[, cols, drop = FALSE]
      if(simplify && ncol(ret) == 1){
        return(ret[1, 1])
      }else{
        return(ret)
      }
    },

    #' @description
    #' mute all messages (not including warnings)
    #' @param silent logical.
    mute = function(silent){
      private$silent <- silent
    },

    #' @description
    #' calculate independent increments from a given set of milestones
    #' @param formula An object of class \code{formula} that can be used with
    #' \code{survival::coxph}. Must consist \code{arm} and endpoint in \code{data}.
    #' No covariate is allowed. Stratification variables are supported and can be
    #' added using \code{strata(...)}.
    #' @param placebo character. String of placebo in trial's locked data.
    #' @param milestones a character vector of milestone names in the trial, e.g.,
    #' \code{listener$get_milestone_names()}.
    #' @param alternative a character string specifying the alternative hypothesis,
    #' must be one of \code{"greater"} or \code{"less"}. No default value.
    #' \code{"greater"} means superiority of treatment over placebo is established
    #' by an hazard ratio greater than 1 when a log-rank test is used.
    #' @param planned_info a vector of planned accumulative number of event of
    #' time-to-event endpoint. It is named by milestone names.
    #' Note: \code{planned_info} can also be a character
    #' \code{"oracle"} so that planned number of events are set to be observed
    #' number of events, in that case inverse normal z statistics equal to
    #' one-sided logrank statistics. This is for the purpose of debugging only.
    #' In formal simulation, \code{"oracle"} should not be used if adaptation
    #' is present. Pre-fixed \code{planned_info} should be used to create
    #' weights in combination test that controls the family-wise error rate
    #' in the strong sense.
    #' @param ... subset condition that is compatible with \code{dplyr::filter}.
    #' \code{survdiff} will be fitted on this subset only to compute one-sided
    #' logrank statistics. It could be useful when a
    #' trial consists of more than two arms. By default it is not specified,
    #' all data will be used to fit the model.
    #'
    #' @return
    #' This function returns a data frame with columns:
    #' \describe{
    #' \item{\code{p_inverse_normal}}{one-sided p-value for inverse normal test
    #' based on logrank test (alternative hypothesis: risk is higher in placebo arm).
    #' Accumulative data is used. }
    #' \item{\code{z_inverse_normal}}{z statistics of \code{p_inverse_normal}.
    #' Accumulative data is used. }
    #' \item{\code{p_lr}}{one-sided p-value for logrank test
    #'  (alternative hypothesis: risk is higher in placebo arm).
    #' Accumulative data is used. }
    #' \item{\code{z_lr}}{z statistics of \code{p_lr}.
    #' Accumulative data is used. }
    #' \item{\code{info}}{observed accumulative event number. }
    #' \item{\code{planned_info}}{planned accumulative event number. }
    #' \item{\code{info_pbo}}{observed accumulative event number in placebo. }
    #' \item{\code{info_trt}}{observed accumulative event number in treatment arm. }
    #' \item{\code{wt}}{weights in \code{z_inverse_normal}. }
    #' }
    #'
    #' @examples
    #'
    #' \dontrun{
    #' trial$independentIncrement('pfs', 'pbo', listener$get_milestone_names(), 'less', 'oracle')
    #' }
    independentIncrement = function(formula, placebo, milestones, alternative,
                                    planned_info,
                                    ...){

      if(!identical(planned_info, 'oracle') && length(milestones) != length(planned_info)){
        stop('milestones and planned_info should be of same length. ')
      }

      ## by doing this, milestones in function argument can be in arbitrary order
      milestone_time <- sort(self$get_milestone_time(milestones))
      milestones <- names(milestone_time)

      info <- c() ## observed accumulated events
      lr <- c() ## one-sided log rank statistics
      info_pbo <- c()
      info_trt <- c()
      plan_best_info <- ifelse(identical(planned_info, 'oracle'), TRUE, FALSE)
      if(plan_best_info){
        planned_info <- c()
      }else{
        ## make it accumulative
        planned_info <- planned_info[milestones] %>% cumsum()
      }


      ## Get label of treated arm from subset data (through ...)
      ## We need this label to call Trials$get_arm_removal_time

      # Prepare the data based on condition in ...
      analysis_set <- if(...length() == 0){
        self$get_locked_data(tail(milestones, 1))
      }else{
        tryCatch({
          self$get_locked_data(tail(milestones, 1)) %>% dplyr::filter(...)
        },
        error = function(e){
          stop('Error when filtering data in independentIncrement(). ',
               'Please check condition in ..., ',
               'which should be compatible with dplyr::filter. ')
        })
      }

      trt_arm <- setdiff(unique(analysis_set$arm), placebo)
      if(length(trt_arm) == 0){
        stop('No treatment arm can be used in independentIncrement(). ',
             'Check your ... argument. ')
      }

      if(length(trt_arm) > 1){
        stop('More than one treatment arm <',
             paste0(trt_arm, collapse = ', '),
             '> are passed into independentIncrement(). ',
             'Check your ... argument. ')
      }

      rm(analysis_set)

      n_pbo <- c()
      n_trt <- c()
      trt_str <- c()
      milestone_name <- c()
      for(i in seq_along(milestones)){
        milestone_name[i] <- milestones[i]

        ## We assume that placebo and only one treated arm are used in independentIncrement
        ## thus lr_fit should be a data frame of one row
        lr_fit <- fitLogrank(formula, placebo, self$get_locked_data(milestones[i]),
                             alternative, ..., tidy = FALSE)
        if(nrow(lr_fit) > 1){
          stop('Trials$independentIncrement() should be applied to one treated arm at a time. ',
               'Check the entry where you call independentIncrement(). ',
               'Usually you can use its subsetting argument ... to meet this assumption/requirement. ')
        }

        info[i] <- lr_fit$info
        lr[i] <- lr_fit$z
        info_pbo[i] <- lr_fit$info_pbo
        info_trt[i] <- lr_fit$info_trt

        n_pbo[i] <- lr_fit$n_pbo
        n_trt[i] <- lr_fit$n_trt
        trt_str[i] <- lr_fit$arm

        if(plan_best_info){
          planned_info[i] <- lr_fit$info
        }
      }

      names(info) <- milestones

      if(any(diff(planned_info) < 0)){
        stop('milestones and planned_info should be in the same order. ')
      }

      if(any(diff(info) < 0)){
        stop('Debug this as info should be non-decreasing. ')
      }

      ii <- c() ## independent increments
      wt <- c() ## weight in inverse normal statistics
      inverse_normal <- c() ## inverse normal test statistics

      stage_info <- c()
      stage_n_pbo <- c()
      stage_n_trt <- c()
      for(i in seq_along(info)){
        if(i == 1){
          wt[i] <- sqrt(planned_info[i])
          ii[i] <- lr[i]
          inverse_normal[i] <- lr[i]

          stage_info[i] <- info[i]
          stage_n_pbo[i] <- n_pbo[i]
          stage_n_trt[i] <- n_trt[i]

          next
        }

        stage_info[i] <- info[i] - info[i - 1]
        stage_n_pbo[i] <- n_pbo[i] - n_pbo[i - 1]
        stage_n_trt[i] <- n_trt[i] - n_trt[i - 1]
        wt[i] <- sqrt(planned_info[i] - planned_info[i - 1])

        arm_removal_time <- self$get_arm_removal_time(arm = trt_arm)
        milestone_triggering_time <- self$get_milestone_time(milestone_name = names(info)[i])


        ## Use "<", not "<="!!
        ## When arm_removal_time == milestone_triggering_time, it means the arm
        ## is just removed from the trial at that milestone. Thus, data from the
        ## arm can still be used to update testing statistics, no need to be
        ## specified as +/-Inf
        if(arm_removal_time < milestone_triggering_time){
          ## This means this treatment arm has been removed from the trial BEFORE milestone i
          ## Set z statistic of independent increment to be an extremely value
          ## so that inverse normal combination test would always accept
          ## the neutral null hypothesis (p-value is close or equal to 1.0)
          ii[i] <- ifelse(alternative == 'greater', -Inf, Inf) ## 10 * qnorm(.Machine$double.eps) ## about -81
        }else{
          ii[i] <- (sqrt(info[i]) * lr[i] - sqrt(info[i - 1]) * lr[i - 1]) /
            sqrt(stage_info[i])
        }
        inverse_normal[i] <- sum(wt * ii) / sqrt(sum(wt^2))
      }

      ret <-
        data.frame(
          milestone = milestone_name,
          milestone_time = unname(milestone_time),
          p_inverse_normal =
            if(alternative == 'greater'){
              1 - pnorm(inverse_normal)
            }else{
              pnorm(inverse_normal)
            },
          z_inverse_normal = inverse_normal,
          p_logrank =
            if(alternative == 'greater'){
              1 - pnorm(lr)
            }else{
              pnorm(lr)
            },
          z_logrank = lr,
          info = info,
          planned_info = planned_info,
          info_pbo = info_pbo,
          info_trt = info_trt,
          wt = wt,
          z_ii = ii, # stage-wise, independent increment
          n_pbo = n_pbo,
          n_trt = n_trt,
          stage_info = stage_info,
          stage_n_pbo = stage_n_pbo,
          stage_n_trt = stage_n_trt,
          trt_str = trt_str
        )

      if(any(ret$stage_info < 30)){
        ret_ <- ret %>%
          dplyr::filter(stage_info < 30) %>%
          dplyr::select(milestone, milestone_time, planned_info, info, stage_info, stage_n_pbo, stage_n_trt, trt_str)

        warning('In the arm(s) <',
                paste0(unique(ret_$trt_str), collapse = ', '),
                '>, stage-wise information (number of events) are lower than 30 (see stage_* below). \n',
                'Make sure that such a low stage-wise information is sufficient to maintain normality of independent increments of logrank statistics. ',
                immediate. = TRUE)
        message(paste0(capture.output(ret_), collapse = "\n"))
        message("\033[31m>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\033[0m\n")
      }

      ret
    },

    #' @description
    #' carry out closed test based on Dunnett method under group sequential
    #' design.
    #' @param formula An object of class \code{formula} that can be used with
    #' \code{survival::coxph}. Must consist \code{arm} and endpoint in \code{data}.
    #' No covariate is allowed. Stratification variables are supported and can be
    #' added using \code{strata(...)}.
    #' @param placebo character. Name of placebo arm.
    #' @param treatments character vector. Name of treatment arms to be used in
    #' comparison.
    #' @param milestones character vector. Names of triggered milestones at which either
    #' adaptation is applied or statistical testing for endpoint is performed.
    #' Milestones in \code{milestones} does not need to be sorted by their triggering time.
    #' @param alternative a character string specifying the alternative hypothesis,
    #' must be one of \code{"greater"} or \code{"less"}. No default value.
    #' \code{"greater"} means superiority of treatment over placebo is established
    #' by an hazard ratio greater than 1 when a log-rank test is used.
    #' @param planned_info a data frame of planned number of events of
    #' time-to-event endpoint in each stage and each arm. Milestone names, i.e.,
    #' \code{milestones} are row names of \code{planned_info}, and arm names, i.e.,
    #' \code{c(placebo, treatments)} are column names.
    #' Note that it is not the accumulative but stage-wise event numbers.
    #' It is usually not easy to determine these numbers in practice, simulation
    #' may be used to get estimates.
    #' Note: \code{planned_info} can also be a character
    #' \code{"default"} so that \code{planned_info} are set to be number
    #' of newly randomized patients in the control arm in each of the stages.
    #' This assumes that
    #' event rate do not change over time and/or sample ratio between placebo
    #' and a treatment arm does not change as well, which may not be true.
    #' It is for the purpose of debugging or rapid implementation
    #' only. Using simulation to pick \code{planned_info} is recommended in
    #' formal simulation study. Another issue with \code{planned_info} set to
    #' be \code{"default"} is that it is possible patient recruitment is done
    #' before a specific stage, as a result, \code{planned_info} is zero which
    #' can crash the program.
    #' @param ... subset condition that is compatible with \code{dplyr::filter}.
    #' \code{survdiff} will be fitted on this subset only to compute one-sided
    #' logrank statistics. It could be useful when comparison is made on a
    #' subset of treatment arms. By default it is not specified,
    #' all data (placebo plus one treatment arm at a time) in the locked data
    #' are used to fit the model.
    #'
    #' @details
    #' This function computes stage-wise p-values for each of the intersection
    #' hypotheses based on Dunnett test. If only one treatment arm is present,
    #' it is equivalent to compute the stage-wise p-values of elemental
    #' hypotheses. This function also computes inverse normal combination
    #' test statistics at each of the stages.
    #' The choice of \code{planned_info} can affect the calculation of
    #' stage-wise p-values. Specifically, it is used to compute
    #' the columns \code{observed_info} and \code{p_inverse_normal} in returned
    #' data frame, which will be used in \code{Trial$closedTest()}.
    #' The choice of \code{planned_info} can affect the result of
    #' \code{Trial$closedTest()} so user should chose it with caution.
    #'
    #' Note that in \code{Trial$closedTest()},
    #' \code{observed_info}, which is derived from \code{planned_info}, will
    #' lead to the same closed testing results up to a constant. This is because
    #' the closed test uses information fraction
    #' \code{observed_info/sum(observed_info)}. As a result, setting
    #' \code{planned_info} to, e.g., \code{10 * planned_info} should give same
    #' closed test results.
    #'
    #' Based on numerical study, setting \code{planned_info = "default"} leads
    #' to a much higher power (roughly 10\%) than setting \code{planned_info} to
    #' median of event numbers at stages, which can be determined by simulation.
    #' I am not sure if regulator would support such practice. For example,
    #' if a milestone (e.g., interim analysis) is triggered at a pre-specified
    #' calendar time, the number of randomized patients is random and is unknown
    #' when planning the trial. If I understand it correctly, regulator may want
    #' the information fraction in closed test (combined with Dunnett test) to
    #' be pre-fixed. In addition, this choice for \code{planned_info} assumes
    #' that the event rates does not change over time which is obviously not
    #' true. It is recommended to always use pre-fixed \code{planned_info} for
    #' restrict control of family-wise error rate. It should be pointed out
    #' that the choice of pre-fixed \code{planned_info} can affect statistical
    #' power significantly so fine-tuning may be required.
    #'
    #' @return a list with element names like \code{arm_name},
    #' \code{arm1_name|arm2_name}, \code{arm1_name|arm2_name|arm3_name}, etc.,
    #' i.e., all possible combination of treatment arms in comparison. Each
    #' element is a data frame, with its column names self-explained. Specifically,
    #' the columns \code{p_inverse_normal}, \code{observed_info},
    #' \code{is_final} can be used with \code{GroupSequentialTest} to perform
    #' significance test.
    #'
    #' @examples
    #' \dontrun{
    #' trial$dunnettTest('pfs', 'pbo', c('high dose', 'low dose'),
    #'                   listener$get_milestone_names(), 'default')
    #' }
    #'
    dunnettTest = function(formula, placebo, treatments, milestones, alternative,
                           planned_info, ...){

      alternative <- match.arg(alternative, choices = c('greater', 'less'))

      if(!identical(planned_info, 'default')){
        if(!('data.frame' %in% class(planned_info))){
          stop('planned_info should be a data frame of planned information at each of the stages. ')
        }
        if(nrow(planned_info) != length(milestones)){
          stop('milestones and planned_info should be of same length. ')
        }

        if(ncol(planned_info) != length(treatments) + 1){
          stop('length(planned_info) should be equal to length(treatments) + 1, i.e., ',
               length(treatment) + 1, '. ')
        }

        if(!setequal(names(planned_info), c(placebo, treatments))){
          stop('planned_info should use placebo and treatments\' names, <',
               paste0(c(placebo, treatments), collapse = ', '),
               '>, for its column names. ',
               paste0(setdiff(names(planned_info), c(placebo, treatments)), collapse = ', '),
               ' are not accepted. ')
        }

        planned_info <- planned_info[, c(placebo, treatments), drop = FALSE]

        if(!setequal(rownames(planned_info), milestones)){
          stop('planned_info should use milestone names for its row names. ')
        }

        planned_info <- planned_info[milestones, , drop = FALSE]
      }

      ## by doing this, milestones in function argument can be in arbitrary order

      if(!all(milestones %in% names(private$milestone_time))){
        stop('Milestone(s) <', paste0(setdiff(milestones, names(private$milestone_time)), collapse = ', '),
             '> haven\'t been triggered yet, so are unable to be used as "milestones" in dunnettTest(). ')
      }

      milestone_time <- sort(self$get_milestone_time(milestones))
      milestones <- names(milestone_time)

      ii <- list() ## calculate independent increments for each of treatment arms
      for(i in seq_along(treatments)){
        trt_str <- treatments[i]

        if(identical(planned_info, 'default')){
          planned_info_ <- 'oracle'
        }else{
          ## this is a vector named by milestone names
          planned_info_ <- planned_info[milestones, , drop = FALSE] %>%
            select(c(placebo, trt_str)) %>%
            rowSums()
        }

        ii[[trt_str]] <-
          self$independentIncrement(formula, placebo, milestones, alternative,
                                    ## it doesn't matter what is used for planned_info
                                    ## because we only use z_ii in returned object
                                    ## which is irrelevant to planned_info
                                    planned_info = planned_info_,
                                    arm %in% c(placebo, trt_str), ...)

      }


      all_trt <- names(ii)

      all_combn <-
        unlist(
          lapply(seq_along(all_trt),
                 function(k)
                   combn(all_trt, k, simplify = FALSE)
                 ),
          recursive = FALSE
        )

      createArmCombination <- function(comb){
        ## use sort() to allow user ignore order of treatments in argument
        paste0(sort(comb), collapse = '|')
      }

      stage_dunnett_pvalue <- list()
      inverse_normal_dunnett_pvalue <- list()
      for(comb in all_combn){
        inverse_normal_dunnett_pvalue[[createArmCombination(comb)]] <- NULL
        for(milestone_name in milestones){ ## milestones is already ordered by triggering time
          z_ii <- NULL
          ratio_trt <- NULL
          stage_n_pbo <- NULL
          available_trt <- NULL
          if(!identical(planned_info, 'default')){
            pinfo <- planned_info[milestone_name, placebo]
          }
          for(trt in comb){
            ii0 <- ii[[trt]] %>% dplyr::filter(milestone %in% milestone_name)

            if(!identical(planned_info, 'default')){
              pinfo <- pinfo + planned_info[milestone_name, trt]
            }

            ## we expect stage_n_pbo is a constant vector
            stage_n_pbo <- c(stage_n_pbo, ii0$stage_n_pbo)


            arm_removal_time <- self$get_arm_removal_time(arm = trt)
            milestone_triggering_time <- self$get_milestone_time(milestone_name = milestone_name)


            if(arm_removal_time >= milestone_triggering_time){
              z_ii <- c(z_ii, ii0$z_ii)
              ratio_trt <- c(ratio_trt,
                             sqrt(ii0$stage_n_trt / (ii0$stage_n_pbo + ii0$stage_n_trt)))

              available_trt <- c(available_trt, trt)
            }
          }

          name1 <- paste0(createArmCombination(comb), '@', milestone_name)
          if(length(available_trt) > 0){
            name2 <- paste0(paste0(sort(available_trt), collapse = '|'), '@', milestone_name)
            if(!is.null(stage_dunnett_pvalue[[name2]])){
              stage_dunnett_pvalue[[name1]] <- stage_dunnett_pvalue[[name2]]
            }else{
              if(alternative == 'greater'){
                lower <- rep(-Inf, length(available_trt))
                upper <- rep(max(z_ii), length(available_trt))
              }else{
                lower <- rep(min(z_ii), length(available_trt))
                upper <- rep(Inf, length(available_trt))
              }

              corr <- outer(ratio_trt, ratio_trt,
                            function(x, y) x * y)
              diag(corr) <- 1.

              if(length(available_trt) > 1){
                ## no matter what alternative is
                stage_dunnett_pvalue[[name1]] <-
                  1 - pmvnorm(lower = lower, upper = upper, sigma = corr)
              }else{
                if(alternative == 'greater'){
                  stage_dunnett_pvalue[[name1]] <- 1 - pnorm(upper)
                }else{
                  stage_dunnett_pvalue[[name1]] <- pnorm(lower)
                }
              }

              stage_dunnett_pvalue[[name2]] <- stage_dunnett_pvalue[[name1]]
            }

          }else{
            stage_dunnett_pvalue[[name1]] <- 1
          }

          if(identical(planned_info, 'default')){
            pinfo <- unique(stage_n_pbo)
            stopifnot(length(pinfo) == 1)
          }

          get_time_variable <- function(formula) {
            lhs <- formula[[2]]  # extract LHS of Surv(...)
            if(as.character(lhs[[1]]) != 'Surv'){
              stop('Left side is not a Surv() object')
            }

            time_var <- lhs[[2]] # the second element is column name of time (the first one is "Surv")
            as.character(time_var)
          }

          tmp <-
            data.frame(
              endpoint = get_time_variable(formula),
              milestone = milestone_name,
              milestone_time = unname(milestone_time[milestone_name]),
              p_logrank = ii0$p_logrank,
              stage_p = stage_dunnett_pvalue[[name1]],
              stage_planned_info = pinfo)

          inverse_normal_dunnett_pvalue[[createArmCombination(comb)]] <-
            rbind(inverse_normal_dunnett_pvalue[[createArmCombination(comb)]], tmp)
          rm(tmp)

          tmp_ <- inverse_normal_dunnett_pvalue[[createArmCombination(comb)]]
          if(any(tmp_$stage_planned_info == 0)){
            idx_ <- which(tmp_$stage_planned_info == 0)[1]
            if(idx_ == 1){
              stop('Using planned_info = "default" in dunnettTest() causes issues under your simulation settings. \n',
                   'It specified weights of closed test based on number of newly randomized patients between milestones. \n',
                   'In your case, no patient is recruited before milestone <', tmp_$milestone[idx_], '>. \n',
                   'Contact the package author if you do not know how to specify <planned_info> after reading manual of Trials$dunnettTest. \n')
            }else{
              stop('Using planned_info = "default" in dunnettTest() causes issues under your simulation settings. \n',
                   'It specified weights of closed test based on number of newly randomized patients between milestones. \n',
                   'In your case, no patient is recruited between milestones <', tmp_$milestone[idx_ - 1],
                   '> and <', tmp_$milestone[idx_], '>. \n',
                   'Contact the package author if you do not know how to specify <planned_info> after reading manual of Trials$dunnettTest. \n')
            }
          }

        }

      }

      for(i in seq_along(inverse_normal_dunnett_pvalue)){
        tmp <- inverse_normal_dunnett_pvalue[[i]]
        tmp$wt <- sqrt(tmp$stage_planned_info)

        tmp$z_inverse_normal <- cumsum(tmp$wt * qnorm(1 - tmp$stage_p)) / sqrt(cumsum(tmp$wt^2))
        tmp$p_inverse_normal <- 1 - pnorm(tmp$z_inverse_normal)
        tmp$planned_info <- cumsum(tmp$stage_planned_info) ## used as observed_info in GroupSequentialTest
        ## the last entry in is_final should be set to TRUE when calling GroupSequentialTest
        ## However, we don't do this here because not all rows in inverse_normal_dunnett_pvalue
        ## will be used to test a specific endpoint (e.g., PFS may not be tested
        ## at all milestone time). Instead, set the last entry to TRUE before
        ## performing the significance test
        # tmp$is_final <- FALSE
        inverse_normal_dunnett_pvalue[[i]] <- tmp
        rm(tmp)
      }

      class(inverse_normal_dunnett_pvalue) <- c('dunnett', class(inverse_normal_dunnett_pvalue))
      inverse_normal_dunnett_pvalue

    },

    #' @description
    #' perform closed test based on Dunnett test
    #' @param dunnett_test object returned by \code{Trial$dunnettTest()}.
    #' @param treatments character vector. Name of treatment arms to be used in
    #' comparison.
    #' @param milestones character vector. Names of triggered milestones at which
    #' significance testing for endpoint is performed in closed test.
    #' Milestones in \code{milestones} does not need to be sorted by their triggering time.
    #' @param alpha numeric. Allocated alpha.
    #' @param alpha_spending alpha spending function. It can be \code{"asP"} or
    #' \code{"asOF"}. Note that theoretically it can be \code{"asUser"}, but
    #' it is not tested. It may be supported in the future.
    #'
    #' @return a data frame of columns \code{arm}, \code{decision}
    #' (final decision on a hypothesis at the end of trial, \code{"accept"} or \code{"reject"}),
    #' \code{milestone_at_reject}, and \code{reject_time}.
    #' If a hypothesis is accepted at then end of a trial,
    #' \code{milestone_at_reject} is \code{NA}, and \code{reject_time} is \code{Inf}.
    #'
    #' Note that if a hypothesis is tested at multiple milestones, the final
    #' \code{decision} will be \code{"accept"} if it is accepted at at least
    #' one milestone. The \code{decision} is \code{"reject"} only if the hypothesis
    #' is rejected at all milestones.
    #'
    #' @examples
    #' \dontrun{
    #' dt <- trial$dunnettTest(
    #'   Surv(pfs, pfs_event) ~ arm,
    #'   placebo = 'pbo',
    #'   treatments = c('high dose', 'low dose'),
    #'   milestones = c('dose selection', 'interim', 'final'),
    #'   data.frame(pbo = c(100, 160, 80),
    #'              low = c(100, 160, 80),
    #'              high = c(100, 160, 80),
    #'              row.names = c('dose selection', 'interim', 'final'))
    #'
    #' trial$closedTest(dt, treatments = c('high dose', 'low dose'),
    #'                  milestones = c('interim', 'final'),
    #'                  alpha = 0.025, alpha_spending = 'asOF')
    #' }
    #'
    closedTest = function(dunnett_test, treatments, milestones, alpha, alpha_spending = c('asP', 'asOF')){

      alpha_spending <- match.arg(alpha_spending)

      if(!('dunnett' %in% class(dunnett_test))){
        stop('dunnett_test must be of class "dunnett". ',
             'Make sure that it is returned by dunnettTest(). ')
      }

      all_combn <-
        unlist(
          lapply(seq_along(treatments),
                 function(k)
                   combn(treatments, k, simplify = FALSE)
          ),
          recursive = FALSE
        )

      for(i in seq_along(dunnett_test)){

        if(all(milestones %in% dunnett_test[[i]]$milestone)){
          dunnett_test[[i]] <- dunnett_test[[i]] %>%
            dplyr::filter(milestone %in% milestones) %>%
            arrange(milestone_time) %>%
            mutate(is_final = FALSE)

          dunnett_test[[i]]$is_final[nrow(dunnett_test[[i]])] <- TRUE
        }else{
          stop('Milestones <',
               paste0(setdiff(milestones, dunnett_test[[i]]$milestone), collapse = ', '),
               '> are not in dunnett_test. ')
        }
      }

      createArmCombination <- function(comb){
        ## use sort() to allow user ignore order of treatments in argument
        paste0(sort(comb), collapse = '|')
      }

      for(comb in all_combn){
        name1 <- createArmCombination(comb)
        if(!(name1 %in% names(dunnett_test))){
          stop('Combination <', name1, '> is not found in dunnett_test. ',
               'Make sure that <', paste0(treatments, collapse = ', '),
               '> are all used with dunnettTest() when computing dunnett_test. ')
        }
      }

      gst <- list()
      gst_res <- list()
      for(i in seq_along(dunnett_test)){
        comb <- names(dunnett_test)[i]
        gst[[comb]] <- GroupSequentialTest$new(
          name = comb,
          alpha = alpha, alpha_spending = alpha_spending,
          planned_max_info = max(dunnett_test[[comb]]$planned_info)
        )

        gst[[comb]]$test(observed_info = dunnett_test[[comb]]$planned_info,
                         is_final = dunnett_test[[comb]]$is_final,
                         p_values = dunnett_test[[comb]]$p_inverse_normal)

        gst_res[[comb]] <- gst[[comb]]$get_trajectory() %>%
          mutate(milestone = dunnett_test[[comb]]$milestone) %>%
          mutate(milestone_time = dunnett_test[[comb]]$milestone_time)
      }

      # print(gst_res)

      tmp0 <-
        data.frame(
          arm = NA,
          comb = NA,
          reject = FALSE,
          milestone_at_reject = NA,
          reject_time = Inf,
          stageLevels = NA_real_,
          obs_p_value = NA_real_
        )

      ret <- list()
      for(trt in treatments){

        ret[[trt]] <- NULL
        for(comb in names(gst_res)){
          if(!(trt %in% unlist(strsplit(comb, split = '\\|')))){
            next
          }

          tmp <- tmp0
          tmp$arm <- trt
          tmp$comb <- comb
          if('reject' %in% gst_res[[comb]]$decision){ ## trial reach the endpoint
            tmp$reject <- TRUE
            idx <- which(gst_res[[comb]]$decision %in% 'reject')[1]
            tmp$milestone_at_reject <- gst_res[[comb]]$milestone[idx]
            tmp$reject_time <- gst_res[[comb]]$milestone_time[idx]
            tmp$stageLevels <- gst_res[[comb]]$stageLevels[idx]
            tmp$obs_p_value <- gst_res[[comb]]$obs_p_value[idx]
          }

          ret[[trt]] <- rbind(ret[[trt]], tmp)
          rm(tmp)
        }

      }


      # print(ret)
      ret_ <- NULL
      for(trt in names(ret)){
        if(all(ret[[trt]]$reject)){
          idx <- which.max(ret[[trt]]$reject_time)[1]
          ret_ <- rbind(ret_,
                        data.frame(
                          arm = trt,
                          decision = 'reject',
                          milestone_at_reject = ret[[trt]]$milestone_at_reject[idx],
                          reject_time = ret[[trt]]$reject_time[idx]))
        }else{
          ret_ <- rbind(ret_,
                        data.frame(
                          arm = trt,
                          decision = 'accept',
                          milestone_at_reject = NA,
                          reject_time = Inf))
        }
      }
      ret_
    },

    #' @description
    #' return random seed
    get_seed = function(){
      private$seed
    },

    #' @description
    #' print a trial
    print = function(){
      white_text_blue_bg <- "" ## "\033[37;44m"
      reset <- "" ## "\033[0m"  # Reset to default color
      logo <- '\u2695\u2695' ## stringi::stri_escape_unicode('⚕')

      cat(white_text_blue_bg, logo, 'Trial Name: ', self$get_name(), reset, '\n')
      cat(white_text_blue_bg, logo, 'Description: ', self$get_description(), reset, '\n')
      cat(white_text_blue_bg, logo, '# of Arms: ', self$get_number_arms(), reset, '\n')
      cat(white_text_blue_bg, logo, 'Registered Arms: ',
          paste0(self$get_arms_name(), collapse = ', '), reset, '\n')
      cat(white_text_blue_bg, logo, 'Sample Ratio: ',
          paste0(self$get_sample_ratio(), collapse = ', '), reset, '\n')
      cat(white_text_blue_bg, logo, '# of Patients: ', self$get_number_patients(), reset, '\n')
      cat(white_text_blue_bg, logo, 'Planned Duration: ', self$get_duration(), reset, '\n')
      cat(white_text_blue_bg, logo, 'Random Seed: ', self$get_seed(), reset, '\n')

      invisible(self)

    },

    #' @description
    #' return a snapshot of a trial before it is executed.
    get_snapshot_copy = function(){
      private$.snapshot
    },

    #' @description
    #' make a snapshot before running a trial. This can be useful when
    #' resetting a trial. This is only called when initializing a `Trial`
    #' object, when arms have not been added yet.
    make_snapshot = function() {

      private$.snapshot <- list()

      for(field in names(private)){
        if(field %in% c('.snapshot', 'permuted_block_randomization', 'validate_arguments')){
          next
        }
        private$.snapshot[[field]] <- private[[field]]
      }

    },

    #' @description
    #' make a snapshot of arms
    make_arms_snapshot = function(){
      arm_names <- self$get_arms_name()
      arms <- self$get_arms()
      sample_ratio <- self$get_sample_ratio()
      stopifnot(length(arms) == length(sample_ratio))
      stopifnot(length(arm_names) == length(sample_ratio))

      private$.snapshot$arms <- list()
      for(arm_name in arm_names){
        private$.snapshot$arms[[arm_name]] <- arms[[arm_name]]$clone(deep = TRUE)
      }
      private$.snapshot$sample_ratio <- sample_ratio

    },

    #' @description
    #' reset a trial to its snapshot taken before it was executed. Seed will be
    #' reassigned with a new one. Enrollment time are re-generated. If the trial
    #' already have arms when this function is called, they are added back to
    #' recruit patients again.
    reset = function() {

      arms <- private$.snapshot[['arms']]
      sample_ratio <- private$.snapshot[['sample_ratio']]

      if(is.null(arms) || is.null(sample_ratio)){
        warning('arms is not found in the snapshot. ',
                'There is nothing to be reset. ')
        return(invisible(NULL))
      }

      for (field in names(private$.snapshot)){
        private[[field]] <- private$.snapshot[[field]]
      }

      private$milestone_time <- c()
      private$trial_data <- NULL
      private$enroll_time <- NULL
      private$randomization_queue <- NULL
      private$n_enrolled_patients <- NULL
      private$sample_ratio <- NULL
      private$arms <- list()

      private$seed <- sample(.Machine$integer.max, 1)
      private$output$seed <- private$seed
      set.seed(private$seed)

      private$enroll_time <-
        sort(self$get_enroller()(n = private$n_patients), decreasing = FALSE)

      arms <- private$.snapshot[['arms']]
      sample_ratio <- private$.snapshot[['sample_ratio']]

      if(length(arms) > 0){
        stopifnot(length(arms) == length(sample_ratio))
        ## must call add_arms to add arms back to the trial
        ## because this will generate some data as well.
        ## set self$arms <- ... does not fulfill the purpose of
        ## resetting a trial
        do.call(self$add_arms, c(list(sample_ratio), arms))
      }

    },

    #' @description
    #' save time when an arm is added to the trial
    #' @param arm name of added arm.
    #' @param time time when an arm is added.
    set_arm_added_time = function(arm, time){
      if(!is.null(private$arm_time[[arm]][['time_added']])){
        stop('The time the arm <', arm, '> was added to the trial has already been recorded <',
             private$arm_time[[arm]][['time_added']], '>. You cannot overwrite it. ',
             'Usually this indicates an error in your codes. ')
      }
      stopifnot(time >= 0)
      private$arm_time[[arm]][['time_added']] <- time
    },

    #' @description
    #' get time when an arm is added to the trial
    #' @param arm arm name.
    get_arm_added_time = function(arm){
      ## arm is not in the trial
      if(is.null(private$arm_time[[arm]][['time_added']])){
        if(!private$silent){
          message('Arm <', arm, '> is not in the trial. ')
        }
        return(Inf)
      }else{
        return(private$arm_time[[arm]][['time_added']])
      }
    },

    #' @description
    #' save time when an arm is removed to the trial
    #' @param arm name of removed arm.
    #' @param time time when an arm is removed.
    set_arm_removal_time = function(arm, time){
      if(!is.null(private$arm_time[[arm]][['time_removed']])){
        stop('The time the arm <', arm, '> was removed from the trial has already been recorded <',
             private$arm_time[[arm]][['time_removed']], '>. You cannot overwrite it. ',
             'Usually this indicates an error in your codes. ')
      }
      stopifnot(time >= 0)
      private$arm_time[[arm]][['time_removed']] <- time
    },

    #' @description
    #' get time when an arm is removed from the trial
    #' @param arm arm name.
    get_arm_removal_time = function(arm){
      ## arm is not in the trial
      if(is.null(private$arm_time[[arm]][['time_removed']])){
        if(!private$silent){
          # message('Arm <', arm, '> is still in the trial. ')
        }
        return(Inf)
      }else{
        return(private$arm_time[[arm]][['time_removed']])
      }
    }

  ),

  private = list(
    seed = NULL,
    name = NULL,
    description = NULL,
    n_patients = NULL,
    duration = NULL,
    n_enrolled_patients = NULL,
    sample_ratio = NULL,

    arms = list(),
    now = 0, # current time point of a trial. Change to the data of patients
             # enrolled before are not allowed. When a trial is created,
             # now = 0. If a milestone triggers a data lock (in Milestones class),
             # now will be set to the time of data lock (e.g. futility, interim).
             # When adding or removing a arm at a milestone,
             # private$randomizatioon_queue[private$enroll_time > now] will be
             # regenerated. This is important because randomization needs to be
             # done with possibly changed sample ratio. enroll_patients() is
             # then executed with updated randomizatioon_queue.
    randomization_queue = NULL,
    enroller = NULL,
    enroll_time = NULL,

    dropout = NULL, # function to generate dropout time

    trial_data = NULL,
    locked_data = list(),

    milestone_time = c(),
    arm_time = list(), # time when arms are added to or removed from the trial

    silent = FALSE,

    output = NULL,

    ## User can save whatever they want in an unstructured way (list)
    ## This is useful for simulation to store some setting parameters
    ## that could be used in action functions.
    custom_data = list(),

    validate_arguments =
      function(name, n_patients, duration, description, seed,
               enroller, dropout, silent, ...){

      stopifnot(is.null(seed) || is.wholenumber(seed))
      stopifnot(is.character(name))
      stopifnot(is.character(description))

      stopifnot(is.numeric(n_patients) &&
                  (length(n_patients) == 1) &&
                  is.wholenumber(n_patients))

      stopifnot(is.numeric(duration) &&
                  (length(duration) == 1) &&
                  duration > 0)

      stopifnot(is.function(enroller))
      stopifnot(is.null(dropout) || is.function(dropout))

      stopifnot(is.logical(silent))

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
             'e.g., calling $add_arms twice and no milestone happen in between. \n',
             'However, whenever a milestone is triggered during a trial, ',
             'patients being enrolled after the milestone time will be rolled back, ',
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

      if(!private$silent){
        message('Randomization is done for ', length(randomization_queue),
                ' potential patients. \n') #,
                # 'Make sure that you only see this message when initializing a trial, \n',
                # 'or after adding/removing an arm from the trial. \n',
                # 'Otherwise it may indicator a potential issue. \n')
      }
    },

    .snapshot = list()

  )
)
