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
#' risk2$odds_ratio <- .8
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
#'   enroller = rexp, rate = log(2) / 5)
#' trial$add_arms(sample_ratio = c(1, 2), placebo, active)
#' trial
#' table(trial$get_randomization_queue())
#' trial$get_enroll_time(1:4)
#' trial$enroll_a_patient()
#' trial$enroll_a_patient()
#' trial$get_enroll_time(1:2)
#' table(trial$get_randomization_queue())
#'
#' trial$get_trial_data()
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
    #' @param enroller a function returning a vector enrollment time for
    #' patients. Its first argument is the number of enrolled patients.
    #' @param ... arguments of \code{enroller}.
    initialize =
      function(
        name,
        n_patients,
        description = name,
        enroller,
        ...
      ){

        private$validate_arguments(
          name, n_patients, description, enroller, ...)

        private$arms <- list()
        private$name <- name
        private$description <- description
        private$n_patients <- n_patients
        private$n_enrolled_patients <- 0
        private$trial_data <- NULL

        private$enroller <- DynamicFunction(enroller, simplify = TRUE, ...)
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
    #' add a list of arms to the trial
    #' @param sample_ratio integer vector. Sample ratio for permuted block
    #' randomization. It will be appended to existing sample ratio in the trial.
    #' @param ... one or more objects of class \code{Arm}
    add_arms = function(sample_ratio, ...){

      stopifnot(is.numeric(sample_ratio) && all(is.wholenumber(sample_ratio)))

      arm_list <- list(...)
      stopifnot(length(arm_list) == length(sample_ratio))

      arm_names <- NULL
      for(arm in arm_list){
        stopifnot(inherits(arm, 'Arm'))

        if(arm$get_name() %in% self$get_arms_name()){
          stop('Arm ', arm$get_name(), ' already exists in the trial. ',
               'Do you want to update it instead?')
        }
        arm_names <- c(arm_names, arm$get_name())
      }

      for(arm in arm_list){
        private$arms[[arm$get_name()]] <- arm
      }

      names(sample_ratio) <- arm_names
      private$sample_ratio <- c(private$sample_ratio, sample_ratio)
      rm(sample_ratio)

      block_size <- sum(private$sample_ratio)

      ## update randomization plan for unenrolled patients
      private$permuted_block_randomization(block_size)

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
    get_sample_ratio = function(){
      private$sample_ratio
    },

    #' @description
    #' return number of patients when planning the trial
    get_number_patients = function(){
      private$n_patients
    },

    #' @description
    #' return number of enrolled (randomized) patients
    get_number_enrolled_patients = function(){
      private$n_enrolled_patients
    },

    #' @description
    #' return number of unenrolled patients
    get_number_unenrolled_patients = function(){
      private$n_patients - private$n_enrolled_patients
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
    #' @examples
    #' ## trial$get_enroll_time(1:3) ## first three
    #' ## trial$get_enroll_time(-1) ## all except the last one
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

      if(length(self$get_arms) == 0){
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
      stopifnot(nrow(private$trial_data) == private$n_enrolled_patients)

    },

    #' @description
    #' assign new patients to pre-planned randomization queue at pre-specified
    #' enrollment time.
    #' @param n_patients number of new patients to be enrolled. If \code{NULL},
    #' all remaining patients in plan are enrolled. Error may be triggered if
    #' n_patients is greater than remaining patients as planned.
    enroll_patients = function(n_patients = NULL){

      if(length(self$get_arms) == 0){
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
      arm_data <- list()
      arms_in_trial <- sort(unique(next_enroll_arms))
      for(i in seq_along(arms_in_trial)){
        arm <- arms_in_trial[i]
        patients_index <- which(next_enroll_arms %in% arm)
        n_patients_in_arm <- length(patients_index)
        arm_data[[arm]] <-
          data.frame(
            patient_id = self$get_number_enrolled_patients() + patients_index,
            arm = arm,
            enroll_time = next_enroll_time[patients_index]
          )
        for(ep in self$get_an_arm(arm)$get_endpoints()){
          arm_data[[arm]] <- cbind(arm_data[[arm]], ep$get_generator()(n_patients_in_arm))
        }
        patient_data <- rbind(patient_data, arm_data[[arm]])
      }


      for(arm in names(arm_data)){
        patient_data[which(next_enroll_arms %in% arm), ] <- arm_data[[arm]]
      }

      private$trial_data <- bind_rows(private$trial_data, patient_data)
      private$n_enrolled_patients <- private$n_enrolled_patients + n_patients
      stopifnot(nrow(private$trial_data) == private$n_enrolled_patients)

    }

  ),

  private = list(
    name = NULL,
    description = NULL,
    n_patients = NULL,
    n_enrolled_patients = NULL,
    sample_ratio = NULL,

    arms = list(),
    randomization_queue = NULL,
    enroller = NULL,
    enroll_time = NULL,

    trial_data = NULL,

    validate_arguments =
      function(name, n_patients, description, enroller, ...){

      stopifnot(is.character(name))
      stopifnot(is.character(description))

      stopifnot(is.numeric(n_patients) &&
                  (length(n_patients) == 1) &&
                  is.wholenumber(n_patients))

      stopifnot(is.function(enroller))

      # Check that the first argument of enroller is "n"
      arg_names <- names(formals(enroller))

      n_ <- 2
      enroller_ <- DynamicFunction(
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
        stopifnot(
          length(self$get_randomization_queue()) ==
            self$get_number_unenrolled_patients())
      }

      if(is.null(block_size)){
        block_size <- sum(private$sample_ratio)
      }

      if(self$get_number_unenrolled_patients() == 0){
        stop('All patients are enrolled. No further randomization is needed. ',
             'If you see this message, there is probably an unexpected issue with your code. ')
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
    }


  )
)
