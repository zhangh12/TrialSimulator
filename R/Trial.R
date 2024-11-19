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
#' ## Plan a trial, Trial-3415, of up to 100 patients.
#' ## Enrollment time follows an exponential distribution, with median 5
#' trial <- Trial$new(
#'   name = 'Trial-3415', n_patients = 100,
#'   enroller = rexp, rate = log(2) / 5)
#' trial$add_arms(sample_ratio = c(1, 2), placebo, active)
#' trial
#' table(trial$get_randomization_queue())
#' trial$get_enroll_time()[1:4]
#' trial$enroll_a_patient()
#' trial$enroll_a_patient()
#' trial$get_enroll_time()[1:2]
#' table(trial$get_randomization_queue())

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

        private$name <- name
        private$description <- description
        private$n_patients <- n_patients
        private$n_enrolled_patients <- 0

        private$enroller <- DynamicFunction(enroller, ...)

        ## sort enrollment time
        private$enroll_time <-
          sort(private$enroller(n_patients), decreasing = FALSE)

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
    get_randomization_queue = function(){
      private$randomization_queue
    },

    #' @description
    #' return enrollment time of planned but not yet enrolled patients.
    #' This function does not update enroll_time, just return its value
    #' for debugging purpose.
    get_enroll_time = function(){
      private$enroll_time
    },

    #' @description
    #' assign a new patient to an arm based on planned randomization queue
    enroll_a_patient = function(){

      if(length(self$get_arms) == 0){
        stop('No arm is added in the trial yet. Patient cannot be enrolled. ')
      }

      if(self$get_number_unenrolled_patients() == 0){
        stop('Maximum planned sample size has been reached. Patient cannot be enrolled. ')
      }

      next_enroll_arm <- private$randomization_queue[1]
      ## update randomization_queue after enrolling a new patient.
      ## randomization_queue only keep randomization queue for future patients
      private$randomization_queue <- private$randomization_queue[-1]

      next_enroll_time <- private$enroll_time[1]
      private$enroll_time <- private$enroll_time[-1]

      private$n_enrolled_patients <- private$n_enrolled_patients + 1

      list(
        next_enroll_arm = next_enroll_arm,
        next_enroll_time = next_enroll_time
      )

    },

    #' @description
    #' get number of arms in the trial
    get_number_arms = function(){
      length(private$arms)
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

    ## add_arms() can only be called once, then arms_is_added is set to TRUE which
    ## prevents calling add_arms() again. As a result, all arms should be added
    ## into the trial together. sample_ratio should also be specified accordingly.
    ## This makes sure that Trial can link each arm to the correct sample ratio.
    ## If arms need to be added or removed while the trial is running
    ## (e.g., arm selection, futility, interim, etc.), use update_arms() instead
    ## which allows updating sample_ratio as well.
    arms_is_added = FALSE,

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

      n_ <- 10
      enroller_ <- DynamicFunction(
        enroller, rng = deparse(substitute(enroller)), ...)
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

      if(!is.null(private$randomization_queue)){
        stopifnot(
          length(private$randomization_queue) ==
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
