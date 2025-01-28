#' Class of GroupSequentialTest
#' @description
#' Perform group sequential test for a single endpoint based on sequential
#' one-sided p-values at each stages. Selected alpha spending functions,
#' including user-defined functions, are supported. Boundaries are calculated
#' with `rpact`. At the final analysis, adjustment can be applied for
#' over-running or under-running trial where observed final information is
#' greater or lower than the planned maximum information. See
#' Wassmer & Brannath, 2016, p78f. The test is based on p-values not z
#' statistics because it is easier to not handling direction of alternative
#' hypothesis in current implementation. In addition, one one-sided test is
#' supported which should be sufficient for common use in clinical design.
#'
#' @docType class
#' @importFrom rpact getDesignGroupSequential
#'
#' @examples
#' ## Note: examples showed here replicate the results from
#' ## https://www.rpact.org/vignettes/planning/rpact_boundary_update_example/
#'
#' ## Example 1. Generate boundaries for a pre-fix group sequential design
#' gst <- GroupSequentialTest$new(
#'   alpha = .025, alpha_spending = 'asOF',
#'   planned_max_info = 387)
#'
#' ## without giving p-values, boundaries are returned without actual testing
#' gst$test(observed_info = c(205, 285, 393), is_final = c(FALSE, FALSE, TRUE))
#' gst
#'
#' ## Example 2. Calculate boundaries with observed information at stages
#' ## No p-values are provided
#'
#' ## get an error without resetting an used object
#' try( gst$test(observed_info = 500, is_final = FALSE) )
#'
#' ## reset the object for re-use
#' gst$reset()
#' gst$test(observed_info = c(205, 285, 393), is_final = c(FALSE, FALSE, TRUE))
#' gst
#'
#' ## Example 3. Test stagewise p-values sequentially
#' gst$reset()
#'
#' gst$test(observed_info = 205, is_final = FALSE, p_values = .09)
#' gst$test(285, FALSE, .006)
#'
#' ## print testing trajectory by now
#' gst
#'
#' gst$test(393, TRUE, 002)
#'
#' ## print all testing trajectory
#' gst
#'
#' ## you can also test all stages at once
#' ## the result is the same as calling test() for each of the stages
#' gst$reset()
#' gst$test(c(205, 285, 393), c(FALSE, FALSE, TRUE), c(.09, .006, .002))
#' gst
#'
#' ## Example 4. use user-define alpha spending
#' gst <- GroupSequentialTest$new(
#'   alpha = .025, alpha_spending = 'asUser',
#'   planned_max_info = 387)
#'
#' gst$test(
#'   observed_info = c(205, 285, 393),
#'   is_final = c(FALSE, FALSE, TRUE),
#'   alpha_spent = c(.005, .0125, .025))
#' gst
#'
#' @export
#'
GroupSequentialTest <- R6::R6Class(
  'Group Sequential Test',

  public = list(

    #' @description
    #' initialize a group sequential test. Now only support one-sided test
    #' based on p-values.
    #' @param alpha familywise error rate
    #' @param alpha_spending alpha spending function. Use \code{"asUser"}
    #' if custom alpha spending schedule is used.
    #' @param planned_max_info integer. Planned maximum number of patients for
    #' non-tte endpoints or number of events for tte endpoints
    #' @param name character. Name of the hypothesis, e.g. endpoint, subgroup,
    #' etc. Optional.
    #' @param silent \code{TRUE} if muting all messages.
    initialize =
      function(alpha = .025,
               alpha_spending = c('asP', 'asOF', 'asUser'),
               planned_max_info,
               name = 'H0',
               silent = TRUE){

        sided <- 1
        stopifnot(is.numeric(alpha) && length(alpha) == 1 &&
                    alpha > 0 && alpha < 1)

        stopifnot(is.wholenumber(planned_max_info))
        stopifnot(length(name) == 1 && is.character(name))

        stopifnot(is.logical(silent) && length(silent) == 1)
        private$silent <- silent
        private$update_max_info <- FALSE
        private$info_fraction <- NULL
        private$observed_info <- NULL
        private$complete <- FALSE
        private$stage <- 1
        private$info_fraction <- NULL
        private$planned_max_info <- planned_max_info
        private$alpha <- alpha
        private$alpha_spending <- match.arg(alpha_spending)
        private$alpha_spent <- 1e-6
        private$always_asUser <- ifelse(private$alpha_spending == 'asUser', TRUE, FALSE)
        private$name <- name
        private$sided <- 1 # sided
        private$original_planned_max_info <- planned_max_info
        private$original_alpha_spending <- match.arg(alpha_spending)

        asf <- c(asOF = "O'Brien & Fleming",
                 asP = 'Pocock',
                 asUser = 'custom')

        if(!private$silent){
          message('A group sequential design with overall alpha = <',
                  private$alpha, '> and alpha spending function <',
                  asf[private$alpha_spending],
                  '> is initialized for the hypothesis <', private$name, '>. \n',
                  'Call $test() to compute boundaries or test the hypothesis. \n',
                  ifelse(private$alpha_spending %in% 'asUser',
                         'You need to specifiy alpha_spent to use "asUser". ',
                         "You don't need to specify alpha_spent. ")
                  )
        }

      },

    #' @description
    #' get name of hypothesis
    get_name = function(){
      private$name
    },

    #' @description
    #' get overall alpha
    get_alpha = function(){
      private$alpha
    },

    #' @description
    #' set alpha spending function. This is useful when set 'asUser' at the
    #' final stage to adjust for an under- or over-running trial.
    #' @param asf character of alpha spending function.
    set_alpha_spending = function(asf){
      stopifnot(asf %in% c('asP', 'asOF', 'asUser'))
      private$alpha_spending <- asf
    },

    #' @description
    #' return character of alpha spending function
    get_alpha_spending = function(){
      private$alpha_spending
    },

    #' @description
    #' return planned maximum information
    get_max_info = function(){
      private$planned_max_info
    },

    #' @description
    #' set planned maximum information. This is used at the final stage
    #' to adjust for an under- or over-running trial.
    #' @param obs_max_info integer. Maximum information, which could be
    #' observed number of patients or events at the final stage.
    set_max_info = function(obs_max_info){
      tmp <- private$planned_max_info
      private$planned_max_info <- obs_max_info
      if(!private$silent){
        message('Maximum information is updated at stage ', self$get_stage(),
                ' (', tmp, ' -> ', obs_max_info, '). ')
      }
    },

    #' @description
    #' get current stage.
    get_stage = function(){
      private$stage
    },

    #' @description
    #' an object of class \code{GroupSequentialTest} is designed to be used
    #' sequentially by calling \code{GroupSequentialTest$test}. When all
    #' planned tests are performed, no further analysis could be done. In that
    #' case keep calling \code{GroupSequentialTest$test} will trigger an error.
    #' To reuse the object for a new set of staged p-values, call this function
    #' to reset the status to stage 1. See examples. This implementation can
    #' prevent the error that more than the planned number of stages are tested.
    reset = function(){
      private$info_fraction <- NULL
      private$observed_info <- NULL
      private$complete <- FALSE
      private$planned_max_info <- private$original_planned_max_info
      private$info_fraction <- NULL
      self$set_alpha_spending(private$original_alpha_spending)
      private$always_asUser <- ifelse(private$original_alpha_spending == 'asUser', TRUE, FALSE)
      private$stage <- 1
      private$alpha_spent <- 1e-6
      private$trajectory <- NULL
      message('GroupSequentialTest object <', private$name,
              '> has been reset and is ready to use. ')
    },

    #' @description
    #' save testing result at current stage
    #' @param result a data frame storing testing result at a stage.
    #' @param is_final logical. \code{TRUE} if final test for the hypothesis,
    #' \code{FALSE} otherwise.
    set_trajectory = function(result, is_final = FALSE){
      private$trajectory <- dplyr::bind_rows(private$trajectory, result)
      if(is_final){
        ## information fraction is finalized only at the final stage
        private$trajectory$informationRates <- private$info_fraction[-1]
      }
    },

    #' @description
    #' return testing trajectory until current stage. This function can be
    #' called at any stage. See examples.
    get_trajectory = function(){
      private$trajectory
    },

    #' @description
    #' compute boundaries given current (potentially updated) settings. It
    #' returns different values if settings are changed over time.
    get_stage_level = function(){

      if(!private$always_asUser && self$get_alpha_spending() == 'asUser'){
        private$alpha_spent <- c(private$alpha_spent, self$get_alpha())
      }

      suppressWarnings(
        design <- rpact::getDesignGroupSequential(
          sided = private$sided,
          alpha = private$alpha,
          informationRates = private$info_fraction,
          typeOfDesign = self$get_alpha_spending(),
          userAlphaSpending = private$alpha_spent
        )
      )

      if(private$update_max_info){
        self$set_alpha_spending(private$original_alpha_spending)
      }

      ## will be used in next test if max_info is updated at the final stage
      ## in that case, alpha_spending will be set to 'asUser'
      private$alpha_spent <- design$alphaSpent

      design <- design %>%
        as.data.frame() %>%
        mutate(stages = row_number() - 1) %>%
        dplyr::filter(stages %in% self$get_stage())

      level <- design$stageLevels[1]
      attr(level, 'details') <-
        design %>%
        dplyr::select(typeOfDesign, stages, informationRates, alpha,
                      sided, alphaSpent, criticalValues, stageLevels) %>%
        as.data.frame()
      level

    },

    #' @description
    #' test a hypothesis with the given p-value at current stage
    #' @param p_value numeric. A p-value.
    #' @param is_final logical. \code{TRUE} if this test is carried out for
    #' the final analysis.
    #' @param observed_info integer. Observed information at current stage. It
    #' can be the number of samples (non-tte) or number of events (tte) at test.
    #' If the current stage is final, observed_info will be used to update
    #' planned_max_info, the alpha spending function (\code{typeOfDesign}
    #' in \code{rpact}) will be updated to \code{'asUser'}, and the argument
    #' \code{userAlphaSpending} will be used when calling
    #' \code{rpact::getDesignGroupSequential}.
    #' @param alpha_spent numeric if \code{alpha_spending = "asUser"}. It must
    #' be between 0 and \code{alpha}, the overall alpha of the test.
    #' \code{NA_real_} for other alpha spending functions \code{"asOF"} and
    #' \code{"asP"}.
    test_one = function(p_value, is_final, observed_info, alpha_spent = NA_real_){

      if(missing(p_value)){
        p_value <- NA

        if(!private$silent){
          message('p_value is missing so a dummy test is performed by setting it to NA. ')
        }
      }

      private$observed_info <- c(private$observed_info, observed_info)

      if(is_final){ # at final stage
        if(observed_info != self$get_max_info()){ # under or over running a trial
          self$set_max_info(observed_info)
          self$set_alpha_spending('asUser')
          private$update_max_info <- TRUE
        }
      }else{
        if(observed_info >= self$get_max_info()){
          stop('observed information <',
               observed_info, '> is greater than planned_max_info <',
               self$get_max_info(),
               '>. This can only happened in the final stage. ')
        }
        private$update_max_info <- FALSE
      }

      ## set first information fraction to be 1e-6 for easy implementation
      private$info_fraction <- c(1e-6, private$observed_info / self$get_max_info())
      private$alpha_spent <- sort(unique(c(private$alpha_spent, alpha_spent)))

      stage_level <- self$get_stage_level()
      private$stage <- private$stage + 1

      test_result <-
        attr(stage_level, 'details') %>%
        mutate(obs_p_value = p_value) %>%
        mutate(decision = ifelse(p_value < stage_level, 'reject', 'accept')) %>%
        mutate(hypothesis = private$name)

      self$set_trajectory(test_result, is_final)

      if(is_final){
        private$complete <- TRUE
      }

      test_result

    },

    #' @description
    #' Carry out test based on group sequential design. If \code{p_values}
    #' is \code{NULL}, dummy values will be use and boundaries are calculated
    #' for users to review.
    #' @param observed_info a vector of integers, observed information at stages.
    #' @param is_final logical vector. \code{TRUE} if the test is for the final
    #' analysis.
    #' @param p_values a vector of p-values. If specified, its length should
    #' equal to the length of \code{observed_info}.
    #' @param alpha_spent accumulative alpha spent at observed information.
    #' It is a numeric vector of values between 0 and 1, and of length that
    #' equals \code{length(observed_info)} if alpha-spending
    #' function is not \code{"asUser"}. Otherwise \code{NULL}.
    test = function(observed_info, is_final, p_values = NULL, alpha_spent = NULL){

      if(private$complete){
        stop('Group sequential test has been completed. \n',
             'No further test is available. \n',
             'Run GroupSequentialTest$reset() and try again. ')
      }

      if(any(is.na(observed_info))){
        stop('No NA is allowed in observed_info. ')
      }

      if(!all(is.wholenumber(observed_info)) || any(observed_info <= 0)){
        stop('observed_info should be a vector of positive integers. ')
      }

      if(any(is.na(is_final))){
        stop('No NA is allowed in logical vector is_final. ')
      }

      stopifnot(is.logical(is_final))
      if(length(observed_info) != length(is_final)){
        stop('observered_info and is_final must of same length. ')
      }

      if(!is.null(p_values)){
        if(length(p_values) != length(observed_info)){
          stop('p_values and observed_info should be of same length. ')
        }
      }else{
        p_values <- rep(NA, length(observed_info))
        if(!private$silent){
          message('No p-values are provided. Only decision boundaries are calculated. ')
        }
      }

      if(private$always_asUser){
        if(is.null(alpha_spent)){
          stop('alpha_spent cannot be NULL as custom alpha spending function is in use. ')
        }

        if(length(alpha_spent) != length(observed_info)){
          browser()
          stop('alpha_spent and observed_info should be of same length. ')
        }

        if(any(alpha_spent <= 0 | alpha_spent > self$get_alpha())){
          stop('alpha_spent should be of values between 0 and ', self$get_alpha())
        }

        if(length(alpha_spent) > 1 && any(diff(alpha_spent) <= 0)){
          stop('alpha_spent should be monotonically increasing. ')
        }

        for(i in seq_along(alpha_spent)){
          if(is_final[i] && abs(alpha_spent[i] - self$get_alpha()) > 1e-6){
            stop('At final test, the accumulated alpha_spent should be ',
                 self$get_alpha())
          }
        }

      }else{ ## asOF, asP
        if(!is.null(alpha_spent)){
          stop('alpha_spent should not be specified as alpha spending function is <',
               self$get_alpha_spending(), '>. ')
        }

        alpha_spent <- rep(NA_real_, length(observed_info))
      }



      for(i in seq_along(p_values)){
        self$test_one(p_value = p_values[i],
                      is_final = is_final[i],
                      observed_info = observed_info[i],
                      alpha_spent = alpha_spent[i])
      }
    },

    #' @description
    #' generic function for \code{print}
    print = function(){
      print(self$get_trajectory())
    }


  ),

  private = list(
    sided = 1, ## always one-sided for now
    alpha = NULL, ## overall FWER
    name = 'H0', ## name of hypothesis
    alpha_spending = NULL, ## 'asOF', 'asP', 'asUser'
    planned_max_info = NULL, ## integer to be updated at final stage

    silent = TRUE,

    always_asUser = NULL,
    observed_info = NULL,
    info_fraction = NULL,
    update_max_info = FALSE,

    original_planned_max_info = NULL,
    original_alpha_spending = NULL,

    stage = 1, ## integer, current stage, change over time

    alpha_spent = NULL,
    trajectory = NULL,
    complete = FALSE
  )
)
