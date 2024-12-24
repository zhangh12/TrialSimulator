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
#'   info_fraction = c(.5, .75, 1), planned_max_info = 387)
#'
#' ## without giving p-values, boundaries are returned without actual testing
#' gst$test_all()
#' gst
#'
#' ## Example 2. Calculate boundaries with observed information at stages
#'
#' ## get an error without resetting an used object
#' try( gst$test_all(observed_info = c(205, 285, 393)) )
#'
#' ## reset the object for re-use
#' gst$reset()
#' gst$test_all(observed_info = c(205, 285, 393))
#' gst
#'
#' ## Example 3. Test stagewise p-values sequentially
#' gst$reset()
#'
#' gst$test(p_value = .09, observed_info = 205)
#' gst$test(.006, 285)
#'
#' ## print testing trajectory by now
#' gst
#'
#' gst$test(.002, 393)
#'
#' ## print all testing trajectory
#' gst
#'
#' ## you can also test all stages at once
#' ## the result is the same as calling test() for each stage
#' gst$reset()
#' gst$test_all(c(.09, .006, .002), c(205, 285, 393))
#' gst
#'
#' ## Example 4. You don't have to update observed_info at all stages if
#' ## a trial runs as planned. For example, for the primary endpoint being
#' ## used to determine the stage (e.g., perform interim/final analysis when
#' ## number of events is reached), observed_info can be NULL. However,
#' ## observed_info is usually needed for other endpoints (e.g., secondary
#' ## endpoints)
#'
#' gst$reset()
#' gst$test(p_value = .09)
#' gst$test(0.006)
#' gst$test(0.002, observed_info = 393) # if final information is not as planned
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
    #' @param alpha_spending alpha spending function
    #' @param info_fraction a vector of numeric. Cumulative information fraction
    #' @param planned_max_info integer. Planned maximum number of patients for
    #' non-tte endpoints or number of events for tte endpoints
    #' @param name character. Name of the hypothesis, e.g. endpoint, subgroup,
    #' etc.
    initialize =
      function(alpha = .025,
               alpha_spending = c('asP', 'asOF', 'asKD', 'asHSD'),
               info_fraction,
               planned_max_info,
               name = 'H0'){

        sided <- 1
        stopifnot(is.numeric(alpha) && length(alpha) == 1 &&
                    alpha > 0 && alpha < 1)

        stopifnot(is.vector(info_fraction) && all(info_fraction >= 0))

        if(any(diff(info_fraction) <= 0)){
          stop('info_fraction should be monotonically increasing. ')
        }

        stopifnot(is.wholenumber(planned_max_info))
        stopifnot(length(name) == 1 && is.character(name))

        private$info_fraction <- info_fraction
        private$planned_max_info <- planned_max_info
        private$alpha <- alpha
        private$alpha_spending <- match.arg(alpha_spending)
        private$name <- name
        private$sided <- 1 # sided

        private$stage <- 1
        private$n_stages <- length(private$info_fraction)

        private$update_max_info <- FALSE

        private$original_info_fraction <- info_fraction
        private$original_planned_max_info <- planned_max_info
        private$original_alpha_spending <- match.arg(alpha_spending)

      },

    #' @description
    #' get current stage. \code{GroupSequentialTest} maintains a private field
    #' for tracking the current stage.
    get_stage = function(){
      private$stage
    },

    #' @description
    #' set alpha spending function. This is useful when set 'asUser' at the
    #' final stage to adjust for an under- or over-running trial.
    #' @param asf character of alpha spending function.
    set_alpha_spending = function(asf){
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
      message('Maximum information is updated at stage ', self$get_stage(),
              ' (', tmp, ' -> ', obs_max_info, '). ')
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
      private$planned_max_info <- private$original_planned_max_info
      private$info_fraction <- private$original_info_fraction
      self$set_alpha_spending(private$original_alpha_spending)
      private$stage <- 1
      private$update_max_info <- FALSE
      private$alpha_spent <- NULL
      private$trajectory <- NULL
      message('GroupSequentialTest object <', private$name,
              '> has been reset and is ready to use. ')
    },

    #' @description
    #' save testing result at current stage
    #' @param result a data frame storing testing result at a stage.
    set_trajectory = function(result){
      private$trajectory <- rbind(private$trajectory, result)
      if(nrow(private$trajectory) == private$n_stages){
        private$trajectory$informationRates <- private$info_fraction
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

      if(!private$update_max_info){ # not an over or under running trial
        design <- rpact::getDesignGroupSequential(
          sided = private$sided,
          alpha = private$alpha,
          informationRates = private$info_fraction,
          typeOfDesign = self$get_alpha_spending()
        )
      }else{
        stopifnot(self$get_alpha_spending() == 'asUser')
        design <- rpact::getDesignGroupSequential(
          sided = private$sided,
          alpha = private$alpha,
          informationRates = private$info_fraction,
          typeOfDesign = self$get_alpha_spending(),
          userAlphaSpending = private$alpha_spent
        )
      }

      ## will be used in next test if max_info is updated at the final stage
      ## in that case, alpha_spending will be set to 'asUser'
      private$alpha_spent <- design$alphaSpent

      design <- design %>%
        as.data.frame() %>%
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
    #' @param observed_info integer. Observed information at current stage. It
    #' can be the number of samples (non-tte) or number of events (tte) at test.
    #' If the current stage is final, observed_info will be used to update
    #' planned_max_info, the alpha spending function (\code{typeOfDesign}
    #' in \code{rpact}) will be updated to \code{'asUser'}, and the argument
    #' \code{userAlphaSpending} will be used when calling
    #' \code{rpact::getDesignGroupSequential}. It can be \code{NULL} if
    #' \code{info_fraction} and \code{planned_max_info} are not changed.
    test = function(p_value, observed_info = NULL){

      if(self$get_stage() > private$n_stages){
        stop('Group sequential test has been completed. \n',
             'No further test is available. \n',
             'Try GroupSequentialTest$reset() before restarting it. ')
      }

      stopifnot(is.null(observed_info) ||
                  (length(observed_info) == 1 &&
                     is.wholenumber(observed_info) &&
                     observed_info > 0))

      if(!is.null(observed_info)){
        if(self$get_stage() == private$n_stages){ # at final stage
          if(observed_info != self$get_max_info()){ # under or over running a trial
            private$info_fraction <- private$info_fraction * self$get_max_info() / observed_info
            self$set_max_info(observed_info)
            self$set_alpha_spending('asUser')
            private$update_max_info <- TRUE
          }
        }else{
          if(observed_info >= self$get_max_info()){
            stop('observed information <',
                 observed_info, ' is greater than planned_max_info <',
                 self$get_max_info(),
                 '>. This can only happened in the final stage. ')
          }
          private$update_max_info <- FALSE
        }

        message('Information fraction is updated at stage ', self$get_stage(),
                ' (', private$info_fraction[self$get_stage()], ' -> ',
                observed_info/self$get_max_info(), '). ')
        private$info_fraction[self$get_stage()] <- observed_info/self$get_max_info()
      }

      stage_level <- self$get_stage_level()
      private$stage <- private$stage + 1

      test_result <-
        attr(stage_level, 'details') %>%
        mutate(obs_p_value = p_value) %>%
        mutate(decision = ifelse(p_value < stage_level, 'reject', 'accept'))

      self$set_trajectory(test_result)
      test_result

    },

    #' @description
    #' similar to \code{GroupSequentialTest$test} if \code{p_value}
    #' is provided. If \code{p_value} is \code{NULL}, boundary of planned design
    #' will be returned with updated observed information \code{observed_info}.
    #' This function can be used to test p-values of all planned stages.
    #' If you want to test at current stage dynamically, use \code{test}.
    #' @param p_values a vector of p-values. If specified, its length should
    #' equal to the number of stages in the \code{GroupSequentialTest} object.
    #' @param observed_info a vector of integers. If specified, its length
    #' should equal to the number of stages in the \code{GroupSequentialTest}
    #' object.
    test_all = function(p_values = NULL, observed_info = NULL){

      if(!is.null(p_values)){
        if(length(p_values) != private$n_stages){
          stop('p_values should be of length ', private$n_stages, '. ')
        }
      }else{
        p_values <- rep(NA, private$n_stages)
      }

      if(!is.null(observed_info)){
        if(length(observed_info) != private$n_stages){
          stop('observed_info should be of length ', private$n_stages, '. ')
        }
      }else{
        observed_info <- rep(NA, private$n_stages)
      }

      for(i in seq_along(p_values)){
        if(is.na(observed_info[i])){
          oi <- NULL
        }else{
          oi <- observed_info[i]
        }

        self$test(p_value = p_values[i], observed_info = oi)
      }
    },

    #' @description
    #' generic function for \code{print}
    print = function(){
      print(self$get_trajectory())
    }


  ),

  private = list(
    sided = NULL,
    alpha = NULL,
    name = NULL,
    alpha_spending = NULL,
    info_fraction = NULL,
    planned_max_info = NULL,

    original_info_fraction = NULL,
    original_planned_max_info = NULL,
    original_alpha_spending = NULL,

    stage = NULL,
    n_stages = NULL,

    update_max_info = NULL,
    alpha_spent = NULL,
    trajectory = NULL
  )
)
