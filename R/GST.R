#' Class of GST
#' @description
#' Create a class of group sequential test for a single endpoint.
#'
#' @docType class
#' @export
GST <- R6::R6Class(
  'Group Sequential Test',

  public = list(

    initialize =
      function(alpha = .025,
               alpha_spending = c('asP', 'asOF', 'asKD', 'asHSD'),
               info_fraction,
               planned_max_info,
               sided = 1,
               name = 'H0'){

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

    get_stage = function(){
      private$stage
    },

    set_alpha_spending = function(asf){
      private$alpha_spending <- asf
    },

    get_alpha_spending = function(){
      private$alpha_spending
    },

    get_max_info = function(){
      private$planned_max_info
    },

    set_max_info = function(obs_max_info){
      tmp <- private$planned_max_info
      private$planned_max_info <- obs_max_info
      message('Maximum information is updated at stage ', self$get_stage(),
              ' (', tmp, ' -> ', obs_max_info, '). ')
    },

    reset = function(){
      private$planned_max_info <- private$original_planned_max_info
      private$info_fraction <- private$original_info_fraction
      self$set_alpha_spending(private$original_alpha_spending)
      private$stage <- 1
      private$update_max_info <- FALSE
      private$alpha_spent <- NULL
      private$trajectory <- NULL
      message('GST object <', private$name, '> has been reset and is ready to use. ')
    },

    set_trajectory = function(result){
      private$trajectory <- rbind(private$trajectory, result)
      if(nrow(private$trajectory) == private$n_stages){
        private$trajectory$informationRates <- private$info_fraction
      }
    },

    get_trajectory = function(){
      private$trajectory
    },

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
    test_at_stage = function(p_value,
                             observed_info = NULL){

      if(self$get_stage() > private$n_stages){
        stop('Group sequential test has been completed. ',
             'No further test is available. ',
             'Try GST$reset() before restarting it. ')
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
    #' similar to \code{GST$test_at_stage} if \code{p_value} is provided.
    #' If \code{p_value} is \code{NULL}, boundary of planned design will be
    #' returned with updated observed information \code{observed_info}.
    #' @param p_values a vector of p-values. If specified, its length should
    #' equal to the number of stages in the \code{GST} object.
    #' @param observed_info a vector of integers. If specified, its length
    #' should equal to the number of stages in the \code{GST} object.
    test = function(p_values = NULL, observed_info = NULL){

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

        self$test_at_stage(p_value = p_values[i], observed_info = oi)
      }
    },

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
