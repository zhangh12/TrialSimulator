#' Class of GraphicalTesting
#' @description
#' Perform graphical testing under group sequential design for one or multiple
#' endpoints. See Maurer & Bretz (2013).
#'
#' @docType class
#' @importFrom gMCPLite hGraph
#'
#' @examples
#' ## examples
#' @export
#'
GraphicalTesting <- R6::R6Class(
  'Graphical Testing Procedure',

  public = list(

    initialize = function(alpha,
                          transition,
                          alpha_spending,
                          planned_max_info,
                          hypotheses = NULL){

      private$validate_arguments(alpha,
                                 transition,
                                 alpha_spending,
                                 planned_max_info,
                                 hypotheses)

      if(is.null(hypotheses)){
        hypotheses <- paste0('H', 1:length(alpha))
      }
      private$hypotheses <- hypotheses
      private$hids_in_graph <- 1:length(hypotheses)
      private$transition <- transition
      private$alpha <- alpha

      private$gst <- list()
      for(i in seq_along(private$hypotheses)){
        private$gst[[i]] <-
          list(
            alpha = private$alpha[i],
            alpha_spending = alpha_spending[i],
            info = NULL,
            planned_max_info = planned_max_info[i],
            name = private$hypotheses[i]
          )
      }

      message('A graph is initialized for ', length(private$hypotheses),
              ' hypotheses at FWER = ', sum(private$alpha), '. ')

    },

    is_valid_hid = function(hid){
      stopifnot(hid > 0 && hid <= nrow(private$transition))
    },

    get_hypothesis_name = function(hid){
      self$is_valid_hid(hid)
      private$hypotheses[hid]
    },

    get_weight = function(hid1, hid2){
      self$is_valid_hid(hid1)
      self$is_valid_hid(hid2)
      private$transition[hid1, hid2]
    },

    set_weight = function(hid1, hid2, value){
      self$is_valid_hid(hid1)
      self$is_valid_hid(hid2)
      if(hid1 == hid2){
        stopifnot(value == 0)
      }

      if(value < 0 && value > -1e-6){
        value <- .0
        warning('A slightly less-than-zero weight value is reset to 0. ')
      }

      if(value > 1 && value - 1 < 1e-6){
        warning('A slightly greater-than-1 weight value is reset to 1. ')
        value <- 1.0
      }

      stopifnot(value >= 0 && value <= 1)
      private$transition[hid1, hid2] <- value
    },

    get_alpha = function(hid){
      self$is_valid_hid(hid)
      private$alpha[hid]
    },

    set_alpha = function(hid, value){
      self$is_valid_hid(hid)
      stopifnot(value >= 0 && value <= 1)
      private$alpha[hid] <- value
    },

    get_hypotheses_ids = function(){
      1:nrow(private$transition)
    },

    get_number_hypotheses = function(){
      nrow(private$transition)
    },

    get_hids_not_in_graph = function(){
      setdiff(self$get_hypotheses_ids(), private$hids_in_graph)
    },

    get_testable_hypotheses = function(){
      which(private$alpha > 0)
    },

    has_testable_hypotheses = function(){
      length(self$get_testable_hypotheses() > 0)
    },

    is_in_graph = function(hid){
      hid %in% private$hids_in_graph
    },

    is_testable = function(hid){
      hid %in% self$get_testable_hypotheses()
    },

    get_hid = function(hypothesis){
      stopifnot(hypothesis %in% private$hypotheses)
      hid <- which(private$hypotheses %in% hypothesis)
      stopifnot(length(hid) == 1 && !is.na(hid))
      hid
    },

    #' @description
    #' remove a node from graph when a hypothesis is rejected
    #' @param hid id of a hypothesis
    reject_a_hypothesis = function(hypothesis){

      hid <- self$get_hid(hypothesis)

      self$is_valid_hid(hid)
      if(!self$is_in_graph(hid)){
        stop('Hypothesis ', self$get_hypothesis_name(hid),
             ' is not in the graph or already been rejected. ')
      }

      if(!self$is_testable(hid)){
        stop('Hypothesis ', self$get_hypothesis_name(hid),
             ' cannot be rejected because no alpha is allocated to it. ')
      }

      private$hids_in_graph <- setdiff(private$hids_in_graph, hid)

      for(l in private$hids_in_graph){
        alp <- self$get_alpha(l) + self$get_alpha(hid) * self$get_weight(hid, l)
        self$set_alpha(l, alp)
      }

      for(l in self$get_hids_not_in_graph()){
        self$set_alpha(l, .0)
      }

      self$set_alpha(hid, .0) # not necessary

      to_be_zero <- list()
      for(l in self$get_hypotheses_ids()){
        for(m in self$get_hypotheses_ids()){

          if(self$is_in_graph(l) && self$is_in_graph(m) && (l != m) &&
             self$get_weight(l, hid) * self$get_weight(hid, l) < 1){
            wt <- (self$get_weight(l, m) +
                     self$get_weight(l, hid) * self$get_weight(hid, m)) /
              (1 - self$get_weight(l, hid) * self$get_weight(hid, l))

            self$set_weight(l, m, wt)
          }else{
            to_be_zero <- append(to_be_zero, list(list(l = l, m = m)))
            next
          }
        }
      }

      for(lst in to_be_zero){
        self$set_weight(lst$l, lst$m, .0)
      }

      message('Hypothesis <', hypothesis, '> is rejected. ')

    },

    set_trajectory = function(result){
      private$trajectory <- rbind(private$trajectory, result)
    },

    get_trajectory = function(){
      private$trajectory
    },

    test_hypotheses = function(stats){

      if(nrow(stats) == 0){
        message('No hypothesis is given to be tested. ')
        return(invisible(NULL))
      }

      if(length(unique(stats$order)) != 1){
        stop('test_hypotheses expect p-values of hypotheses of the same analysis. ',
             'Debug it. ')
      }

      try_again <- TRUE
      while(try_again){
        try_again <- FALSE

        for(h in stats$hypotheses){

          hid <- self$get_hid(h)

          stat <- stats %>% dplyr::filter(hypotheses == self$get_hypothesis_name(hid))
          stopifnot(private$gst[[hid]]$name == stat$hypotheses)

          if(!is.na(stat$max_info)){
            private$gst[[hid]]$planned_max_info <- stat$max_info
          }

          stopifnot(is.null(private$gst[[hid]]$info) || stat$info >= max(private$gst[[hid]]$info))
          private$gst[[hid]]$info <- sort(unique(c(private$gst[[hid]]$info, stat$info)))
          stopifnot(private$gst[[hid]]$planned_max_info >= max(private$gst[[hid]]$info))

          private$gst[[hid]]$info_fraction <-
            private$gst[[hid]]$info / private$gst[[hid]]$planned_max_info

          if(!(1 %in% private$gst[[hid]]$info_fraction)){
            private$gst[[hid]]$info_fraction <- c(private$gst[[hid]]$info_fraction, 1.)
          }
          current_stage <- length(private$gst[[hid]]$info)

          args <- private$gst[[hid]]

          if(self$get_alpha(hid) == 0){
            private$gst[[hid]]$info_fraction <- NULL
            message('<', args$name, ', order = ', stat$order,
                    '> cannot be tested with alpha = 0. ')
            next
          }

          gst <- GroupSequentialTest$new(alpha = self$get_alpha(hid),
                                         alpha_spending = args$alpha_spending,
                                         info_fraction = args$info_fraction,
                                         planned_max_info = args$planned_max_info,
                                         name = args$name)

          gst$test_all()

          gst <- gst$get_trajectory() %>%
            dplyr::filter(stages == current_stage)

          decision_ <- ifelse(stat$p < gst$stageLevels, 'reject', 'accept')

          message('<', args$name, ', order = ', stat$order,
                  '> is ', decision_,
                  'ed (obs = ', signif(stat$p, 2),
                  ', level = ', signif(gst$stageLevels, 2), '). ')

          if(decision_ == 'reject'){
            self$reject_a_hypothesis(self$get_hypothesis_name(hid))

            self$print(trajectory = FALSE)
          }

          gst <- gst %>%
            mutate(obs_p_value = stat$p) %>%
            mutate(decision = decision_) %>%
            mutate(order = stat$order)

          self$set_trajectory(gst)
          private$gst[[hid]]$info_fraction <- NULL

          if(decision_ == 'reject'){
            try_again <- TRUE
            break
          }

        }

        if(!try_again){
          message('No further hypothesis can be rejected (order = ',
                  stats$order[1], '). ')
        }
      }

    },

    test = function(stats){

      if(!self$has_testable_hypotheses()){
        message('All hypotheses in graph have been tested and completed. ')
        return(invisible(NULL))
      }

      if(!is.data.frame(stats)){
        stop('stats of test should be a data frame. ')
      }

      if(is.null(stats$order)){
        stats$order <- 0
      }

      if(!all(c('hypotheses', 'p', 'info', 'max_info') %in% names(stats))){
        stop('Columns <',
             paste0(setdiff(c('hypotheses', 'p', 'info', 'max_info'), names(stats)),
                    collapse = ', '),
             '> are missing in stats. ')
      }

      if(!all(is.wholenumber(stats$order))){
        stop('Column <order> in stats should be integers. ')
      }

      self$print(trajectory = FALSE)

      for(ord in sort(unique(stats$order))){
        ## hypotheses to be tested together
        stats_ <- stats %>% dplyr::filter(order == ord)

        self$test_hypotheses(stats_)

      }

      self$get_current_testing_results()

    },

    get_current_testing_results = function(){

      current_results <- self$get_trajectory() %>%
        group_split(hypothesis) %>%
        lapply(
          function(h){
            max_allocated_alpha <- max(h$alpha)
            if('reject' %in% h$decision){
              h <- h %>%
                dplyr::filter(decision %in% 'reject') %>%
                distinct()
              if(nrow(h) > 1){
                stop('A hypothesis has been rejected more than once. ',
                     'Debug it. ')
              }
            }else{
              h <- h %>%
                dplyr::filter(order %in% max(order, na.rm = TRUE)) %>%
                distinct() %>%
                arrange(desc(alpha)) %>%
                head(1)
            }
            h$max_allocated_alpha <- max_allocated_alpha
            return(h)
          }
        ) %>%
        do.call(rbind, .) %>%
        dplyr::select(hypothesis,
                      obs_p_value,
                      max_allocated_alpha,
                      decision,
                      stages,
                      order,
                      typeOfDesign) %>%
        arrange(order, stages, desc(max_allocated_alpha), obs_p_value) %>%
        as.data.frame()


      current_results
    },

    print = function(graph = TRUE, trajectory = TRUE){

      gplot <-
        gMCPLite::hGraph(nHypotheses = self$get_number_hypotheses(),
                         nameHypotheses = private$hypotheses,
                         alphaHypotheses = private$alpha,
                         m = private$transition,
                         palette = '#56B4E9')
      if(graph){
        print(gplot)
      }

      if(trajectory){
        View(self$get_trajectory())
      }

    }

  ),

  private = list(
    hypotheses = NULL,
    transition = NULL,
    alpha = NULL,

    hids_in_graph = NULL,
    gst = NULL, ## arguments for defining a group sequential test object,
    trajectory = NULL,

    validate_arguments = function(alpha,
                                  transition,
                                  alpha_spending,
                                  planned_max_info,
                                  hypotheses){

      stopifnot(is.matrix(transition))
      stopifnot(is.vector(alpha) && is.numeric(alpha))
      stopifnot(nrow(transition) == ncol(transition))
      if(!is.null(hypotheses)){
        stopifnot(nrow(transition) == length(hypotheses))
      }
      stopifnot(nrow(transition) == length(alpha))

      if(any(is.na(transition))){
        stop('NA is not allowed in transition matrix. ')
      }

      if(any(is.na(alpha))){
        stop('NA is not allowed in alpha of hypotheses. ')
      }
      stopifnot(all(transition >= 0))
      stopifnot(all(transition <= 1))
      stopifnot(all(diag(transition) == 0))

      stopifnot(all(alpha >= 0))
      stopifnot(sum(alpha) <= 1)
      stopifnot(all(abs(rowSums(transition) - 1)< 1e-6))

      stopifnot(all(alpha_spending %in% c('asP', 'asOF', 'asKD', 'asHSD')))
      stopifnot(is.vector(alpha_spending) && is.character(alpha_spending))
      stopifnot(length(alpha_spending) == nrow(transition))

      stopifnot(all(is.wholenumber(planned_max_info)))
      stopifnot(all(planned_max_info > 0))
      stopifnot(length(planned_max_info) == nrow(transition))

    }
  )

)
