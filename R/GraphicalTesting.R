#' Class of GraphicalTesting
#' @description
#' Perform graphical testing under group sequential design for one or multiple
#' endpoints. See Maurer & Bretz (2013).
#'
#' @docType class
#' @importFrom gMCPLite hGraph
#'
#' @examples
#'
#' ## Example 1
#' ## dry-run to study the behavior of a graph
#' ## without group sequential design
#' if(FALSE){
#' eps <- .01
#' alpha <- c(.01, .04, 0, 0, 0)
#' transition <- matrix(c(
#'   0, 0, 0, 0, 1,
#'   0, 0, .75, 0, .25,
#'   0, 1/2-eps/2, 0, eps, 1/2-eps/2,
#'   0, 0, 0, 0, 0,
#'   0, 1/2, 1/2, 0, 0
#' ), nrow = 5, byrow = TRUE)
#'
#' ## dummy can be anything, we don't actually use it
#' asf <- rep('asOF', 5)
#' ## dummy can be anything, we don't actually use it
#' max_info <- c(300, 1100, 1100, 1100, 500)
#'
#' hs <- c('H1: UPCR IgA', 'H2: eGFR GN', 'H3: eGFR GN 10wk', 'H5: 2nd Endpoints', 'H4: eGFR IgA')
#'
#' ## initialize an object
#' gt <- GraphicalTesting$new(alpha, transition, asf, max_info, hs)
#' print(gt)
#'
#' ## reject hypotheses based on customized order
#' ## to understand the behavior of a testing strategy
#' ## Any other rejection order is possible
#' gt$reject_a_hypothesis('H1: UPCR IgA')
#' print(gt)
#'
#' gt$reject_a_hypothesis('H2: eGFR GN')
#' print(gt)
#'
#' gt$reject_a_hypothesis('H4: eGFR IgA')
#' print(gt)
#'
#' gt$reject_a_hypothesis('H3: eGFR GN 10wk')
#' print(gt)
#'
#' gt$reset()
#' }
#'
#' ## Example 2
#' if(FALSE){
#' ## Example modified from vignettes in gMCPLite:
#' ## Graphical testing for group sequential design
#' ## initial alpha split to each of the hypotheses
#' alpha <- c(.01, .01, .004, .0, .0005, .0005)
#'
#' ## transition matrix of the initial graph
#' transition <- matrix(c(
#'   0, 1, 0, 0, 0, 0,
#'   0, 0, .5, .5, 0, 0,
#'   0, 0, 0, 1, 0, 0,
#'   0, 0, 0, 0, .5, .5,
#'   0, 0, 0, 0, 0, 1,
#'   .5, .5, 0, 0, 0, 0
#' ), nrow = 6, byrow = TRUE)
#'
#' ## alpha spending functions per hypothesis
#' asf <- c('asUser', 'asOF', 'asUser', 'asOF', 'asOF', 'asOF')
#'
#' ## planned maximum number of events per hypothesis
#' max_info <- c(295, 800, 310, 750, 500, 1100)
#'
#' ## name of hypotheses
#' hs <- c('H1: OS sub',
#'         'H2: OS all',
#'         'H3: PFS sub',
#'         'H4: PFS all',
#'         'H5: ORR sub',
#'         'H6: ORR all')
#'
#' gt <- GraphicalTesting$new(alpha, transition, asf, max_info, hs)
#'
#' ## print initial graph
#' gt
#'
#' ## nominal p-values at each stage
#' ## Note: p-values with same order are calculated with the same locked data
#' ## Note: alpha_spent is only specified for hypotheses using custom alpha
#' ##       spending function "asUser"
#' stats <-
#'   data.frame(
#'     order = c(1:3, 1:3, 1:2, 1:2, 1, 1),
#'     hypotheses = c(rep('H1: OS sub',3), rep('H2: OS all',3),
#'     'H5: ORR sub', 'H6: ORR all'),
#'     p = c(.03, .0001, .000001, .2, .15, .1, .2, .001, .3, .2, .00001, .1),
#'     info = c(185, 245, 295, 529, 700, 800, 265, 310, 675, 750, 490, 990),
#'     is_final = c(F, F, T, F, F, T, F, T, F, T, T, T),
#'     max_info = c(rep(295, 3), rep(800, 3), rep(310, 2), rep(750, 2), 490, 990),
#'     alpha_spent = c(c(.1, .4, 1), rep(NA, 3), c(.3, 1), rep(NA, 2), NA, NA)
#'   )
#'
#' ## test the p-values from the first analysis, plot the updated graph
#' gt$test(stats %>% dplyr::filter(order==1))
#'
#' ## test the p-values from the second analysis, plot the updated graph
#' gt$test(stats %>% dplyr::filter(order==2))
#'
#' ## test the p-values from the third analysis, plot the updated graph
#' ## because no futher test would be done, displayed results are final
#' gt$test(stats %>% dplyr::filter(order==3))
#'
#'
#' ## plot the final status of the graph
#' print(gt, trajectory = FALSE)
#'
#' ## you can get final testing results as follow
#' gt$get_current_testing_results()
#'
#' ## if you want to see step-by-step details
#' if(FALSE){
#'   print(gt$get_trajectory())
#' }
#'
#' ## equivalently, you can call gt$test(stats) for only once to get same results.
#' gt$reset()
#' gt$test(stats)
#'
#' ## if you only want to get the final testing results
#' gt$get_current_decision()
#' }
#'
#' @export
#'
GraphicalTesting <- R6::R6Class(
  'GraphicalTestingProcedure',

  public = list(

    #' @description
        #' Initialize an object for graphical testing procedure.
        #' Group sequential design is also supported.
        #' @param alpha initial alpha allocated to each of the hypotheses.
        #' @param transition matrix of transition weights. Its diagonals should
        #' be all 0s. The row sums should be 1s (for better power) or
        #' 0s (if no outbound edge from a node).
        #' @param alpha_spending character vector of same length of \code{alpha}.
        #' Currently it supports \code{'asP'}, \code{'asOF'}, and \code{'asUser'}.
        #' @param planned_max_info vector of integers. Maximum numbers of
        #' events (tte endpoints) or patients (non-tte endpoints) at the final
        #' analysis of each hypothesis when planning a trial. The actual numbers
        #' could be different, which can be specified elsewhere.
        #' @param hypotheses vector of characters. Names of hypotheses.
        #' @param silent \code{TRUE} if muting all messages and not generating
        #' plots.
    initialize = function(alpha,
                          transition,
                          alpha_spending,
                          planned_max_info,
                          hypotheses = NULL,
                          silent = FALSE){

      private$validate_arguments(alpha,
                                 transition,
                                 alpha_spending,
                                 planned_max_info,
                                 hypotheses)

      stopifnot(is.logical(silent))
      private$silent <- silent

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
            is_final = NULL,
            p = NULL,
            planned_max_info = planned_max_info[i],
            name = private$hypotheses[i],
            alpha_spent = NULL,
            critical_values = NULL
          )
      }

      if(!private$silent){
        message('A graph is initialized for ', length(private$hypotheses),
                ' hypotheses at FWER = ', sum(private$alpha), '. ')
      }

      private$trajectory <- NULL

      private$original_silent <- private$silent
      private$original_hypotheses <- private$hypotheses
      private$original_hids_in_graph <- private$hids_in_graph
      private$original_transition <- private$transition
      private$original_alpha <- private$alpha
      private$original_gst <- private$gst
      private$original_trajectory <- private$trajectory

    },

    #' @description
    #' reset an object of class \code{GraphicalTesting} to original status
    #' so that it can be reused.
    reset = function(){
      private$silent <- private$original_silent
      private$hypotheses <- private$original_hypotheses
      private$hids_in_graph <- private$original_hids_in_graph
      private$transition <- private$original_transition
      private$alpha <- private$original_alpha
      private$gst <- private$original_gst

      private$trajectory <- private$original_trajectory

      message('GraphicalTesting object has been reset and is ready to use. ')
    },

    #' @description
    #' determine if index of a hypothesis is valid
    #' @param hid an integer
    is_valid_hid = function(hid){
      stopifnot(hid > 0 && hid <= nrow(private$transition))
    },

    #' @description
    #' get name of a hypothesis given its index.
    #' @param hid an integer
    get_hypothesis_name = function(hid){
      self$is_valid_hid(hid)
      private$hypotheses[hid]
    },

    #' @description
    #' return weight between two nodes
    #' @param hid1 an integer
    #' @param hid2 an integer
    get_weight = function(hid1, hid2){
      self$is_valid_hid(hid1)
      self$is_valid_hid(hid2)
      private$transition[hid1, hid2]
    },

    #' @description
    #' update weight between two nodes
    #' @param hid1 an integer
    #' @param hid2 an integer
    #' @param value numeric value to be set as a weight two nodes
    set_weight = function(hid1, hid2, value){
      self$is_valid_hid(hid1)
      self$is_valid_hid(hid2)
      if(hid1 == hid2){
        stopifnot(value == 0)
      }

      if(value < 0 && value > -1e-6){
        value <- .0
        if(!private$silent){
          warning('A slightly less-than-zero weight value is reset to 0. ')
        }
      }

      if(value > 1 && value - 1 < 1e-6){
        if(!private$silent){
          warning('A slightly greater-than-1 weight value is reset to 1. ')
        }
        value <- 1.0
      }

      stopifnot(value >= 0 && value <= 1)
      private$transition[hid1, hid2] <- value
    },

    #' @description
        #' return alpha allocated to a hypothesis when calling this function.
        #' Note that a function can be called several time with the graph is
        #' updated dynamically. Thus, returned alpha can be different even for
        #' the same \code{hid}.
        #' @param hid an integer
    get_alpha = function(hid){
      self$is_valid_hid(hid)
      private$alpha[hid]
    },

    #' @description
    #' update alpha of a hypothesis
    #' @param hid integer. Index of a hypothesis
    #' @param value numeric value to be allocated
    set_alpha = function(hid, value){
      self$is_valid_hid(hid)
      stopifnot(value >= 0 && value <= 1)
      private$alpha[hid] <- value
    },


    #' @description
    #' return all valid \code{hid}
    get_hypotheses_ids = function(){
      1:nrow(private$transition)
    },

    #' @description
        #' return number of hypotheses, including those been rejected.
    get_number_hypotheses = function(){
      nrow(private$transition)
    },

    #' @description
        #' return index of hypotheses that are rejected.
    get_hids_not_in_graph = function(){
      setdiff(self$get_hypotheses_ids(), private$hids_in_graph)
    },

    #' @description
        #' return index of hypotheses with non-zero alphas, thus can be tested
        #' at the current stage.
    get_testable_hypotheses = function(){
      which(private$alpha > 0)
    },

    #' @description
        #' determine whether at least one hypothesis is testable.
        #' If return \code{FALSE}, the testing procedure is completed.
    has_testable_hypotheses = function(){
      length(self$get_testable_hypotheses()) > 0
    },

    #' @description
    #' determine whether a hypothesis is not yet rejected (in graph).
    #' @param hid integer. Index of a hypothesis
    is_in_graph = function(hid){
      hid %in% private$hids_in_graph
    },

    #' @description
    #' determine whether a hypothesis has a non-zero alpha allocated.
    #' @param hid integer. Index of a hypothesis
    is_testable = function(hid){
      hid %in% self$get_testable_hypotheses()
    },

    #' @description
    #' convert hypothesis's name into (unique) index.
    #' @param hypothesis character. Name of a hypothesis. It is different from
    #' \code{hid}, which is an index.
    get_hid = function(hypothesis){
      stopifnot(hypothesis %in% private$hypotheses)
      hid <- which(private$hypotheses %in% hypothesis)
      stopifnot(length(hid) == 1 && !is.na(hid))
      hid
    },

    #' @description
    #' remove a node from graph when a hypothesis is rejected
    #' @param hypothesis name of a hypothesis. It is different from
    #' \code{hid}, which is an index.
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
        if(alp < 1e-5){
          alp <- 1e-5 ## numeric rounding error existing when computing integral for spent alpha. try to avoid too small allocated alpha
        }
        stopifnot(alp >= self$get_alpha(l))
        if(alp > self$get_alpha(l)){
          if(!private$silent && TRUE){
            message('alpha of hypothesis <', self$get_hypothesis_name(l),
                    '> is updated (',
                    signif(self$get_alpha(l), 2), ' -> ', signif(alp, 2), '). ')
          }
        }
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

      if(!private$silent){
        message('Hypothesis <', hypothesis, '> is rejected. ')
      }

    },

    #' @description
    #' save new testing results at current stage
    #' @param result a data frame of specific columns.
    set_trajectory = function(result){
      private$trajectory <- rbind(private$trajectory, result)
    },

    #' @description
        #' return testing results by the time this function is called.
        #' Note that graphical test is carried out in a sequential manner.
        #' Users may want to review the results anytime. Value returned
        #' by this function can possibly vary over time.
    get_trajectory = function(){
      private$trajectory
    },

    #' @description
        #' test hypotheses using p-values (and other information in \code{stats})
        #' base on the current graph. All rows should have the same order
        #' number.
        #' @param stats a data frame. It must contain the following columns:
        #' \describe{
        #' \item{\code{order}}{integer. P-values (among others) of hypotheses that
        #' can be tested at the same time (e.g., an interim, or final analysis)
        #' should be labeled with the same order number.
        #' If a hypothesis is not tested at a stage,
        #' simply don't put it in \code{stats} with that order number.}
        #' \item{\code{hypotheses}}{character. Name of hypotheses to be tested. They
        #' should be identical to those when calling \code{GraphicalTesting$new}.}
        #' \item{\code{p}}{nominal p-values.}
        #' \item{\code{info}}{observed number of events or samples at test. These will
        #' be used to compute information fractions in group sequential design.}
        #' \item{\code{max_info}}{integers. Maximum information at test. At interim,
        #' \code{max_info} should be equal to \code{planned_max_info} when
        #' calling \code{GraphicalTesting$new}. At the final stage of a
        #' hypothesis, one can update it with observed numbers.}
        #' }
    test_hypotheses = function(stats){

      if(nrow(stats) == 0){
        message('No hypothesis is given to be tested. ')
        return(invisible(NULL))
      }

      if(length(unique(stats$order)) != 1){
        stop('test_hypotheses expect p-values of hypotheses of the same analysis. ',
             'Debug it. ')
      }

      ## for a given set of hypotheses to be tested together, once a hypothesis
      ## is rejected, the remaining hypotheses should be tested again with
      ## updated graph
      tested <- rep(FALSE, length(stats$hypotheses))
      names(tested) <- stats$hypotheses
      test_again <- TRUE
      while(test_again){
        test_again <- FALSE

        ## TRUE if at least one hypothesis has alpha > 0
        at_least_one_testable <- FALSE
        for(h in stats$hypotheses){

          hid <- self$get_hid(h)

          stat <- stats %>% dplyr::filter(hypotheses == self$get_hypothesis_name(hid))
          stopifnot(private$gst[[hid]]$name == stat$hypotheses)

          if(!is.na(stat$max_info)){
            if(stat$max_info != private$gst[[hid]]$planned_max_info){
              if(!private$silent){
                message('Planned maximum information for hypothesis <',
                        private$gst[[hid]]$name,
                        '> is changed from <',
                        private$gst[[hid]]$planned_max_info,
                        '> -> <', stat$max_info, '>. ')
              }
            }
            private$gst[[hid]]$planned_max_info <- stat$max_info
          }

          stopifnot(is.null(private$gst[[hid]]$info) || stat$info >= max(private$gst[[hid]]$info))

          if(!tested[h]){
            tested[h] <- TRUE
            private$gst[[hid]]$info <- c(private$gst[[hid]]$info, stat$info)
            private$gst[[hid]]$is_final <- c(private$gst[[hid]]$is_final, stat$is_final)
            private$gst[[hid]]$p <- c(private$gst[[hid]]$p, stat$p)

            if(private$gst[[hid]]$alpha_spending == 'asUser'){
              if(is.na(stat$alpha_spent)){
                stop('alpha_spent cannot be NA as alpha spending function is <asUser> ',
                     'for hypothesis <', private$gst[[hid]]$name, '>. ')
              }

              if(stat$alpha_spent <=0 || stat$alpha_spent > 1){
                stop('alpha_spent for hypothesis <', private$gst[[hid]]$name,
                     '> is out of range (should be within (0, 1]). ')
              }

              if(stat$is_final && 1 - stat$alpha_spent > 1e-4){
                stop('At final stage, alpha_spent for hypothesis <',
                     private$gst[[hid]]$name,
                     '> should be 1. ')
              }

              if(!is.null(private$gst[[hid]]$alpha_spent) &&
                 stat$alpha_spent <= max(private$gst[[hid]]$alpha_spent)){
                stop('At final stage, alpha_spent for hypothesis <',
                     private$gst[[hid]]$name,
                     '> should be increasing. ')
              }
              private$gst[[hid]]$alpha_spent <- c(private$gst[[hid]]$alpha_spent, stat$alpha_spent)
            }
          }

          stopifnot(private$gst[[hid]]$planned_max_info >= max(private$gst[[hid]]$info))

          current_stage <- length(private$gst[[hid]]$info)

          args <- private$gst[[hid]]

          if(self$get_alpha(hid) == 0){
            if(length(private$gst[[hid]]$critical_values) < length(private$gst[[hid]]$info)){
              private$gst[[hid]]$critical_values <- c(private$gst[[hid]]$critical_values, Inf)
              stopifnot(length(private$gst[[hid]]$critical_values) == length(private$gst[[hid]]$info))
            }
            if(!private$silent){
              message('<', args$name, ', order = ', stat$order,
                      '> cannot be tested with alpha = 0. ')
            }
            next
          }else{
            at_least_one_testable <- TRUE
          }

          if(length(args$is_final) == 1 && tail(args$is_final, 1)){
            ## this hypothesis can only be tested once, no final adjustment is needed
            gst <- GroupSequentialTest$new(alpha = self$get_alpha(hid),
                                           alpha_spending = args$alpha_spending,
                                           planned_max_info = args$planned_max_info,
                                           name = args$name,
                                           silent = private$silent)

            gst$test(observed_info = args$info,
                     is_final = args$is_final,
                     p_values = args$p)
          }else{
            if(length(args$is_final) > 1 && tail(args$is_final, 1)){
              ## this hypothesis can be tested more than once
              ## and it is the final test
              ## change alpha_spending to be 'asUser' to make adjustment
              gst <- GroupSequentialTest$new(alpha = self$get_alpha(hid),
                                             alpha_spending = 'asUser',
                                             planned_max_info = args$planned_max_info,
                                             name = args$name)

              info_frac_ <- args$info/args$planned_max_info

              ##################################################################
              ## a very tricky bug was fixed here
              ## Note that this function test all hypotheses that have their
              ## p-values ready for being tested at the same time (stage)
              ## A hypothesis A can possibly be tested for more than once.
              ## This could happen when it was accepted first, and then
              ## another hypothesis, say B, was rejected with its its alpha
              ## being passed to A. Now A will be tested again, which can
              ## trigger this bug. Fixed. An example to trigger the bug is
              ##   order hypotheses            p info is_final max_info
              ##    1    pfs low 1.000000e+00  188    FALSE      352
              ##    2    pfs low 1.000000e+00  352     TRUE      352
              ##    1   pfs high 1.045526e-03  188    FALSE      352
              ##    2   pfs high 5.721944e-06  352     TRUE      352
              ##    2     os low 1.000000e+00  352    FALSE      430
              ##    3     os low 1.000000e+00  430     TRUE      430
              ##    2    os high 3.211445e-04  352    FALSE      430
              ##    3    os high 1.142131e-05  430     TRUE      430
              ## where the graphical testing object is defined as
              ## alpha <- c(.01/2, .01/2, .015/2, .015/2)
              ## transition <- matrix(1/3, nrow = 4, ncol = 4)
              ## diag(transition) <- 0
              ## asf <- rep('asOF', 4)
              ## max_info <- c(352, 352, 430, 430)
              ## hs <- c('pfs low', 'pfs high', 'os low', 'os high')
              ## gt <- GraphicalTesting$new(alpha, transition, asf, max_info, hs)
              ## gt$test(graph_stats) ## this line will trigger issues

              ## add this assert to prevent issue like this
              stopifnot(length(info_frac_) - length(args$critical_values) <= 1)

              ## add this assert to prevent issue like this
              if(length(info_frac_) == length(args$critical_values)){
                stopifnot(tested[h])
              }

              alpha_spent_ <-
                computeCumulativeAlphaSpent(
                  args$critical_values[1:(length(info_frac_) - 1)], ## this line fixes the bug
                  info_frac_[-length(info_frac_)])

              ##################################################################

              alpha_spent_[alpha_spent_ < min(1e-5, gst$get_alpha())] <- min(1e-5, gst$get_alpha()/2)

              gst$test(observed_info = args$info,
                       is_final = args$is_final,
                       p_values = args$p,
                       alpha_spent = c(alpha_spent_, gst$get_alpha()))
            }else{
              ## this is not the final test
              ## alpha_spending should not be changed,
              ## no adjustment is needed
              gst <- GroupSequentialTest$new(alpha = self$get_alpha(hid),
                                             alpha_spending = args$alpha_spending,
                                             planned_max_info = args$planned_max_info,
                                             name = args$name)

              if(args$alpha_spending %in% 'asUser'){
                ## asUser, so custom alpha_spent * allocated alpha is used
                gst$test(observed_info = args$info,
                         is_final = args$is_final,
                         p_values = args$p,
                         alpha_spent = args$alpha_spent * self$get_alpha(hid))
              }else{
                ## asOF or asP, alpha_spent should not be used
                gst$test(observed_info = args$info,
                         is_final = args$is_final,
                         p_values = args$p)
              }
            }
          }

          gst <- gst$get_trajectory() %>%
            dplyr::filter(stages == current_stage)

          if(length(private$gst[[hid]]$critical_values) == length(private$gst[[hid]]$info)){
            k_ <- length(private$gst[[hid]]$critical_values)
            private$gst[[hid]]$critical_values[k_] <- gst$criticalValues
          }else{
            private$gst[[hid]]$critical_values <- c(private$gst[[hid]]$critical_values, gst$criticalValues)
            stopifnot(length(private$gst[[hid]]$critical_values) == length(private$gst[[hid]]$info))
          }


          decision_ <- ifelse(stat$p < gst$stageLevels, 'reject', 'accept')

          if(!private$silent){
            message('<', args$name, ', order = ', stat$order,
                    '> is ', decision_,
                    'ed (obs = ', signif(stat$p, 2),
                    ', level = ', signif(gst$stageLevels, 2), '). ')
          }

          if(decision_ == 'reject'){
            self$reject_a_hypothesis(self$get_hypothesis_name(hid))

            self$print(graph = !private$silent, trajectory = FALSE)
          }

          gst <- gst %>%
            mutate(obs_p_value = stat$p) %>%
            mutate(decision = decision_) %>%
            mutate(order = stat$order)

          self$set_trajectory(gst)
          private$gst[[hid]]$info_fraction <- NULL

          if(decision_ == 'reject'){
            test_again <- TRUE
            break
          }

        }

        if(!at_least_one_testable){
          stop('None of the hypotheses has non-zero alpha at this stage. ',
               'Check your initial alpha split and transition matrix. ')
        }

        if(!test_again){
          if(!private$silent){
            message('No further hypothesis can be rejected (order = ',
                    stats$order[1], '). ')
          }
        }
      }

    },

    #' @description
    #' test hypotheses using p-values (and other information in \code{stats})
    #' base on the current graph. Users can call this function multiple times.
    #' P-values of the same order should be passed through \code{stats}
    #' together. P-values of multiple orders can be passed together as well.
    #' For example, if users only have p-values at current stage, they can call
    #' this function and update the graph accordingly. In this case, column
    #' \code{order} in \code{stats} is a constant. They can call this
    #' function again when p-values of next stage is available, where
    #' \code{order} is another integer. In simulation, if p-values of all
    #' stages are on hand, users can call this function to
    #' test them all in a single pass. In this case, column \code{order} in
    #' \code{stats} can have different values.
    #' @param stats a data frame. It must contain the following columns:
    #' \describe{
    #' \item{\code{order}}{integer. P-values (among others) of hypotheses that
    #' can be tested at the same time (e.g., an interim, or final analysis)
    #' should be labeled with the same order number.
    #' If a hypothesis is not tested at a stage,
    #' simply don't put it in \code{stats} with that order number.
    #' If all p-values in \code{stats} are tested at the same stage, \code{order}
    #' can be absent.}
    #' \item{\code{hypotheses}}{character. Name of hypotheses to be tested. They
    #' should be identical to those when calling
    #' \code{GraphicalTesting$new}.}
    #' \item{\code{p}}{nominal p-values.}
    #' \item{\code{info}}{observed number of events or samples at test. These will
    #' be used to compute information fractions in group sequential design.}
    #' \item{\code{max_info}}{integers. Maximum information at test. At interim,
    #' \code{max_info} should be equal to \code{planned_max_info} when
    #' calling \code{GraphicalTesting$new}. At the final stage of a
    #' hypothesis, one can update it with observed numbers.}
    #' \item{\code{alpha_spent}}{accumulative proportion of allocated alpha
    #' to be spent if \code{alpha_spending = "asUser"}. Set it to
    #' \code{NA_real_} otherwise. If no hypothesis uses \code{"asUser"} in
    #' \code{stats}, this column could be ignored. }
    #' }
    #'
    #' @return a data frame returned by \code{get_current_testing_results}.
    #' It contains details of each of the testing steps.
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

      if(!all(c('hypotheses', 'p', 'info', 'is_final', 'max_info') %in% names(stats))){
        stop('Columns <',
             paste0(setdiff(c('hypotheses', 'p', 'info', 'is_final', 'max_info'),
                            names(stats)),
                    collapse = ', '),
             '> are missing in stats. ')
      }

      if(is.null(stats$alpha_spent)){
        stats$alpha_spent <- NA_real_
      }

      if(!all(is.wholenumber(stats$order))){
        stop('Column <order> in stats should be integers. ')
      }

      self$print(graph = !private$silent, trajectory = FALSE)

      for(ord in sort(unique(stats$order))){
        ## hypotheses to be tested together
        stats_ <- stats %>% dplyr::filter(order == ord)

        self$test_hypotheses(stats_)

      }

      self$get_current_testing_results()

    },

    #' @description
    #' return testing results with details by the time this function
    #' is called. This function can be called by users by multiple
    #' times, thus the returned value varies over time.
    #' This function is called by \code{GraphicalTesting::test},
    #' and returns a data frame consisting of columns
    #' \describe{
    #' \item{\code{hypothesis}}{name of hypotheses.}
    #' \item{\code{obs_p_value}}{observed p-values.}
    #' \item{\code{max_allocated_alpha}}{maximum allocated alpha for the hypothesis.}
    #' \item{\code{decision}}{\code{'reject'} or \code{'accept'} the hypotheses.}
    #' \item{\code{stages}}{stage of a hypothesis. }
    #' \item{\code{order}}{order number that this hypothesis is tested for the last time.
    #' It is different from \code{stages}.}
    #' \item{\code{typeOfDesign}}{name of alpha spending functions.}
    #' }
    get_current_testing_results = function(){

      if(is.null(self$get_trajectory())){
        return(NULL)
      }

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

    #' @description
    #' get current decisions for all hypotheses. Returned value could
    #' changes over time because it depends on the stages being tested already.
    #' @return a named vector of values \code{"accept"} or \code{"reject"}.
    #' Note that if a hypothesis is not yet tested when calling this function,
    #' the decision for that hypothesis would be \code{"accept"}.
    get_current_decision = function(){

      hypotheses <- private$hypotheses

      current <- self$get_current_testing_results()

      ret <- rep('accept', length(hypotheses))
      names(ret) <- hypotheses

      if(is.null(current)){
        return(ret)
      }

      stopifnot(all(current$hypothesis %in% hypotheses))

      for(h in current$hypothesis){
        ret[h] <- current$decision[current$hypothesis %in% h]
      }

      ret

    },

    #' @description
    #' generic function for \code{print}
    #' @param graph logic. \code{TRUE} if visualizing the current graph,
    #' which can vary over time.
    #' @param trajectory logic. \code{TRUE} if print the current data frame of
    #' trajectory, which can vary over time.
    #' @param ... other arguments supported in \code{gMCPLite::hGraph},
    #' e.g., \code{trhw} and \code{trhh} to control the size of transition box,
    #' and \code{trdigits} to control the digits displayed for transition
    #' weights.
    print = function(graph = TRUE, trajectory = TRUE, ...){

      gplot <-
        gMCPLite::hGraph(nHypotheses = self$get_number_hypotheses(),
                         nameHypotheses = private$hypotheses,
                         alphaHypotheses = private$alpha,
                         m = private$transition,
                         palette = '#56B4E9',
                         trhw = .15,
                         trhh = .1125,
                         trdigits = 3, ...)
      if(graph){
        print(gplot)
      }

      if(trajectory){
        # View(self$get_trajectory())
        print(self$get_trajectory())
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

    silent = NULL,

    original_silent = NULL,
    original_hypotheses = NULL,
    original_hids_in_graph = NULL,
    original_transition = NULL,
    original_alpha = NULL,
    original_gst = NULL,
    original_trajectory = NULL,

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

      stopifnot(all(abs(rowSums(transition) - 1)< 1e-6 |
                      abs(rowSums(transition) - 0)< 1e-6))

      stopifnot(all(alpha_spending %in% c('asP', 'asOF', 'asUser')))
      stopifnot(is.vector(alpha_spending) && is.character(alpha_spending))
      stopifnot(length(alpha_spending) == nrow(transition))

      stopifnot(all(is.wholenumber(planned_max_info)))
      stopifnot(all(planned_max_info > 0))
      stopifnot(length(planned_max_info) == nrow(transition))

    }
  )

)
