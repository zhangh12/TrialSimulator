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
                          hypotheses = NULL){

      private$validate_arguments(alpha, transition, hypotheses)

      if(is.null(hypotheses)){
        hypotheses <- paste0('H', 1:length(alpha))
      }
      private$hypotheses <- hypotheses
      private$hids_in_graph <- 1:length(hypotheses)
      private$transition <- transition
      private$alpha <- alpha

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

    is_in_graph = function(hid){
      hid %in% private$hids_in_graph
    },

    is_testable = function(hid){
      hid %in% self$get_testable_hypotheses()
    },

    #' @description
    #' remove a node from graph when a hypothesis is rejected
    #' @param hid id of a hypothesis
    reject_a_hypothese = function(hid){

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

    },

    print = function(){

      gplot <-
        gMCPLite::hGraph(nHypotheses = self$get_number_hypotheses(),
                         nameHypotheses = private$hypotheses,
                         alphaHypotheses = private$alpha,
                         m = private$transition)
      print(gplot)
    }

  ),

  private = list(
    hypotheses = NULL,
    transition = NULL,
    alpha = NULL,

    hids_in_graph = NULL,

    validate_arguments = function(alpha, transition, hypotheses){

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
      stopifnot(all(rowSums(transition) == 1))

    }
  )

)
