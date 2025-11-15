#' Class of Controller
#' @description
#' Create a class of controller to run a trial.
#'
#' Public methods in this R6 class are used in developing
#' this package. Thus, we have to export the whole R6 class which exposures all
#' public methods. However, only the public methods in the list below are
#' useful to end users.
#'
#' \itemize{
#' \item \code{$run()}
#' \item \code{$get_output()}
#' \item \code{$reset()}
#' }
#'
#' @docType class
#' @examples
#' ##
#' @export
Controllers <- R6::R6Class(
  'Controllers',

  private = list(
    trial = NULL,
    listener = NULL,
    silent = FALSE,
    dry_run = FALSE,
    output = NULL,

    run_ = function(plot_event = TRUE, silent = FALSE, dry_run = FALSE){

      private$silent <- silent
      private$dry_run <- dry_run
      self$mute()

      self$get_listener()$monitor(self$get_trial(), private$dry_run)
      if(plot_event){
        self$get_trial()$event_plot()
      }

    },

    run_sequential_ = function(n, plot_event, silent, dry_run){

      for(idx in 1:n){
        tryCatch(
          expr = {
            private$run_(plot_event, silent, dry_run)
          },

          error = function(e){
            self$get_trial()$save(e$message, 'error_message', overwrite = TRUE)
            private$output <- bind_rows(private$output, self$get_trial()$get_output())
            stop(e$message)
          }
        )

        private$output <- bind_rows(private$output, self$get_trial()$get_output())

        if(idx < n){
          self$reset()
        }
      }

    },

    run_parallel_ = function(n, n_workers, silent, dry_run){

      if(!requireNamespace("mirai", quietly = TRUE)){
        stop('Package "mirai" is required for parallel execution (n_workers > 1). ',
             'Install it with install.packages("mirai").')
      }

      ## Serialize trial and listener once; workers will deserialize
      ## independent copies. make_arms_snapshot() has already been called
      ## in run(), so the snapshot is baked into the serialized bytes.
      trial_raw <- serialize(self$get_trial()$clone(deep = TRUE), NULL)
      listener_raw <- serialize(self$get_listener()$clone(deep = TRUE), NULL)

      ## Distribute replicates evenly across workers
      reps_per_worker <- rep(floor(n / n_workers), n_workers)
      remainder <- n %% n_workers
      if(remainder > 0){
        reps_per_worker[1:remainder] <- reps_per_worker[1:remainder] + 1L
      }

      with(
        mirai::daemons(n_workers),
        {
          ## Submit all tasks
          tasks <- vector("list", n_workers)
          for(i in seq_len(n_workers)){
            tasks[[i]] <- mirai::mirai(
              {
                library(TrialSimulator)

                trial <- unserialize(trial_raw)
                listener <- unserialize(listener_raw)

                output <- NULL
                error_msg <- NULL

                ## R uses Mersenne-Twister streams by default to generate
                ## random numbers, which theoretically is not safe for parallel,
                ## but the chance of getting wrong is low given the extremely
                ## long period it has (2^19937 - 1).
                ## The packages parallel and mirai use L'Ecuyer-CMRG streams.
                ## The reason why I use Mersenne-Twister:
                ## 1.
                RNGkind('default')
                for(j in seq_len(reps)){
                  trial$reset()
                  listener$reset()

                  trial$mute(silent)
                  listener$mute(silent)

                  run_ok <- tryCatch(
                    {
                      listener$monitor(trial, dry_run)
                      TRUE
                    },
                    error = function(e){
                      trial$save(e$message, 'error_message', overwrite = TRUE)
                      error_msg <<- e$message
                      FALSE
                    }
                  )

                  output <- dplyr::bind_rows(output, trial$get_output())

                  if(!run_ok) break
                }

                list(output = output, error = error_msg)
              },
              trial_raw = trial_raw,
              listener_raw = listener_raw,
              #worker_seed = worker_seeds[i],
              reps = reps_per_worker[i],
              silent = silent,
              dry_run = dry_run
            )
          }

          ## Collect results; fail-fast on first error
          for(i in seq_len(n_workers)){
            result <- mirai::call_mirai(tasks[[i]])$data

            ## Unexpected worker-level failure (e.g. package not found,
            ## serialization issue)
            if(mirai::is_error_value(result)){
              stop('Worker ', i, ' failed unexpectedly: ', result)
            }

            private$output <- bind_rows(private$output, result$output)

            ## Expected simulation error (from stop() in user action functions)
            if(!is.null(result$error)){
              stop(result$error)
            }
          }
        }
      )

      RNGkind('default')

    }

  ),

  public = list(

    #' @description
    #' initialize a controller of the trial
    #' @param trial a trial object returned from \code{trial()}.
    #' @param listener a listener object returned from \code{listener()}.
    initialize = function(trial, listener){
      stopifnot(inherits(trial, 'Trials'))
      stopifnot(inherits(listener, 'Listeners'))
      private$trial <- trial
      private$listener <- listener
      private$silent <- FALSE
      private$dry_run <- FALSE
    },

    #' @description
    #' return listener in a controller.
    get_listener = function(){
      private$listener
    },

    #' @description
    #' return trial in a controller.
    get_trial = function(){
      private$trial
    },

    #' @description
    #' mute all messages (not including warnings).
    #' @param silent logical.
    mute = function(){
      self$get_trial()$mute(private$silent)
      self$get_listener()$mute(private$silent)
    },

    #' @description
    #' reset the trial and listener registered to the controller before running
    #' additional replicate of simulation. This is usually done between two
    #' calls of \code{controller$run()}.
    #'
    reset = function(){
      self$get_trial()$reset()
      self$get_listener()$reset()
    },

    #' @description
    #' return a data frame of all current outputs saved by calling \code{save()}.
    #' @param cols character vector. Columns to be returned from the data frame
    #' of simulation outputs. If \code{NULL}, all columns are returned.
    #' @param simplify logical. Return vector rather than a data frame of one
    #' column when \code{length(cols) == 1} and \code{simplify == TRUE}.
    #' @param tidy logical. \code{TrialSimulator} automatically records a set
    #' of standard outputs at milestones, even when \code{doNothing} is used
    #' as action functions. These includes time of triggering milestones,
    #' number of observed events for time-to-event endpoints, and number of
    #' non-missing readouts for non-TTE endpoints
    #' (see \code{vignette('actionFunctions')}). This usually mean a large
    #' number of columns in outputs. If users have no intent to summarize a
    #' trial on these columns, setting \code{tidy = TRUE} can eliminate these
    #' columns from \code{get_output()}. This is useful to reduced the size of
    #' output data frame when a large number of replicates are done for
    #' simulation. Note that currently we use regex
    #' \code{"^n_events_<.*?>_<.*?>$"} and
    #' \code{"^milestone_time_<.*?>$"} to match columns to be eliminated.
    #' If users plan to use \code{tidy = TRUE}, caution is needed when naming
    #' custom outputs in \code{save()}. Default \code{FALSE}.
    get_output = function(cols = NULL, simplify = TRUE, tidy = FALSE){
      if(is.null(cols)){
        cols <- colnames(private$output)
      }

      if(!all(cols %in% names(private$output))){
        stop('Columns <', paste0(setdiff(cols, names(private$output)), collapse = ', '),
             '> are not found in trial$output. Check if there is a typo. ')
      }

      ret <- private$output[, cols, drop = FALSE]

      if(tidy){
        ret <- ret %>%
          select(!matches("^n_events_<.*?>_<.*?>$")) %>%
          select(!matches("^milestone_time_<.*?>$"))
      }

      if(simplify && ncol(ret) == 1){
        return(ret[, 1])
      }else{
        return(ret)
      }
    },

    #' @description
    #' run trial simulation.
    #' @param n integer. Number of replicates of simulation.
    #' \code{n = 1} by default. Simulation results can be accessed by
    #' \code{controller$get_output()}.
    #' @param n_workers integer. Number of parallel workers. When
    #' \code{n_workers = 1} (default), replicates are run sequentially.
    #' When \code{n_workers > 1}, replicates are distributed across
    #' parallel workers using the \code{mirai} package, which must be
    #' installed separately. Each worker receives a serialized copy of
    #' the trial and listener objects and runs its share of replicates
    #' independently. If any replicate encounters an error, execution
    #' stops and already-collected results are preserved in
    #' \code{$get_output()}. To debug, manually set \code{seed} in
    #' \code{trial()} and \code{n_workers = 1} in \code{run()} for reproduced
    #' results. Note that optimal \code{n_workers} may not be
    #' \code{parallel::detectCores()}. For example, Macbook with M1/M2/M3 chips
    #' may have performance cores and efficiency cores. To achieve the best
    #' parallel performance, one may want to use the performance cores only.
    #' For a M1 laptop with 4 performance cores, \code{n_workers = 3} may give
    #' the best performance.
    #' @param plot_event logical. Create event plot if \code{TRUE}. Users
    #' should set it to be \code{FALSE} if \code{n > 1}. Forced to
    #' \code{FALSE} when \code{n_workers > 1}.
    #' @param silent logical. \code{TRUE} if muting all messages during a
    #' trial. Note that warning messages are still displayed.
    #' @param dry_run logical. We are considering retire this argument.
    #' \code{TRUE} if action function provided by users is
    #' ignored and an internal default action \code{.default_action} is called
    #' instead. This default function only locks data when the milestone is
    #' triggered. Milestone time and number of endpoints' events or sample sizes
    #' are saved. It is suggested to set \code{dry_run = TRUE} to estimate
    #' distributions of triggering time and number of events before formally
    #' using custom action functions if a fixed design is in use.
    #' This helps determining planned maximum
    #' information for group sequential design and reasonable time of milestone
    #' of interest when planning a trial. Set it to \code{FALSE} for formal
    #' simulations. However, for an adaptive design where arm(s) could
    #' possibly be added or removed, setting \code{dry_run} to \code{TRUE}
    #' is usually not helpful because adaption should be executed
    #' before estimating the milestone time.
    run = function(n = 1, n_workers = 1, plot_event = TRUE, silent = FALSE, dry_run = FALSE){

      self$get_trial()$make_arms_snapshot()
      private$output <- NULL

      if(plot_event){
        if(n > 1 || n_workers > 1){
          plot_event <- FALSE
          if(!silent){
            warning('plot_event is forced to FALSE for parallel execution or when n > 1.',
                    immediate. = TRUE)
          }
        }
      }

      if(n_workers == 1){
        private$run_sequential_(n, plot_event, silent, dry_run)
      }else{
        private$run_parallel_(n, n_workers, silent, dry_run)
      }
    }
  )

)
