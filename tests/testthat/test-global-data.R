# Lifecycle of the two user data storages of a trial
#
# custom_data (save_custom_data/get/bind) is replicate-local: it passes
# intermediate results between action functions within one simulated trial,
# and is cleared when run() starts, before every replicate, and when run()
# returns. global_data (the global_data argument of trial(), read with
# get_global_data()) is read-only and available in every replicate: set only
# at trial creation, never overwritten or removed, with reference-semantics
# objects (R6, data.table) copied at registration and on read so the
# registered template stays pristine.

gd_trial <- function(seed = 1, n_patients = 100, global_data = list()) {
  pbo <- arm(name = "pbo")
  pbo$add_endpoints(endpoint(name = "pfs", type = "tte",
                             generator = rexp, rate = log(2) / 5))
  tr <- trial(name = "t", n_patients = n_patients, seed = seed,
              enroller = StaggeredRecruiter,
              accrual_rate = data.frame(end_time = Inf, piecewise_rate = 20),
              silent = TRUE, global_data = global_data)
  tr$add_arms(sample_ratio = 1, pbo)
  tr
}

gd_run <- function(trial, action, n = 1) {
  lstn <- listener(silent = TRUE)
  lstn$add_milestones(milestone(name = "m", when = calendarTime(time = 10),
                                action = action))
  ctrl <- controller(trial, lstn)
  ctrl$run(n = n, silent = TRUE, plot_event = FALSE)
  ctrl
}

GdCounter <- R6::R6Class(
  "GdCounter",
  public = list(n = 0,
                bump = function() { self$n <- self$n + 1; invisible(self) })
)


test_that("custom data saved in an action does not leak into the next replicate", {

  tr <- gd_trial()
  act <- function(trial) {
    ## with the pre-1.38.0 snapshot pollution this stopped replicate 2
    ## with a duplicate-name error (overwrite defaults to FALSE)
    trial$save_custom_data(trial$get_current_time(), name = "look_time")
    trial$save(value = trial$get("look_time"), name = "look_time_out")
  }
  ctrl <- NULL
  expect_no_error(ctrl <- gd_run(tr, act, n = 3))
  out <- ctrl$get_output()
  expect_equal(nrow(out), 3)

  ## after run(), the trial keeps the LAST replicate's custom data,
  ## inspectable like the rest of its final state
  expect_equal(tr$get_custom_data("look_time"), out$look_time_out[3])

})

test_that("every replicate starts with an empty custom data storage", {

  tr <- gd_trial()

  ## data saved between trial() and run() must not reach replicate 1 either:
  ## it would make replicate 1 differ from all later ones
  tr$save_custom_data(1, name = "pre_run")

  act <- function(trial) {
    sees <- function(name) tryCatch({trial$get_custom_data(name); TRUE},
                                    error = function(e) FALSE)
    trial$save(value = sees("pre_run"), name = "sees_pre_run")
    trial$save(value = sees("x"), name = "sees_leftover")
    trial$save_custom_data(1, name = "x")
  }
  ctrl <- gd_run(tr, act, n = 3)
  out <- ctrl$get_output()
  expect_equal(out$sees_pre_run, rep(FALSE, 3))
  expect_equal(out$sees_leftover, rep(FALSE, 3))

  ## the last replicate's custom data stays readable after run()
  expect_equal(tr$get_custom_data("x"), 1)

  ## ... but the next run() discards it at start, together with anything
  ## saved between the runs
  ctrl$reset()
  tr$save_custom_data(99, name = "x")
  ctrl$run(n = 2, silent = TRUE, plot_event = FALSE)
  expect_equal(ctrl$get_output()$sees_leftover, rep(FALSE, 2))

})

test_that("save_custom_data validates its inputs", {

  tr <- gd_trial()
  expect_error(tr$save_custom_data(1, name = ""), "non-empty character")
  expect_error(tr$save_custom_data(1, name = c("a", "b")), "non-empty character")
  expect_error(tr$save_custom_data(NULL, name = "a"), "cannot be NULL")

  tr$save_custom_data(1, name = "a")
  expect_error(tr$save_custom_data(2, name = "a"), "Pick another name")
  ## the trial is silent, so overwriting warns nothing but still applies
  expect_no_warning(tr$save_custom_data(2, name = "a", overwrite = TRUE))
  expect_equal(tr$get("a"), 2)

  ## a non-silent trial warns on overwrite
  pbo <- arm(name = "pbo")
  pbo$add_endpoints(endpoint(name = "pfs", type = "tte",
                             generator = rexp, rate = log(2) / 5))
  tr2 <- suppressMessages(
    trial(name = "t2", n_patients = 100, seed = 1,
          enroller = StaggeredRecruiter,
          accrual_rate = data.frame(end_time = Inf, piecewise_rate = 20)))
  tr2$save_custom_data(1, name = "a")
  expect_warning(tr2$save_custom_data(2, name = "a", overwrite = TRUE),
                 "overwritten")
  expect_equal(tr2$get("a"), 2)

})

test_that("global data is available in every replicate and survives reset()", {

  tr <- gd_trial(global_data = list(config = list(fwer = 0.025)))

  act <- function(trial) {
    trial$save(value = trial$get_global_data("config")$fwer, name = "fwer")
  }
  ctrl <- gd_run(tr, act, n = 3)
  expect_equal(ctrl$get_output()$fwer, rep(0.025, 3))

  ## a rerun after reset() sees the identical global data
  ctrl$reset()
  ctrl$run(n = 2, silent = TRUE, plot_event = FALSE)
  expect_equal(ctrl$get_output()$fwer, rep(0.025, 2))

})

test_that("trial() validates global_data", {

  gd_trial_with <- function(global_data) {
    trial(name = "t", n_patients = 100, seed = 1,
          enroller = StaggeredRecruiter,
          accrual_rate = data.frame(end_time = Inf, piecewise_rate = 20),
          silent = TRUE, global_data = global_data)
  }

  expect_error(gd_trial_with(c(a = 1)), "plain list")
  expect_error(gd_trial_with(data.frame(a = 1)), "plain list")
  expect_error(gd_trial_with(list(1, 2)), "should be named")
  expect_error(gd_trial_with(list(a = 1, 2)), "should be named")
  expect_error(gd_trial_with(list(a = 1, a = 2)), "unique")
  expect_error(gd_trial_with(list(a = NULL)), "cannot be NULL")
  expect_error(gd_trial_with(list(a = 1, e = new.env())),
               "plain\\s+environment")
  expect_error(gd_trial_with(list(a = 1, b = list(e = new.env()))),
               "plain\\s+environment")

  ## an empty list (the default) and NULL are both fine
  expect_no_error(gd_trial_with(list()))
  expect_no_error(gd_trial_with(NULL))

  ## global data is read-only: there is no setter to overwrite or remove an
  ## entry, and a typo in get_global_data() is an informative error
  tr <- gd_trial(global_data = list(a = 1))
  expect_error(tr$get_global_data("typo"), "cannot be found in global_data")

})

test_that("R6 global data stays pristine: copied at registration and on read", {

  template <- GdCounter$new()
  tr <- gd_trial(global_data = list(counter = template))

  ## registration captures an independent copy of the external object
  template$bump()
  expect_equal(tr$get_global_data("counter")$n, 0)

  ## every read returns an independent clone of the registered template
  c1 <- tr$get_global_data("counter")
  c2 <- tr$get_global_data("counter")
  expect_false(identical(c1, c2))
  c1$bump()
  expect_equal(c2$n, 0)
  expect_equal(tr$get_global_data("counter")$n, 0)

  ## an R6 object nested deeper in plain lists is protected as well
  tr2 <- gd_trial(global_data = list(nested = list(cnt = GdCounter$new(),
                                                   alpha = .025)))
  tr2$get_global_data("nested")$cnt$bump()
  expect_equal(tr2$get_global_data("nested")$cnt$n, 0)
  expect_equal(tr2$get_global_data("nested")$alpha, .025)

  ## the intended workflow: update a per-replicate copy across milestones
  ## through custom data, while the template stays at its as-designed state
  act <- function(trial) {
    cnt <- trial$get_global_data("counter")
    cnt$bump()
    trial$save_custom_data(cnt, name = "cnt")
    trial$save(value = trial$get("cnt")$n, name = "n_seen")
  }
  ctrl <- gd_run(tr, act, n = 3)
  expect_equal(ctrl$get_output()$n_seen, rep(1, 3))

})

test_that("data.table global data is copied at registration and on read", {

  skip_if_not_installed("data.table")

  dt <- data.table::data.table(x = 1:3)
  tr <- gd_trial(global_data = list(dt = dt))

  ## in-place modification of the external object does not reach the trial
  data.table::set(dt, j = "x", value = 4:6)
  expect_equal(tr$get_global_data("dt")$x, 1:3)

  ## in-place modification of a returned copy does not reach the template
  out <- tr$get_global_data("dt")
  data.table::set(out, j = "x", value = 7:9)
  expect_equal(tr$get_global_data("dt")$x, 1:3)

})

test_that("run() refuses an already-run trial wrapped in a new controller", {

  tr <- gd_trial()
  act <- function(trial) invisible(NULL)
  gd_run(tr, act, n = 2)

  ## the has_run guard lives on the controller; a new controller around the
  ## dirty trial must hit the trial-level guard instead of failing at the
  ## first milestone with a misleading message
  lstn <- listener(silent = TRUE)
  lstn$add_milestones(milestone(name = "m", when = calendarTime(time = 10),
                                action = act))
  ctrl2 <- controller(tr, lstn)
  expect_error(ctrl2$run(n = 1, silent = TRUE, plot_event = FALSE),
               "already been run and is no longer in its as-designed state")

  ## reset() restores the as-designed state, after which the run is allowed
  ctrl2$reset()
  expect_no_error(ctrl2$run(n = 2, silent = TRUE, plot_event = FALSE))
  expect_equal(nrow(ctrl2$get_output()), 2)

})
