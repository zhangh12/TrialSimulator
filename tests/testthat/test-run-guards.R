# Guards around the run()/reset() lifecycle and the trial clock:
# the backward-clock check in lock_data (early, state-clean) and the
# set_current_time backstop; run() cannot be called twice without reset()
# (a failed run counts too); reset() keeps every registered milestone,
# including those registered after run().

make_arm <- function(name, rate) {
  ep <- endpoint(name = 'pfs', type = 'tte', generator = rexp,
                 rate = log(2) / rate)
  a <- arm(name = name)
  a$add_endpoints(ep)
  a
}

make_trial <- function(seed = 1, n_patients = 400, duration = 40) {
  accrual <- data.frame(end_time = Inf, piecewise_rate = 30)
  trial(name = "t", n_patients = n_patients, duration = duration, seed = seed,
        enroller = StaggeredRecruiter, accrual_rate = accrual,
        dropout = rweibull, shape = 1, scale = 1e6,
        silent = TRUE)
}

## a fresh trial + listener + controller with milestones at the given
## calendar times, not yet run
make_setup <- function(times, seed = 1, actions = NULL) {
  pbo <- make_arm("pbo", 10)
  trt <- make_arm("trt", 12)
  tr <- make_trial(seed = seed)
  add_arms(tr, sample_ratio = c(1, 1), pbo, trt)

  lstn <- listener(silent = TRUE)
  for (i in seq_along(times)) {
    action <- if (is.null(actions)) doNothing else actions[[i]]
    lstn$add_milestones(
      milestone(name = names(times)[i],
                when = calendarTime(time = times[i]),
                action = action)
    )
  }
  list(trial = tr, listener = lstn,
       controller = controller(tr, lstn))
}


test_that("a milestone locking earlier than the trial clock errors before touching state", {

  ## milestones deliberately registered out of order: <late> fires first at
  ## calendar 20, then <early> resolves to calendar 10 < 20
  st <- make_setup(c(late = 20, early = 10))

  expect_error(
    st$controller$run(n = 1, silent = TRUE, plot_event = FALSE),
    regexp = "Cannot lock data for milestone <early>"
  )

  ## the guard fires before any state is touched for <early>
  pr <- st$trial$.__enclos_env__$private
  expect_equal(pr$now, 20)
  expect_equal(names(st$trial$get_milestone_time()), "late")
  expect_error(st$trial$get_locked_data("early"))
})


test_that("set_current_time refuses to move the clock backward", {

  st <- make_setup(c(final = 20))
  pr <- st$trial$.__enclos_env__$private

  pr$set_current_time(10)
  expect_equal(pr$now, 10)
  expect_error(pr$set_current_time(5), regexp = "cannot move backward")
  expect_equal(pr$now, 10)
})


test_that("run() cannot be called twice without reset(), and works after it", {

  st <- make_setup(c(interim = 8, final = 16))
  st$controller$run(n = 1, silent = TRUE, plot_event = FALSE)

  expect_error(
    st$controller$run(n = 1, silent = TRUE, plot_event = FALSE),
    regexp = "already been called"
  )

  st$controller$reset()
  st$controller$run(n = 1, silent = TRUE, plot_event = FALSE)
  out <- st$controller$get_output()
  expect_equal(nrow(out), 1)
  expect_true("milestone_time_<final>" %in% names(out))
})


test_that("a failed run also requires reset() before running again", {

  st <- make_setup(c(interim = 8, final = 16),
                   actions = list(doNothing,
                                  function(trial) stop("boom")))
  expect_error(st$controller$run(n = 1, silent = TRUE, plot_event = FALSE),
               regexp = "boom")
  expect_error(st$controller$run(n = 1, silent = TRUE, plot_event = FALSE),
               regexp = "already been called")
})


test_that("reset() keeps milestones registered after run()", {

  st <- make_setup(c(m1 = 8, m2 = 16))
  st$controller$run(n = 1, silent = TRUE, plot_event = FALSE)

  st$listener$add_milestones(milestone(name = "m3",
                                       when = calendarTime(time = 24)))
  st$controller$reset()
  expect_equal(st$listener$get_milestone_names(), c("m1", "m2", "m3"))

  st$controller$run(n = 1, silent = TRUE, plot_event = FALSE)
  out <- st$controller$get_output()
  expect_true(all(c("milestone_time_<m2>", "milestone_time_<m3>") %in%
                    names(out)))
})


test_that("reset() before any run() keeps registered milestones and warns", {

  st <- make_setup(c(m1 = 8, m2 = 16))
  expect_warning(st$controller$reset(), regexp = "nothing to be reset")
  expect_equal(st$listener$get_milestone_names(), c("m1", "m2"))

  st$controller$run(n = 1, silent = TRUE, plot_event = FALSE)
  expect_equal(nrow(st$controller$get_output()), 1)
})


test_that("between-replicate resets in run(n > 1) are not blocked by the guard", {

  st <- make_setup(c(m1 = 8, m2 = 16))
  st$controller$run(n = 3, silent = TRUE, plot_event = FALSE)

  expect_equal(st$listener$get_milestone_names(), c("m1", "m2"))
  expect_equal(nrow(st$controller$get_output()), 3)
})
