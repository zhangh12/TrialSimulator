## Progress bar in Controllers$run() (sequential mode, silent = TRUE).
## The bar is activated only when projected remaining time exceeds an
## internal threshold (1 minute), so a fast run never launches it.
## testthat runs in a non-dynamic terminal, thus nothing is rendered;
## these tests exercise the activation and ticking code paths.

make_controller <- function(action = doNothing) {
  control <- arm(name = "control arm")
  active <- arm(name = "active arm")
  control$add_endpoints(
    endpoint(name = "PFS", type = "tte", generator = rexp, rate = log(2) / 5))
  active$add_endpoints(
    endpoint(name = "PFS", type = "tte", generator = rexp, rate = log(2) / 6))

  tr <- trial(name = "t", n_patients = 50, duration = 40,
              accrual_rate = data.frame(end_time = Inf, piecewise_rate = 30),
              silent = TRUE)
  tr$add_arms(sample_ratio = c(1, 1), control, active)

  lst <- listener(silent = TRUE)
  lst$add_milestones(
    milestone(name = "final", action = action, when = calendarTime(time = 40)))

  controller(tr, lst)
}

test_that("fast silent run completes without activating the bar", {
  ctrl <- make_controller()
  expect_error(ctrl$run(n = 5, silent = TRUE), NA)
  expect_equal(nrow(ctrl$get_output()), 5)
})

test_that("slow replicates activate the bar and the run still completes", {
  skip_on_cran()

  ## replicate 1 takes ~1.2s, so projected remaining time at idx = 1 is
  ## about 1.2 * 399 > 60 seconds and the bar is activated; remaining
  ## replicates are fast and only tick the bar.
  cnt <- new.env()
  cnt$i <- 0
  slow_start <- function(trial) {
    cnt$i <- cnt$i + 1
    if (cnt$i <= 2) Sys.sleep(1.2)
    invisible(NULL)
  }

  ctrl <- make_controller(action = slow_start)
  ## the activated bar writes progress lines to stderr; divert them so the
  ## suite log stays clean (the assertions below don't inspect the display)
  msgs <- file(tempfile(), open = "wt")
  sink(msgs, type = "message")
  on.exit({ sink(type = "message"); close(msgs) }, add = TRUE)
  expect_error(ctrl$run(n = 400, silent = TRUE), NA)
  expect_equal(nrow(ctrl$get_output()), 400)
})
