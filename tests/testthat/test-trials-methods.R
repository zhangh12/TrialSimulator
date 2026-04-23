# Trials, Listeners, and plot-method coverage
#
# Covers:
#   - Trials: print, get_description, get_arms_name, has_arm, get_sample_ratio,
#     get_number_arms, get_seed, event_plot, get_output, get_milestone_time
#   - Listeners: add_milestones errors on duplicates, print, get_milestone_names
#   - plot.three_state_model: runs to completion without error
#   - Endpoints$print output: exercises non-tte with readout path

test_that("Trials accessors and print reflect the constructed trial", {

  pbo <- arm(name = "pbo")
  pbo$add_endpoints(endpoint(name = "pfs", type = "tte",
                             generator = rexp, rate = log(2) / 8))
  trt <- arm(name = "trt")
  trt$add_endpoints(endpoint(name = "pfs", type = "tte",
                             generator = rexp, rate = log(2) / 12))

  tr <- trial(name = "demo", n_patients = 200, duration = 30, seed = 42L,
              enroller = StaggeredRecruiter,
              accrual_rate = data.frame(end_time = Inf, piecewise_rate = 20),
              silent = TRUE)
  tr$add_arms(sample_ratio = c(1, 1), pbo, trt)

  expect_equal(tr$get_seed(), 42L)
  expect_equal(sort(tr$get_arms_name()), sort(c("pbo", "trt")))
  expect_equal(tr$get_number_arms(), 2)
  expect_true(tr$has_arm())

  ratio <- tr$get_sample_ratio()
  expect_true(is.numeric(ratio) || is.list(ratio))

  desc <- tr$get_description()
  expect_true(is.character(desc) || is.null(desc))

  out <- capture.output(print(tr))
  expect_true(length(out) > 0)
})

test_that("Trials event_plot runs without error after a run", {

  pbo <- arm(name = "pbo")
  pbo$add_endpoints(endpoint(name = "pfs", type = "tte",
                             generator = rexp, rate = log(2) / 6))
  trt <- arm(name = "trt")
  trt$add_endpoints(endpoint(name = "pfs", type = "tte",
                             generator = rexp, rate = log(2) / 10))

  tr <- trial(name = "t", n_patients = 200, duration = 30, seed = 1L,
              enroller = StaggeredRecruiter,
              accrual_rate = data.frame(end_time = Inf, piecewise_rate = 20),
              silent = TRUE)
  tr$add_arms(sample_ratio = c(1, 1), pbo, trt)

  lstn <- listener(silent = TRUE)
  lstn$add_milestones(milestone(name = "final",
                                when = calendarTime(time = 30)))

  ctrl <- controller(tr, lstn)
  ctrl$run(n = 1, silent = TRUE, plot_event = FALSE)

  pdf(NULL); on.exit(dev.off())
  expect_no_error(tr$event_plot())
  expect_gte(tr$get_milestone_time("final"), 0)
})

test_that("Listeners reject duplicate milestones and list their names", {

  lstn <- listener(silent = TRUE)
  m1 <- milestone(name = "m1", when = calendarTime(time = 10))
  m2 <- milestone(name = "m2", when = calendarTime(time = 20))
  dup <- milestone(name = "m1", when = calendarTime(time = 30))

  lstn$add_milestones(m1, m2)
  expect_equal(sort(lstn$get_milestone_names()),
               sort(c("m1", "m2")))

  expect_warning(lstn$add_milestones(dup), regexp = "over-write|already")

  out <- capture.output(print(lstn))
  expect_true(length(out) > 0)
})

test_that("plot.three_state_model runs to completion", {

  skip_if_not_installed("ggplot2")

  set.seed(7)
  ret <- solveThreeStateModel(median_pfs = 4.6, median_os = 9.6,
                              corr = c(.5, .6),
                              h12 = seq(.08, .2, length.out = 8))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(plot(ret))
})

test_that("Endpoints print handles tte and non-tte with readout", {

  ep_tte <- endpoint(name = "pfs", type = "tte",
                     generator = rexp, rate = 0.1)
  ep_non <- endpoint(name = "orr", type = "non-tte",
                     readout = c(orr = 6), generator = rnorm)

  out_tte <- capture.output(print(ep_tte))
  out_non <- capture.output(print(ep_non))
  expect_true(length(out_tte) > 0)
  expect_true(length(out_non) > 0)
})

test_that("Trials save/get_custom_data and get_output round-trip", {

  pbo <- arm(name = "pbo")
  pbo$add_endpoints(endpoint(name = "pfs", type = "tte",
                             generator = rexp, rate = 0.1))

  tr <- trial(name = "t", n_patients = 100, duration = 20, seed = 3L,
              enroller = StaggeredRecruiter,
              accrual_rate = data.frame(end_time = Inf, piecewise_rate = 20),
              silent = TRUE)
  tr$add_arms(sample_ratio = 1, pbo)

  act <- function(trial) {
    trial$save(value = 42L, name = "answer")
    trial$save_custom_data(value = list(x = 1), name = "cfg")
  }

  lstn <- listener(silent = TRUE)
  lstn$add_milestones(milestone(name = "final",
                                action = act,
                                when = calendarTime(time = 20)))

  ctrl <- controller(tr, lstn)
  ctrl$run(n = 1, silent = TRUE, plot_event = FALSE)

  op <- ctrl$get_output()
  expect_true("answer" %in% names(op))
  expect_equal(op$answer[1], 42)

  expect_equal(tr$get_custom_data("cfg"), list(x = 1))
})
