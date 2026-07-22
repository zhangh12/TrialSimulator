# User-facing wrappers for adaptive trial mutations
#
# Covers: add_arms, remove_arms, resize, set_duration, update_generator,
# update_sample_ratio, stop_followup — each invoked inside an action function
# to verify they forward to the corresponding Trials$... method.

make_arm <- function(name, rate) {
  ep <- endpoint(name = 'pfs', type = 'tte', generator = rexp,
                 rate = log(2) / rate)
  a <- arm(name = name)
  a$add_endpoints(ep)
  a
}

make_trial <- function(seed = 1, n_patients = 400, duration = 30) {
  accrual <- data.frame(end_time = Inf, piecewise_rate = 30)
  trial(name = "t", n_patients = n_patients, duration = duration, seed = seed,
        enroller = StaggeredRecruiter, accrual_rate = accrual,
        dropout = rweibull, shape = 1, scale = 1e6,
        silent = TRUE)
}


test_that("add_arms wrapper adds an arm at trial creation", {

  pbo <- make_arm("pbo", 10)
  trt <- make_arm("trt", 12)
  tr <- make_trial()
  add_arms(tr, sample_ratio = c(1, 1), pbo, trt)

  lstn <- listener(silent = TRUE)
  lstn$add_milestones(milestone(name = "f", when = calendarTime(time = 30)))
  controller(tr, lstn)$run(n = 1, silent = TRUE, plot_event = FALSE)

  d <- tr$get_locked_data("f")
  expect_equal(sort(unique(d$arm)), sort(c("pbo", "trt")))
})


test_that("remove_arms wrapper drops an arm after a milestone", {

  pbo <- make_arm("pbo", 10)
  trt1 <- make_arm("trt1", 12)
  trt2 <- make_arm("trt2", 14)
  tr <- make_trial()
  add_arms(tr, sample_ratio = c(1, 1, 1), pbo, trt1, trt2)

  drop <- milestone(name = "drop",
                    when = calendarTime(time = 10),
                    action = function(trial) { remove_arms(trial, "trt1") })
  final <- milestone(name = "final", when = calendarTime(time = 30))

  lstn <- listener(silent = TRUE)
  lstn$add_milestones(drop, final)
  controller(tr, lstn)$run(n = 1, silent = TRUE, plot_event = FALSE)

  d_before <- tr$get_locked_data("drop")
  d_after <- tr$get_locked_data("final")

  expect_true("trt1" %in% unique(d_before$arm))
  # trt1 patients present but censored at time of removal
  after_trt1 <- d_after[d_after$arm == "trt1", ]
  expect_true(nrow(after_trt1) > 0)
  expect_true(all(after_trt1$enroll_time + after_trt1$pfs <= 10 + 1e-6 |
                    after_trt1$pfs_event == 0))
})


test_that("resize wrapper increases maximum sample size mid-trial", {

  pbo <- make_arm("pbo", 10)
  trt <- make_arm("trt", 12)
  tr <- make_trial(n_patients = 200)
  add_arms(tr, sample_ratio = c(1, 1), pbo, trt)

  check <- milestone(name = "check",
                     when = calendarTime(time = 5),
                     action = function(trial) { resize(trial, 400) })
  final <- milestone(name = "final", when = calendarTime(time = 30))
  lstn <- listener(silent = TRUE)
  lstn$add_milestones(check, final)
  controller(tr, lstn)$run(n = 1, silent = TRUE, plot_event = FALSE)

  d <- tr$get_locked_data("final")
  # With a generous duration and piecewise_rate=30/month, we should comfortably
  # exceed the original 200-patient cap once resized.
  expect_gt(nrow(d), 200)
})


test_that("set_duration wrapper extends trial duration", {

  pbo <- make_arm("pbo", 10)
  trt <- make_arm("trt", 12)
  tr <- make_trial(duration = 15)
  add_arms(tr, sample_ratio = c(1, 1), pbo, trt)

  extend <- milestone(name = "extend",
                      when = calendarTime(time = 10),
                      action = function(trial) { set_duration(trial, 30) })
  final <- milestone(name = "final", when = calendarTime(time = 30))
  lstn <- listener(silent = TRUE)
  lstn$add_milestones(extend, final)
  controller(tr, lstn)$run(n = 1, silent = TRUE, plot_event = FALSE)

  # If set_duration had failed, the trial would have locked at time 15 and no
  # final milestone at time 30 would be reachable.
  t_final <- tr$get_milestone_time("final")
  expect_gte(t_final, 15)
})


test_that("update_generator wrapper swaps generator for an endpoint", {

  pbo <- make_arm("pbo", 10)
  trt <- make_arm("trt", 12)
  tr <- make_trial()
  add_arms(tr, sample_ratio = c(1, 1), pbo, trt)

  swap <- milestone(name = "swap",
                    when = calendarTime(time = 5),
                    action = function(trial) {
                      update_generator(trial,
                                       arm_name = "trt",
                                       endpoint_name = "pfs",
                                       generator = rexp,
                                       rate = log(2) / 50)
                    })
  final <- milestone(name = "final", when = calendarTime(time = 30))
  lstn <- listener(silent = TRUE)
  lstn$add_milestones(swap, final)
  expect_no_error(
    controller(tr, lstn)$run(n = 1, silent = TRUE, plot_event = FALSE)
  )
})


test_that("update_sample_ratio wrapper changes randomization ratio", {

  pbo <- make_arm("pbo", 10)
  trt <- make_arm("trt", 12)
  tr <- make_trial()
  add_arms(tr, sample_ratio = c(1, 1), pbo, trt)

  reallocate <- milestone(name = "reallocate",
                          when = calendarTime(time = 5),
                          action = function(trial) {
                            update_sample_ratio(trial,
                                                arm_names = c("pbo", "trt"),
                                                sample_ratios = c(1, 3))
                          })
  final <- milestone(name = "final", when = calendarTime(time = 30))
  lstn <- listener(silent = TRUE)
  lstn$add_milestones(reallocate, final)
  expect_no_error(
    controller(tr, lstn)$run(n = 1, silent = TRUE, plot_event = FALSE)
  )

  d <- tr$get_locked_data("final")
  # After reallocation trt should attract more enrollments than pbo in
  # patients enrolled after time 5.
  late <- d[d$enroll_time > 5, ]
  if(nrow(late) > 20){
    expect_gt(sum(late$arm == "trt"), sum(late$arm == "pbo"))
  }
})


test_that("stop_followup wrapper censors selected patients at the milestone", {

  pbo <- make_arm("pbo", 10)
  trt <- make_arm("trt", 12)
  tr <- make_trial()
  add_arms(tr, sample_ratio = c(1, 1), pbo, trt)

  stop <- milestone(name = "stop",
                    when = calendarTime(time = 10),
                    action = function(trial) { stop_followup(trial, arm == "pbo") })
  final <- milestone(name = "final", when = calendarTime(time = 30))
  lstn <- listener(silent = TRUE)
  lstn$add_milestones(stop, final)
  controller(tr, lstn)$run(n = 1, silent = TRUE, plot_event = FALSE)

  d <- tr$get_locked_data("final")

  # pbo patients enrolled by time 10 stop being followed at time 10
  pbo_early <- d[d$arm == "pbo" & d$enroll_time <= 10, ]
  expect_true(nrow(pbo_early) > 0)
  expect_true(all(pbo_early$enroll_time + pbo_early$pfs <= 10 + 1e-9))

  # pbo patients enrolled after the milestone are followed as usual
  pbo_late <- d[d$arm == "pbo" & d$enroll_time > 10, ]
  expect_true(nrow(pbo_late) > 0)
  expect_true(any(pbo_late$enroll_time + pbo_late$pfs > 10))

  # trt patients are untouched: events keep accruing after time 10
  trt_early <- d[d$arm == "trt" & d$enroll_time <= 10, ]
  expect_true(any(trt_early$enroll_time + trt_early$pfs > 10))
})


test_that("stop_followup grants additional follow-up before censoring", {

  pbo <- make_arm("pbo", 10)
  trt <- make_arm("trt", 12)
  tr <- make_trial()
  add_arms(tr, sample_ratio = c(1, 1), pbo, trt)

  stop <- milestone(name = "stop",
                    when = calendarTime(time = 10),
                    action = function(trial) {
                      stop_followup(trial, arm == "pbo",
                                    additional_followup = 5)
                    })
  final <- milestone(name = "final", when = calendarTime(time = 30))
  lstn <- listener(silent = TRUE)
  lstn$add_milestones(stop, final)
  controller(tr, lstn)$run(n = 1, silent = TRUE, plot_event = FALSE)

  d <- tr$get_locked_data("final")

  # the affected cohort is fixed at call time (milestone, time 10); its
  # follow-up stops at 10 + 5 = 15
  pbo_cohort <- d[d$arm == "pbo" & d$enroll_time <= 10, ]
  expect_true(nrow(pbo_cohort) > 0)
  expect_true(all(pbo_cohort$enroll_time + pbo_cohort$pfs <= 15 + 1e-9))

  # some cohort events must land in (10, 15]: follow-up truly continued past 10
  expect_true(any(pbo_cohort$enroll_time + pbo_cohort$pfs > 10))

  # pbo patients enrolled after the call are not part of the decision cohort
  # and are followed as usual
  pbo_after <- d[d$arm == "pbo" & d$enroll_time > 10, ]
  expect_true(nrow(pbo_after) > 0)
  expect_true(any(pbo_after$enroll_time + pbo_after$pfs > 15))
})


test_that("stop_followup with empty dots stops all enrolled patients", {

  pbo <- make_arm("pbo", 10)
  trt <- make_arm("trt", 12)
  tr <- make_trial()
  add_arms(tr, sample_ratio = c(1, 1), pbo, trt)

  stop <- milestone(name = "stop",
                    when = calendarTime(time = 10),
                    action = function(trial) { stop_followup(trial) })
  final <- milestone(name = "final", when = calendarTime(time = 30))
  lstn <- listener(silent = TRUE)
  lstn$add_milestones(stop, final)
  controller(tr, lstn)$run(n = 1, silent = TRUE, plot_event = FALSE)

  d <- tr$get_locked_data("final")

  early <- d[d$enroll_time <= 10, ]
  expect_true(nrow(early) > 0)
  expect_true(all(early$enroll_time + early$pfs <= 10 + 1e-9))

  # patients enrolled after the milestone are followed as usual
  late <- d[d$enroll_time > 10, ]
  expect_true(any(late$enroll_time + late$pfs > 10))
})


test_that("stop_followup validates additional_followup and filter conditions", {

  pbo <- make_arm("pbo", 10)
  trt <- make_arm("trt", 12)
  tr <- make_trial()
  add_arms(tr, sample_ratio = c(1, 1), pbo, trt)

  # cannot be called before any milestone has been triggered
  expect_error(tr$stop_followup(), "within an action function")

  # emulate a triggered milestone to reach argument and filter validation
  tr$set_current_time(5)
  tr$save_milestone_time(5, "checkpoint")

  expect_error(tr$stop_followup(additional_followup = -1),
               "cannot be negative")
  expect_error(tr$stop_followup(additional_followup = c(1, 2)),
               "single numeric value")
  expect_error(tr$stop_followup(additional_followup = "now"),
               "single numeric value")

  expect_error(tr$stop_followup(no_such_column > 1),
               "compatible with dplyr::filter")
})
