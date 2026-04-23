# User-facing wrappers for adaptive trial mutations
#
# Covers: add_arms, remove_arms, resize, set_duration, update_generator,
# update_sample_ratio — each invoked inside an action function to verify
# they forward to the corresponding Trials$... method.

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
