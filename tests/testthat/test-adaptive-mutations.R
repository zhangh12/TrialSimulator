# User-facing wrappers for adaptive trial mutations
#
# Covers: add_arms, remove_arms, resize, set_duration, update_generator,
# update_sample_ratio, stop_followup, update_accrual_rate — each invoked
# inside an action function to verify they forward to the corresponding
# Trials$... method.

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


test_that("update_accrual_rate slows enrollment after the milestone", {

  pbo <- make_arm("pbo", 10)
  trt <- make_arm("trt", 12)
  tr <- make_trial()
  add_arms(tr, sample_ratio = c(1, 1), pbo, trt)

  slow <- milestone(name = "slow",
                    when = calendarTime(time = 10),
                    action = function(trial) {
                      update_accrual_rate(
                        trial,
                        data.frame(end_time = Inf, piecewise_rate = 5))
                    })
  final <- milestone(name = "final", when = calendarTime(time = 30))
  lstn <- listener(silent = TRUE)
  lstn$add_milestones(slow, final)
  controller(tr, lstn)$run(n = 1, silent = TRUE, plot_event = FALSE)

  d <- tr$get_locked_data("final")

  # before the milestone: original 30/month, i.e., 300 patients by time 10
  expect_equal(sum(d$enroll_time <= 10), 300)

  # after: deterministic 5/month starting one inter-arrival past the
  # milestone, so the first re-planned patient enrolls exactly at 10 + 1/5
  expect_equal(min(d$enroll_time[d$enroll_time > 10]), 10 + 1 / 5,
               tolerance = 1e-9)
  expect_equal(sum(d$enroll_time > 10 & d$enroll_time <= 20), 50)

  # regeneration keeps the duration-censoring invariant for re-planned rows
  expect_true(all(d$enroll_time + d$pfs <= 30 + 1e-9))
})


test_that("update_accrual_rate with a leading pause halts then resumes", {

  pbo <- make_arm("pbo", 10)
  trt <- make_arm("trt", 12)
  tr <- make_trial()
  add_arms(tr, sample_ratio = c(1, 1), pbo, trt)

  pause <- milestone(name = "pause",
                     when = calendarTime(time = 10),
                     action = function(trial) {
                       update_accrual_rate(
                         trial,
                         data.frame(end_time = c(5, Inf),
                                    piecewise_rate = c(0, 30)))
                     })
  final <- milestone(name = "final", when = calendarTime(time = 30))
  lstn <- listener(silent = TRUE)
  lstn$add_milestones(pause, final)
  controller(tr, lstn)$run(n = 1, silent = TRUE, plot_event = FALSE)

  d <- tr$get_locked_data("final")

  # no one enrolls during the 5-month pause after the milestone
  expect_equal(sum(d$enroll_time > 10 & d$enroll_time <= 15), 0)
  # enrollment resumes one inter-arrival after the pause ends
  expect_equal(min(d$enroll_time[d$enroll_time > 10]), 15 + 1 / 30,
               tolerance = 1e-9)
})


test_that("update_accrual_rate speeding up accrual keeps censoring correct", {

  pbo <- make_arm("pbo", 10)
  trt <- make_arm("trt", 12)
  tr <- make_trial(n_patients = 600, duration = 25)
  add_arms(tr, sample_ratio = c(1, 1), pbo, trt)

  fast <- milestone(name = "fast",
                    when = calendarTime(time = 10),
                    action = function(trial) {
                      update_accrual_rate(
                        trial,
                        data.frame(end_time = Inf, piecewise_rate = 60))
                    })
  final <- milestone(name = "final", when = calendarTime(time = 25))
  lstn <- listener(silent = TRUE)
  lstn$add_milestones(fast, final)
  controller(tr, lstn)$run(n = 1, silent = TRUE, plot_event = FALSE)

  d <- tr$get_locked_data("final")

  # patients re-planned to earlier times must be regenerated, not shifted:
  # every row still satisfies the duration-censoring invariant
  expect_true(all(d$enroll_time + d$pfs <= 25 + 1e-9))
  # the faster rate really pulls enrollment earlier: 300 patients remain at
  # the milestone, so the last enrolls exactly at 10 + 300/60 = 15
  expect_equal(nrow(d), 600)
  expect_equal(max(d$enroll_time), 15, tolerance = 1e-9)
})


test_that("resize after update_accrual_rate continues the new curve", {

  pbo <- make_arm("pbo", 10)
  trt <- make_arm("trt", 12)
  tr <- make_trial()
  add_arms(tr, sample_ratio = c(1, 1), pbo, trt)

  faster <- milestone(name = "faster",
                      when = calendarTime(time = 5),
                      action = function(trial) {
                        update_accrual_rate(
                          trial,
                          data.frame(end_time = Inf, piecewise_rate = 60))
                      })
  grow <- milestone(name = "grow",
                    when = calendarTime(time = 10),
                    action = function(trial) { resize(trial, 500) })
  final <- milestone(name = "final", when = calendarTime(time = 30))
  lstn <- listener(silent = TRUE)
  lstn$add_milestones(faster, grow, final)

  # resize() draws the extra patients from the re-planned redundant pool;
  # a stale old-curve pool would trip roll_back()'s ordering invariant
  expect_no_error(
    controller(tr, lstn)$run(n = 1, silent = TRUE, plot_event = FALSE)
  )
  expect_gt(nrow(tr$get_locked_data("final")), 400)
})


test_that("update_accrual_rate after enrollment completes re-plans resize reserves", {

  # n1 = 0 edge: the milestone fires after all 400 patients are enrolled
  # (30/month completes at 13.33), so only the redundant pool is re-planned;
  # a later resize() must draw added patients from the new curve
  pbo <- make_arm("pbo", 10)
  trt <- make_arm("trt", 12)
  tr <- make_trial()
  add_arms(tr, sample_ratio = c(1, 1), pbo, trt)

  upd <- milestone(name = "upd",
                   when = calendarTime(time = 20),
                   action = function(trial) {
                     update_accrual_rate(
                       trial,
                       data.frame(end_time = Inf, piecewise_rate = 10))
                   })
  grow <- milestone(name = "grow",
                    when = calendarTime(time = 22),
                    action = function(trial) { resize(trial, 450) })
  final <- milestone(name = "final", when = calendarTime(time = 30))
  lstn <- listener(silent = TRUE)
  lstn$add_milestones(upd, grow, final)
  controller(tr, lstn)$run(n = 1, silent = TRUE, plot_event = FALSE)

  d <- tr$get_locked_data("final")
  added <- d[d$enroll_time > 20, ]

  expect_equal(nrow(d), 450)
  expect_equal(nrow(added), 50)
  # added patients follow the new 10/month curve from the update milestone
  expect_equal(min(added$enroll_time), 20 + 1 / 10, tolerance = 1e-9)
  expect_true(all(abs(diff(sort(added$enroll_time)) - 1 / 10) < 1e-9))
})


test_that("update_accrual_rate validates context and accrual_rate", {

  pbo <- make_arm("pbo", 10)
  trt <- make_arm("trt", 12)
  tr <- make_trial()
  add_arms(tr, sample_ratio = c(1, 1), pbo, trt)

  # cannot be called before any milestone has been triggered
  expect_error(
    tr$update_accrual_rate(data.frame(end_time = Inf, piecewise_rate = 5)),
    "within an action function")

  # emulate a triggered milestone to reach accrual_rate validation; errors
  # from StaggeredRecruiter are re-signaled with update_accrual_rate context
  # followed by the actual message
  tr$set_current_time(5)
  tr$save_milestone_time(5, "checkpoint")

  expect_error(
    tr$update_accrual_rate(data.frame(end_time = 10, piecewise_rate = 5)),
    "Invalid accrual_rate passed to update_accrual_rate")
  expect_error(
    tr$update_accrual_rate(data.frame(end_time = 10, piecewise_rate = 5)),
    "must be Inf")
  expect_error(
    tr$update_accrual_rate(list(end_time = Inf, piecewise_rate = 5)),
    "data.frame")
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


test_that("stop_followup sets pending non-TTE readouts to NA", {

  # non-TTE endpoint with readout 3 months after enrollment
  make_resp_arm <- function(name, prob) {
    ep <- endpoint(name = 'resp', type = 'non-tte',
                   readout = c(resp = 3),
                   generator = rbinom, size = 1, prob = prob)
    a <- arm(name = name)
    a$add_endpoints(ep)
    a
  }

  pbo <- make_resp_arm("pbo", 0.3)
  trt <- make_resp_arm("trt", 0.5)
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

  # pbo patients whose readout completed by the stopping time keep their value
  pbo_done <- d[d$arm == "pbo" & d$enroll_time <= 7, ]
  expect_true(nrow(pbo_done) > 0)
  expect_true(all(!is.na(pbo_done$resp)))

  # pbo patients enrolled by 10 with readout pending at 10 lose the value
  pbo_pending <- d[d$arm == "pbo" & d$enroll_time > 7 & d$enroll_time <= 10, ]
  expect_true(nrow(pbo_pending) > 0)
  expect_true(all(is.na(pbo_pending$resp)))

  # pbo patients enrolled after the call and all trt patients are unaffected
  pbo_after <- d[d$arm == "pbo" & d$enroll_time > 10, ]
  expect_true(nrow(pbo_after) > 0)
  expect_true(all(!is.na(pbo_after$resp)))
  expect_true(all(!is.na(d$resp[d$arm == "trt"])))
})


test_that("stop_followup and update_accrual_rate combine in one action", {

  pbo <- make_arm("pbo", 10)
  trt <- make_arm("trt", 12)
  tr <- make_trial()
  add_arms(tr, sample_ratio = c(1, 1), pbo, trt)

  adapt <- milestone(name = "adapt",
                     when = calendarTime(time = 10),
                     action = function(trial) {
                       stop_followup(trial, arm == "pbo")
                       update_accrual_rate(
                         trial,
                         data.frame(end_time = Inf, piecewise_rate = 10))
                     })
  final <- milestone(name = "final", when = calendarTime(time = 30))
  lstn <- listener(silent = TRUE)
  lstn$add_milestones(adapt, final)
  controller(tr, lstn)$run(n = 1, silent = TRUE, plot_event = FALSE)

  d <- tr$get_locked_data("final")

  # the stopped cohort is exactly the pbo patients enrolled by the milestone
  pbo_cohort <- d[d$arm == "pbo" & d$enroll_time <= 10, ]
  expect_true(all(pbo_cohort$enroll_time + pbo_cohort$pfs <= 10 + 1e-9))

  # re-planned patients start one inter-arrival after the milestone: no
  # patient lands exactly at the milestone, so none is swept into the cohort
  late <- sort(d$enroll_time[d$enroll_time > 10])
  expect_equal(late[1], 10 + 1 / 10, tolerance = 1e-9)
  expect_true(all(abs(diff(late) - 1 / 10) < 1e-9))

  # pbo patients enrolled after the action are followed as usual
  pbo_late <- d[d$arm == "pbo" & d$enroll_time > 10, ]
  expect_true(nrow(pbo_late) > 0)
  expect_true(any(pbo_late$enroll_time + pbo_late$pfs > 10 + 1 / 10))
})


test_that("crossover registered before update_accrual_rate reaches re-planned patients", {

  os_e <- endpoint(name = 'os', type = 'tte', generator = rexp, rate = log(2)/10)
  ctrl <- arm(name = 'control'); ctrl$add_endpoints(os_e)
  trt  <- arm(name = 'trt');     trt$add_endpoints(os_e)

  tr <- trial(name = 'x', n_patients = 300, seed = 42, duration = 60,
              enroller = StaggeredRecruiter,
              accrual_rate = data.frame(end_time = Inf, piecewise_rate = 10),
              silent = TRUE)
  tr$add_arms(sample_ratio = c(1, 1), ctrl, trt)

  action <- function(trial){
    what <- function(patient_data)
      data.frame(patient_id    = patient_data$patient_id,
                 new_treatment = ifelse(patient_data$arm == 'control',
                                        'trt', NA_character_))
    how <- function(patient_data)
      data.frame(patient_id = patient_data$patient_id,
                 os = ifelse(patient_data$os > patient_data$switch_time,
                             patient_data$switch_time +
                               5 * (patient_data$os - patient_data$switch_time),
                             patient_data$os))
    crossover(trial, what = what, how = how)
    update_accrual_rate(trial,
                        data.frame(end_time = Inf, piecewise_rate = 5))
  }

  adapt <- milestone(name = 'adapt', when = calendarTime(time = 20),
                     action = action)
  final <- milestone(name = 'final', when = calendarTime(time = 60))
  lst <- listener(silent = TRUE)
  lst$add_milestones(adapt, final)

  expect_no_error(
    controller(tr, lst)$run(n = 1, silent = TRUE, plot_event = FALSE)
  )

  d <- tr$get_locked_data('final')

  # re-planned patients follow the new 5/month curve from the milestone
  late <- sort(d$enroll_time[d$enroll_time > 20])
  expect_true(length(late) > 0)
  expect_equal(late[1], 20 + 1 / 5, tolerance = 1e-9)

  # the crossover regimen is re-applied to the regenerated batch: control
  # patients enrolled after the milestone still switch treatment
  expect_true('n_switches' %in% names(d))
  expect_true(any(d$n_switches[d$arm == 'control' & d$enroll_time > 20] >= 1))
})
