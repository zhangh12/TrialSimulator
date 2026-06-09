## ---- crossover() : milestone-triggered crossover ----------------------------

# Two-arm OS trial; an interim milestone action crosses control patients over to
# trt, extending only their post-switch OS. Returns the trial after running.
run_os_crossover <- function(action, milestone_time = 20, duration = 60,
                             seed = 42, n = 200){
  os_e <- endpoint(name = 'os', type = 'tte', generator = rexp, rate = log(2)/10)
  ctrl <- arm(name = 'control'); ctrl$add_endpoints(os_e)
  trt  <- arm(name = 'trt');     trt$add_endpoints(os_e)

  tr <- trial(name = 'x', n_patients = n, seed = seed, duration = duration,
              enroller = StaggeredRecruiter,
              accrual_rate = data.frame(end_time = Inf, piecewise_rate = 10),
              silent = TRUE)
  tr$add_arms(sample_ratio = c(1, 1), ctrl, trt)

  lst <- listener(silent = TRUE)
  lst$add_milestones(milestone(name = 'interim',
                               when = calendarTime(time = milestone_time),
                               action = action))
  lst$add_milestones(milestone(name = 'final', when = calendarTime(time = duration)))
  controller(tr, lst)$run(n = 1, silent = TRUE, plot_event = FALSE)
  tr
}

# OS values from an identical trial WITHOUT crossover (same seed → same latent).
baseline_os <- function(milestone_time = 20, duration = 60, seed = 42, n = 200){
  os_e <- endpoint(name = 'os', type = 'tte', generator = rexp, rate = log(2)/10)
  ctrl <- arm(name = 'control'); ctrl$add_endpoints(os_e)
  trt  <- arm(name = 'trt');     trt$add_endpoints(os_e)
  tr <- trial(name = 'x', n_patients = n, seed = seed, duration = duration,
              enroller = StaggeredRecruiter,
              accrual_rate = data.frame(end_time = Inf, piecewise_rate = 10),
              silent = TRUE)
  tr$add_arms(sample_ratio = c(1, 1), ctrl, trt)
  tr$get_trial_data()
}

test_that('crossover() preserves observed OS and alters only post-switch OS', {

  skip_if_not_installed('TrialSimulator')

  milestone_time <- 20

  action <- function(trial){
    what <- function(patient_data)
      data.frame(patient_id    = patient_data$patient_id,
                 new_treatment = ifelse(patient_data$arm == 'control', 'trt', NA_character_))
    how <- function(patient_data)
      data.frame(patient_id = patient_data$patient_id,
                 os = ifelse(patient_data$os > patient_data$switch_time,
                             patient_data$switch_time + 5 * (patient_data$os - patient_data$switch_time),
                             patient_data$os))
    crossover(trial, what = what, how = how)
  }

  td0 <- baseline_os(milestone_time)
  os_before <- setNames(td0$os, td0$patient_id)
  enr       <- setNames(td0$enroll_time, td0$patient_id)

  tr1      <- run_os_crossover(action, milestone_time)
  td1      <- tr1$get_trial_data()
  os_after <- setNames(td1$os, td1$patient_id)

  ctrl_ids   <- as.character(td0$patient_id[td0$arm == 'control'])
  cal_before <- enr[ctrl_ids] + os_before[ctrl_ids]
  dead_pre   <- ctrl_ids[cal_before <= milestone_time]   # OS event before the milestone
  alive      <- ctrl_ids[cal_before >  milestone_time]

  # OS observed before the milestone is untouched
  expect_equal(unname(os_after[dead_pre]), unname(os_before[dead_pre]))
  # trt arm never crosses over
  trt_ids <- as.character(td0$patient_id[td0$arm == 'trt'])
  expect_equal(unname(os_after[trt_ids]), unname(os_before[trt_ids]))
  # most still-alive control patients have their post-switch OS altered
  changed <- abs(os_after[alive] - os_before[alive]) > 1e-9
  expect_gt(mean(changed), 0.5)
})

test_that('crossover() only offers still-open patients to what()', {

  skip_if_not_installed('TrialSimulator')

  milestone_time <- 20
  seen <- NULL

  action <- function(trial){
    what <- function(patient_data){
      seen <<- patient_data$patient_id
      data.frame(patient_id = patient_data$patient_id, new_treatment = NA_character_)
    }
    how <- function(patient_data) data.frame(patient_id = patient_data$patient_id)
    crossover(trial, what = what, how = how)
  }

  tr <- run_os_crossover(action, milestone_time)
  td <- tr$get_trial_data()   # unchanged (nobody switched)

  open_at_T <- td$patient_id[(td$enroll_time + pmin(td$os, td$dropout_time)) > milestone_time]
  expect_setequal(seen, open_at_T)
})

test_that('crossover() errors when when() makes a switch predate the milestone', {

  skip_if_not_installed('TrialSimulator')

  action <- function(trial){
    what <- function(patient_data)
      data.frame(patient_id    = patient_data$patient_id,
                 new_treatment = ifelse(patient_data$arm == 'control', 'trt', NA_character_))
    # switch at enrollment (time 0) — for already-enrolled patients this is < T
    when <- function(patient_data)
      data.frame(patient_id = patient_data$patient_id, switch_time = 0)
    how <- function(patient_data) data.frame(patient_id = patient_data$patient_id)
    crossover(trial, what = what, how = how, when = when)
  }

  expect_error(run_os_crossover(action), regexp = 'predates the earliest crossover')
})

test_that('crossover() errors when how() changes a pre-switch (observed) cell', {

  skip_if_not_installed('TrialSimulator')

  # pfs + os: many patients have progressed (pfs observed) before the milestone
  # but are still alive (os open) -> eligible. how() that rewrites pfs touches a
  # pre-switch cell and must error.
  run_pfsos <- function(action){
    pfs_e <- endpoint(name = 'pfs', type = 'tte', generator = rexp, rate = log(2)/5)
    os_e  <- endpoint(name = 'os',  type = 'tte', generator = rexp, rate = log(2)/20)
    ctrl  <- arm(name = 'control'); ctrl$add_endpoints(pfs_e, os_e)
    trt   <- arm(name = 'trt');     trt$add_endpoints(pfs_e, os_e)
    tr <- trial(name = 'x', n_patients = 200, seed = 7, duration = 60,
                enroller = StaggeredRecruiter,
                accrual_rate = data.frame(end_time = Inf, piecewise_rate = 10),
                silent = TRUE)
    tr$add_arms(sample_ratio = c(1, 1), ctrl, trt)
    lst <- listener(silent = TRUE)
    lst$add_milestones(milestone(name = 'interim', when = calendarTime(time = 25),
                                 action = action))
    lst$add_milestones(milestone(name = 'final', when = calendarTime(time = 60)))
    controller(tr, lst)$run(n = 1, silent = TRUE, plot_event = FALSE)
    tr
  }

  action <- function(trial){
    what <- function(patient_data)
      data.frame(patient_id    = patient_data$patient_id,
                 new_treatment = ifelse(patient_data$arm == 'control', 'trt', NA_character_))
    how <- function(patient_data)              # blindly rewrite pfs for everyone
      data.frame(patient_id = patient_data$patient_id, pfs = patient_data$pfs * 2)
    crossover(trial, what = what, how = how)
  }

  expect_error(run_pfsos(action), regexp = 'pre-switch/locked')
})

test_that('crossover() records the switch in regimen_trajectory', {

  skip_if_not_installed('TrialSimulator')

  action <- function(trial){
    what <- function(patient_data)
      data.frame(patient_id    = patient_data$patient_id,
                 new_treatment = ifelse(patient_data$arm == 'control', 'trt', NA_character_))
    how <- function(patient_data) data.frame(patient_id = patient_data$patient_id)
    crossover(trial, what = what, how = how)
  }

  tr <- run_os_crossover(action)
  d  <- tr$get_locked_data('final')

  expect_true(any(grepl(';trt@', d$regimen_trajectory[d$arm == 'control'], fixed = TRUE)))
  expect_true(all(d$regimen_trajectory[d$arm == 'trt'] == 'trt@0'))
  expect_true(all(d$n_switches[d$arm == 'trt'] == 0L))

  long     <- expandRegimen(d)
  switched <- long[long$regimen == 'trt' & long$switch_time_from_enrollment > 0, ]
  expect_true(nrow(switched) > 0)
})

test_that('regimen() rejects the internal earliest_crossover_calendar_time arg', {
  what_fn <- function(patient_data) data.frame(patient_id = patient_data$patient_id, new_treatment = NA_character_)
  when_fn <- function(patient_data) data.frame(patient_id = patient_data$patient_id, switch_time = 1)
  how_fn  <- function(patient_data) data.frame(patient_id = patient_data$patient_id)
  expect_error(regimen(what_fn, when_fn, how_fn, earliest_crossover_calendar_time = 5),
               regexp = 'not a user argument of regimen')
})

test_that('calendarTime() forbids time = 0, so a milestone crossover T is always > 0', {
  # event-/enrollment-driven milestones also trigger only at t > 0, so a
  # crossover triplet's earliest time (get_current_time() + delay) is positive.
  expect_error(calendarTime(time = 0))
})

test_that('crossover() rejects an earliest crossover time of 0 (setup + delay 0)', {

  skip_if_not_installed('TrialSimulator')

  os_e <- endpoint(name = 'os', type = 'tte', generator = rexp, rate = log(2)/10)
  ctrl <- arm(name = 'control'); ctrl$add_endpoints(os_e)
  tr <- trial(name = 'x', n_patients = 40, seed = 5, duration = 60,
              enroller = StaggeredRecruiter,
              accrual_rate = data.frame(end_time = Inf, piecewise_rate = 10), silent = TRUE)
  tr$add_arms(sample_ratio = 1, ctrl)

  what <- function(patient_data)
    data.frame(patient_id = patient_data$patient_id, new_treatment = 'trt')
  how <- function(patient_data) data.frame(patient_id = patient_data$patient_id)

  # called at setup (current time 0) with delay 0 -> Tx == 0
  expect_error(crossover(tr, what = what, how = how),
               regexp = 'earliest crossover time is 0')
})

test_that('crossover() does not accumulate triplets across replicates', {

  skip_if_not_installed('TrialSimulator')

  action <- function(trial){
    what <- function(patient_data)
      data.frame(patient_id    = patient_data$patient_id,
                 new_treatment = ifelse(patient_data$arm == 'control', 'trt', NA_character_))
    how <- function(patient_data) data.frame(patient_id = patient_data$patient_id)
    crossover(trial, what = what, how = how)
  }

  os_e <- endpoint(name = 'os', type = 'tte', generator = rexp, rate = log(2)/10)
  ctrl <- arm(name = 'control'); ctrl$add_endpoints(os_e)
  trt  <- arm(name = 'trt');     trt$add_endpoints(os_e)
  tr <- trial(name = 'x', n_patients = 120, seed = 11, duration = 60,
              enroller = StaggeredRecruiter,
              accrual_rate = data.frame(end_time = Inf, piecewise_rate = 10),
              silent = TRUE)
  tr$add_arms(sample_ratio = c(1, 1), ctrl, trt)
  lst <- listener(silent = TRUE)
  lst$add_milestones(milestone(name = 'interim', when = calendarTime(time = 20), action = action))
  lst$add_milestones(milestone(name = 'final', when = calendarTime(time = 60)))

  expect_no_error(controller(tr, lst)$run(n = 3, silent = TRUE, plot_event = FALSE))

  # exactly one crossover triplet should remain (the last replicate's), not 3
  expect_equal(tr$get_regimen()$get_number_treatment_allocator(), 1L)
})

test_that('crossover() also covers patients enrolled after the milestone', {

  skip_if_not_installed('TrialSimulator')

  milestone_time <- 10   # before accrual finishes, so future entrants exist

  action <- function(trial){
    what <- function(patient_data)
      data.frame(patient_id    = patient_data$patient_id,
                 new_treatment = ifelse(patient_data$arm == 'control', 'trt', NA_character_))
    how <- function(patient_data) data.frame(patient_id = patient_data$patient_id)
    crossover(trial, what = what, how = how)
  }

  tr <- run_os_crossover(action, milestone_time)
  d  <- tr$get_locked_data('final')

  late_ctrl <- d[d$arm == 'control' & d$enroll_time > milestone_time, ]
  expect_true(nrow(late_ctrl) > 0)
  expect_true(all(grepl(';trt@', late_ctrl$regimen_trajectory, fixed = TRUE)))
})
