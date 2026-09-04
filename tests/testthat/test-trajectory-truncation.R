## ---- Trials$truncate_regimen_trajectory() (private) --------------------------
##
## lock_data() truncates each patient's regimen_trajectory to the switches
## that have happened by the lock time and counts them (n_switches). The
## logic lives in a private method so it can be tested on hand-crafted
## trajectories here.

truncate <- function(trajectory, enroll_time, lock_time){
  tr <- trial(name = 't', n_patients = 10, duration = 10, seed = 1,
              enroller = StaggeredRecruiter,
              accrual_rate = data.frame(end_time = Inf, piecewise_rate = 10),
              silent = TRUE)
  tr$.__enclos_env__$private$truncate_regimen_trajectory(trajectory, enroll_time, lock_time)
}

test_that('no switcher: trajectories are returned as is and n_switches are 0', {
  traj <- c('placebo@0', 'low dose@0', 'placebo@0')
  res <- truncate(traj, enroll_time = c(1, 2, 3), lock_time = 5)
  expect_identical(res$trajectory, traj)
  expect_identical(res$n_switches, c(0L, 0L, 0L))
})

test_that('switches before, at, and after the lock time are handled per entry', {
  traj <- c(
    'placebo@0;low dose@2',              # switch at calendar time 1 + 2 = 3 <= 5: kept
    'placebo@0;high dose@4',             # 1 + 4 = 5 == lock time: kept (<=)
    'placebo@0;low dose@5',              # 1 + 5 = 6 > 5: dropped, only arm@0 survives
    'placebo@0;low dose@1;high dose@3',  # 1 + 1 kept, 1 + 3 kept: 2 switches
    'placebo@0;low dose@1;high dose@4.5' # 1 + 1 kept, 1 + 4.5 dropped: 1 switch
  )
  res <- truncate(traj, enroll_time = rep(1, 5), lock_time = 5)
  expect_identical(res$trajectory, c(
    'placebo@0;low dose@2',
    'placebo@0;high dose@4',
    'placebo@0',
    'placebo@0;low dose@1;high dose@3',
    'placebo@0;low dose@1'
  ))
  expect_identical(res$n_switches, c(1L, 1L, 0L, 2L, 1L))
})

test_that('enrollment time shifts the switch to calendar time', {
  ## same switch time from enrollment, different enrollment times
  traj <- rep('placebo@0;trt@3', 3)
  res <- truncate(traj, enroll_time = c(0, 2, 2.5), lock_time = 5)
  expect_identical(res$trajectory, c('placebo@0;trt@3', 'placebo@0;trt@3', 'placebo@0'))
  expect_identical(res$n_switches, c(1L, 1L, 0L))
})

test_that('switchers and non-switchers interleaved keep their positions', {
  traj <- c('a@0', 'a@0;b@1', 'c@0', 'a@0;b@9', 'c@0;b@1;a@2')
  res <- truncate(traj, enroll_time = c(0, 0, 0, 0, 0), lock_time = 5)
  expect_identical(res$trajectory, c('a@0', 'a@0;b@1', 'c@0', 'a@0', 'c@0;b@1;a@2'))
  expect_identical(res$n_switches, c(0L, 1L, 0L, 0L, 2L))
})

test_that('a single switcher and an empty input are handled', {
  res <- truncate('placebo@0;trt@1', enroll_time = 0, lock_time = 5)
  expect_identical(res$trajectory, 'placebo@0;trt@1')
  expect_identical(res$n_switches, 1L)

  res <- truncate(character(0), enroll_time = numeric(0), lock_time = 5)
  expect_identical(res$trajectory, character(0))
  expect_identical(res$n_switches, integer(0))
})

test_that('a malformed trajectory is an internal error, not silent data loss', {
  malformed <- c(
    'placebo@0;trt@x',   # non-numeric time
    'placebo@0;2',       # entry without '@'
    'placebo@0;@2',      # empty name
    'placebo@0;trt@',    # empty time
    'placebo@0;a@b@2'    # two '@'
  )
  for(traj in malformed){
    expect_error(truncate(traj, enroll_time = 0, lock_time = 5),
                 'malformed regimen_trajectory', info = traj)
  }
  expect_error(truncate(c('a@0', 'b@0'), enroll_time = 0, lock_time = 5))

  ## the first entry must survive: a switcher enrolled after the lock time
  ## cannot be in locked data, and the first entry always has time 0
  expect_error(truncate('placebo@0;trt@1', enroll_time = 6, lock_time = 5),
               'initial segment')
  expect_error(truncate('placebo@10;trt@1', enroll_time = 0, lock_time = 5),
               'initial segment')
})

## ---- agreement with the per-patient implementation of 1.35.1 - 1.35.6 --------

old_truncate <- function(trajectory, enroll_time, lock_time){
  switchers <- grep(';', trajectory, fixed = TRUE)
  if(length(switchers) > 0){
    trajectory[switchers] <- mapply(
      function(traj_str, et){
        entries <- strsplit(traj_str, ';', fixed = TRUE)[[1]]
        times <- as.numeric(sub('.*@', '', entries))
        paste(entries[et + times <= lock_time], collapse = ';')
      },
      trajectory[switchers], enroll_time[switchers],
      SIMPLIFY = TRUE, USE.NAMES = FALSE)
  }
  n_switches <- integer(length(trajectory))
  if(length(switchers) > 0){
    n_switches[switchers] <- lengths(
      gregexpr('@', trajectory[switchers], fixed = TRUE)) - 1L
  }
  list(trajectory = trajectory, n_switches = n_switches)
}

test_that('vectorized truncation agrees with the per-patient implementation', {
  set.seed(20260904)
  arms <- c('placebo', 'low dose', 'high dose', 'trt-1')
  n <- 500
  ## locked data only holds patients enrolled by the lock time, so enrollment
  ## times must not exceed the smallest lock time used below
  enroll_time <- runif(n, 0, 5)
  n_sw <- sample(0:3, n, replace = TRUE, prob = c(.5, .25, .15, .1))
  traj <- vapply(seq_len(n), function(i){
    ## switch times from enrollment are increasing; built with paste0() as
    ## apply_regimens() does
    times <- cumsum(runif(n_sw[i], 0, 8))
    switches <- if(n_sw[i] > 0){
      paste0(sample(arms, n_sw[i], replace = TRUE), '@', times)
    }else{
      character(0)
    }
    paste(c(paste0(sample(arms, 1), '@0'), switches), collapse = ';')
  }, character(1))

  for(lock_time in c(5, 15, 25, 40)){
    expect_identical(truncate(traj, enroll_time, lock_time),
                     old_truncate(traj, enroll_time, lock_time))
  }
})

## ---- reserved characters ------------------------------------------------------

test_that("arm() rejects names containing '@' or ';'", {
  expect_error(arm(name = 'dose@high'), "must not contain '@' or ';'")
  expect_error(arm(name = 'a;b'), "must not contain '@' or ';'")
  expect_s3_class(arm(name = 'high dose'), 'Arms')
})

test_that("what() must not return a new_treatment containing '@' or ';'", {
  pfs <- endpoint(name = 'pfs', type = 'tte', generator = rexp, rate = log(2) / 5)
  pbo <- arm(name = 'placebo'); pbo$add_endpoints(pfs)

  what_fn <- function(patient_data) data.frame(patient_id = patient_data$patient_id,
                                               new_treatment = 'dose@high')
  when_fn <- function(patient_data) data.frame(patient_id = patient_data$patient_id,
                                               switch_time = 1)
  how_fn  <- function(patient_data) data.frame(patient_id = patient_data$patient_id)

  tr <- trial(name = 't', n_patients = 20, seed = 4, duration = 10,
              enroller = StaggeredRecruiter,
              accrual_rate = data.frame(end_time = Inf, piecewise_rate = 10),
              silent = TRUE)
  tr$add_regimen(regimen(what_fn, when_fn, how_fn))
  expect_error(tr$add_arms(sample_ratio = 1, pbo), "must not contain '@' or ';'")
})
