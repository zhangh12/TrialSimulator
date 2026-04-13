
## ---- regimen_trajectory encoding --------------------------------------------

test_that('regimen_trajectory is "arm@0" when no patient switches', {

  skip_if_not_installed('TrialSimulator')

  pfs_e <- endpoint(name = 'pfs', type = 'tte', generator = rexp, rate = log(2)/8)
  os_e  <- endpoint(name = 'os',  type = 'tte', generator = rexp, rate = log(2)/16)
  pbo   <- arm(name = 'placebo'); pbo$add_endpoints(pfs_e, os_e)
  trt   <- arm(name = 'trt');     trt$add_endpoints(pfs_e, os_e)

  # what() returns NA for everyone — nobody switches
  what_fn <- function(patient_data) data.frame(patient_id = patient_data$patient_id, new_treatment = NA_character_)
  when_fn <- function(patient_data) data.frame(patient_id = patient_data$patient_id, switch_time = patient_data$pfs)
  how_fn  <- function(patient_data) data.frame(patient_id = patient_data$patient_id)

  tr <- trial(name = 't', n_patients = 60, seed = 1, duration = 30,
              enroller = StaggeredRecruiter,
              accrual_rate = data.frame(end_time = Inf, piecewise_rate = 10),
              silent = TRUE)
  tr$add_regimen(regimen(what_fn, when_fn, how_fn))
  tr$add_arms(sample_ratio = c(1, 1), pbo, trt)

  lst <- listener(silent = TRUE)
  lst$add_milestones(milestone(name = 'final', when = calendarTime(time = 30)))
  controller(tr, lst)$run(n = 1, silent = TRUE, plot_event = FALSE)

  d <- tr$get_locked_data('final')
  expect_equal(d$regimen_trajectory, paste0(d$arm, '@0'))
})

test_that('regimen_trajectory encodes a single switching round correctly', {

  skip_if_not_installed('TrialSimulator')

  pfs_e <- endpoint(name = 'pfs', type = 'tte', generator = rexp, rate = log(2)/8)
  os_e  <- endpoint(name = 'os',  type = 'tte', generator = rexp, rate = log(2)/16)
  pbo   <- arm(name = 'placebo'); pbo$add_endpoints(pfs_e, os_e)
  trt   <- arm(name = 'trt');     trt$add_endpoints(pfs_e, os_e)

  # all placebo patients switch to trt at their pfs time
  what_fn <- function(patient_data)
    data.frame(patient_id    = patient_data$patient_id,
               new_treatment = ifelse(patient_data$arm == 'placebo', 'trt', NA_character_))
  when_fn <- function(patient_data) data.frame(patient_id = patient_data$patient_id, switch_time = patient_data$pfs)
  how_fn  <- function(patient_data) data.frame(patient_id = patient_data$patient_id)

  tr <- trial(name = 't', n_patients = 100, seed = 2, duration = 200,
              enroller = StaggeredRecruiter,
              accrual_rate = data.frame(end_time = Inf, piecewise_rate = 10),
              silent = TRUE)
  tr$add_regimen(regimen(what_fn, when_fn, how_fn))
  tr$add_arms(sample_ratio = c(1, 1), pbo, trt)

  lst <- listener(silent = TRUE)
  lst$add_milestones(milestone(name = 'final', when = calendarTime(time = 200)))
  controller(tr, lst)$run(n = 1, silent = TRUE, plot_event = FALSE)

  d <- tr$get_locked_data('final')

  # trt arm: no switch
  expect_equal(d$regimen_trajectory[d$arm == 'trt'], rep('trt@0', sum(d$arm == 'trt')))

  # placebo arm: trajectory starts with 'placebo@0;trt@' and switch_time == pfs
  pbo_d <- d[d$arm == 'placebo', ]
  expect_true(all(startsWith(pbo_d$regimen_trajectory, 'placebo@0;trt@')))

  long <- expandRegimen(pbo_d)
  switched <- long[long$regimen == 'trt', ]
  expect_equal(switched$switch_time_from_enrollment,
               pbo_d$pfs[match(switched$patient_id, pbo_d$patient_id)])
})

test_that('regimen_trajectory accumulates two switching rounds for the same patients', {

  skip_if_not_installed('TrialSimulator')

  pfs_e <- endpoint(name = 'pfs', type = 'tte', generator = rexp, rate = log(2)/6)
  os_e  <- endpoint(name = 'os',  type = 'tte', generator = rexp, rate = log(2)/15)
  pbo   <- arm(name = 'placebo'); pbo$add_endpoints(pfs_e, os_e)
  trt   <- arm(name = 'trt');     trt$add_endpoints(pfs_e, os_e)

  # round 1: placebo → low dose at pfs
  what1 <- function(patient_data) data.frame(patient_id    = patient_data$patient_id,
                                   new_treatment = ifelse(patient_data$arm == 'placebo', 'low dose', NA_character_))
  when1 <- function(patient_data) data.frame(patient_id = patient_data$patient_id, switch_time = patient_data$pfs)
  how1  <- function(patient_data) data.frame(patient_id = patient_data$patient_id)

  # round 2: same placebo patients → high dose at os/2
  what2 <- function(patient_data) data.frame(patient_id    = patient_data$patient_id,
                                   new_treatment = ifelse(patient_data$arm == 'placebo', 'high dose', NA_character_))
  when2 <- function(patient_data) data.frame(patient_id = patient_data$patient_id, switch_time = patient_data$os / 2)
  how2  <- function(patient_data) data.frame(patient_id = patient_data$patient_id)

  tr <- trial(name = 't', n_patients = 60, seed = 3, duration = 200,
              enroller = StaggeredRecruiter,
              accrual_rate = data.frame(end_time = Inf, piecewise_rate = 10),
              silent = TRUE)
  tr$add_regimen(regimen(list(what1, what2), list(when1, when2), list(how1, how2)))
  tr$add_arms(sample_ratio = c(1, 1), pbo, trt)

  lst <- listener(silent = TRUE)
  lst$add_milestones(milestone(name = 'final', when = calendarTime(time = 200)))
  controller(tr, lst)$run(n = 1, silent = TRUE, plot_event = FALSE)

  d <- tr$get_locked_data('final')
  pbo_d <- d[d$arm == 'placebo', ]

  # every placebo patient must have exactly 3 segments
  expect_true(all(startsWith(pbo_d$regimen_trajectory, 'placebo@0;low dose@')))
  n_segments <- lengths(strsplit(pbo_d$regimen_trajectory, ';', fixed = TRUE))
  expect_true(all(n_segments == 3L))
})

test_that('when() returning rows in shuffled order still assigns correct switch times', {

  skip_if_not_installed('TrialSimulator')

  # when() reverses its output rows — if the framework used row position instead
  # of matching on patient_id, each patient would get the wrong switch time.
  # switch_time is set to patient_id * 0.01 so every patient has a unique,
  # patient_id-dependent value we can verify after the run.
  pfs_e <- endpoint(name = 'pfs', type = 'tte', generator = rexp, rate = log(2)/8)
  pbo <- arm(name = 'placebo'); pbo$add_endpoints(pfs_e)
  trt <- arm(name = 'trt');     trt$add_endpoints(pfs_e)

  tr <- trial(name = 't', n_patients = 40, seed = 7, duration = 200,
              enroller = StaggeredRecruiter,
              accrual_rate = data.frame(end_time = Inf, piecewise_rate = 20),
              silent = TRUE)
  tr$add_regimen(regimen(
    what = function(patient_data)
      data.frame(patient_id = patient_data$patient_id, new_treatment = 'switched'),
    when = function(patient_data) {
      df <- data.frame(patient_id = patient_data$patient_id,
                       switch_time = patient_data$patient_id * 0.01)
      df[rev(seq_len(nrow(df))), , drop = FALSE]   # deliberately reversed
    },
    how = function(patient_data) data.frame(patient_id = patient_data$patient_id)
  ))
  tr$add_arms(sample_ratio = c(1, 1), pbo, trt)

  lst <- listener(silent = TRUE)
  lst$add_milestones(milestone(name = 'final', when = calendarTime(time = 200)))
  controller(tr, lst)$run(n = 1, silent = TRUE, plot_event = FALSE)

  long <- expandRegimen(tr$get_locked_data('final'))
  switched <- long[long$switch_time_from_enrollment > 0, ]

  # each patient's switch_time must equal their patient_id * 0.01
  expect_equal(switched$switch_time_from_enrollment,
               switched$patient_id * 0.01)
})

test_that('lock_data trims switch entries that fall after the calendar lock time', {

  skip_if_not_installed('TrialSimulator')

  pfs_e <- endpoint(name = 'pfs', type = 'tte', generator = rexp, rate = log(2)/5)
  os_e  <- endpoint(name = 'os',  type = 'tte', generator = rexp, rate = log(2)/20)
  pbo   <- arm(name = 'placebo'); pbo$add_endpoints(pfs_e, os_e)

  # switch time is os/2 — often exceeds a short lock window
  what_fn <- function(patient_data) data.frame(patient_id = patient_data$patient_id, new_treatment = 'trt')
  when_fn <- function(patient_data) data.frame(patient_id = patient_data$patient_id, switch_time = patient_data$os / 2)
  how_fn  <- function(patient_data) data.frame(patient_id = patient_data$patient_id)

  lock_time <- 5
  tr <- trial(name = 't', n_patients = 200, seed = 4, duration = lock_time,
              enroller = StaggeredRecruiter,
              accrual_rate = data.frame(end_time = Inf, piecewise_rate = 100),
              silent = TRUE)
  tr$add_regimen(regimen(what_fn, when_fn, how_fn))
  tr$add_arms(sample_ratio = 1, pbo)

  lst <- listener(silent = TRUE)
  lst$add_milestones(milestone(name = 'final', when = calendarTime(time = lock_time)))
  controller(tr, lst)$run(n = 1, silent = TRUE, plot_event = FALSE)

  d <- tr$get_locked_data('final')

  # any switch entry that survived must satisfy enroll_time + switch_time <= lock_time
  long <- expandRegimen(d)
  switched <- long[long$switch_time_from_enrollment > 0, ]
  if(nrow(switched) > 0){
    enroll_times <- d$enroll_time[match(switched$patient_id, d$patient_id)]
    expect_true(all(enroll_times + switched$switch_time_from_enrollment <= lock_time + 1e-9))
  }

  # with a short lock window, at least some patients should have had their switch trimmed
  expect_true(any(d$regimen_trajectory == 'placebo@0'))
})

## ---- Argument routing -------------------------------------------------------

test_that('regimen() routes ... to the correct function only (no overlap)', {

  what_fn <- function(patient_data, prob) NULL
  when_fn <- function(patient_data, delay = 0) NULL
  how_fn  <- function(patient_data, factor = 1) NULL

  reg <- regimen(what_fn, when_fn, how_fn, prob = 0.3, delay = 5, factor = 1.2)

  expect_equal(reg$get_treatment_allocator_args(1), list(prob = 0.3))
  expect_equal(reg$get_time_selector_args(1),        list(delay = 5))
  expect_equal(reg$get_data_modifier_args(1),        list(factor = 1.2))
})

test_that('regimen() routes a shared arg to all matching functions across slots', {

  what_fn <- function(patient_data, scale) NULL
  when_fn <- function(patient_data) NULL
  how_fn  <- function(patient_data, scale) NULL

  reg <- regimen(what_fn, when_fn, how_fn, scale = 2.0)

  expect_equal(reg$get_treatment_allocator_args(1), list(scale = 2.0))
  expect_equal(reg$get_time_selector_args(1),        list())
  expect_equal(reg$get_data_modifier_args(1),        list(scale = 2.0))
})

test_that('regimen() routes shared arg across different slots (cross-slot)', {

  ## a is required in what, optional-with-default in when
  what_fn <- function(patient_data, a) NULL
  when_fn <- function(patient_data, a = 3) NULL
  how_fn  <- function(patient_data) NULL

  reg <- regimen(what_fn, when_fn, how_fn, a = 'aaaaa')

  expect_equal(reg$get_treatment_allocator_args(1)$a, 'aaaaa')
  expect_equal(reg$get_time_selector_args(1)$a,        'aaaaa')
  expect_equal(reg$get_data_modifier_args(1),          list())
})

## ---- List-of-functions form (mirrors test3.r) --------------------------------

test_that('regimen() routes args correctly across lists of functions', {

  ## Mirrors test3.r:
  ##   what:  f(patient_data, a, b='b')     x2  a required, b optional
  ##   when:  f(patient_data, a=3, b=4)         a and b both optional
  ##          f(patient_data, b=3, c)           b optional, c required
  ##   how:   f(patient_data, c, d=2)       x2  c required, d optional
  ##   dots:  a='aaaaa', c='ccccc'
  ##
  ##   b has defaults everywhere -> not required in dots, not an error
  ##   d has a default           -> not required in dots, not an error

  what1 <- function(patient_data, a, b = 'b') NULL
  what2 <- function(patient_data, a, b = 'b') NULL
  when1 <- function(patient_data, a = 3, b = 4) NULL
  when2 <- function(patient_data, b = 3, c) NULL
  how1  <- function(patient_data, c, d = 2) NULL
  how2  <- function(patient_data, c, d = 2) NULL

  reg <- regimen(
    list(what1, what2),
    list(when1, when2),
    list(how1,  how2),
    a = 'aaaaa', c = 'ccccc'
  )

  expect_equal(reg$get_treatment_allocator_args(1), list(a = 'aaaaa'))
  expect_equal(reg$get_treatment_allocator_args(2), list(a = 'aaaaa'))
  expect_equal(reg$get_time_selector_args(1),        list(a = 'aaaaa'))
  expect_equal(reg$get_time_selector_args(2),        list(c = 'ccccc'))
  expect_equal(reg$get_data_modifier_args(1),        list(c = 'ccccc'))
  expect_equal(reg$get_data_modifier_args(2),        list(c = 'ccccc'))
})

test_that('regimen() does not require args that have defaults everywhere', {

  ## b has defaults in what and when — omitting b from dots is fine
  what_fn <- function(patient_data, a, b = 'default_b') NULL
  when_fn <- function(patient_data, b = 4) NULL
  how_fn  <- function(patient_data) NULL

  expect_no_error(regimen(what_fn, when_fn, how_fn, a = 'x'))
})

## ---- Variable vs constant in ... --------------------------------------------

test_that('regimen() captures variable values eagerly', {

  p <- 0.7
  what_fn <- function(patient_data, prob) NULL
  when_fn <- function(patient_data) NULL
  how_fn  <- function(patient_data) NULL

  reg <- regimen(what_fn, when_fn, how_fn, prob = p)
  p <- 0.1   # mutate after construction

  expect_equal(reg$get_treatment_allocator_args(1)$prob, 0.7)
})

## ---- Function with ... in formals -------------------------------------------

test_that('regimen() passes all dots to a function that declares ...', {

  what_fn <- function(patient_data, ...) NULL
  when_fn <- function(patient_data) NULL
  how_fn  <- function(patient_data) NULL

  reg <- regimen(what_fn, when_fn, how_fn, prob = 0.5, delay = 3)

  expect_equal(reg$get_treatment_allocator_args(1), list(prob = 0.5, delay = 3))
  expect_equal(reg$get_time_selector_args(1),        list())
})

## ---- expandRegimen ----------------------------------------------------------

test_that('expandRegimen errors clearly when required columns are missing', {

  expect_error(expandRegimen(data.frame(x = 1)),
               regexp = 'expandRegimen:.*patient_id.*regimen_trajectory')

  expect_error(expandRegimen(data.frame(patient_id = 1)),
               regexp = 'expandRegimen:.*regimen_trajectory')

  expect_error(expandRegimen(data.frame(regimen_trajectory = 'a@0')),
               regexp = 'expandRegimen:.*patient_id')
})

test_that('expandRegimen returns correct long format for non-switchers', {

  d <- data.frame(patient_id = 1:3,
                  regimen_trajectory = c('placebo@0', 'placebo@0', 'placebo@0'),
                  stringsAsFactors = FALSE)
  out <- expandRegimen(d)

  expect_equal(nrow(out), 3L)
  expect_equal(names(out), c('patient_id', 'regimen', 'switch_time_from_enrollment'))
  expect_true(all(out$regimen == 'placebo'))
  expect_true(all(out$switch_time_from_enrollment == 0))
})

test_that('expandRegimen expands switchers into multiple rows per patient', {

  d <- data.frame(
    patient_id = c(1L, 2L),
    regimen_trajectory = c('placebo@0;low dose@5.5', 'placebo@0'),
    stringsAsFactors = FALSE
  )
  out <- expandRegimen(d)

  expect_equal(nrow(out), 3L)
  expect_equal(out$patient_id,                  c(1L, 1L, 2L))
  expect_equal(out$regimen,                     c('placebo', 'low dose', 'placebo'))
  expect_equal(out$switch_time_from_enrollment, c(0, 5.5, 0))
})

test_that('expandRegimen accepts data.table input without error', {

  skip_if_not_installed('data.table')
  d <- data.table::data.table(
    patient_id = 1:2,
    regimen_trajectory = c('placebo@0;high dose@3', 'trt@0')
  )
  out <- expandRegimen(d)

  expect_equal(nrow(out), 3L)
  expect_equal(names(out), c('patient_id', 'regimen', 'switch_time_from_enrollment'))
})

## ---- Backward compatibility -------------------------------------------------

test_that('regimen() with no ... is backward compatible', {

  what_fn <- function(patient_data) NULL
  when_fn <- function(patient_data) NULL
  how_fn  <- function(patient_data) NULL

  expect_no_error(regimen(what_fn, when_fn, how_fn))

  reg <- regimen(what_fn, when_fn, how_fn)
  expect_equal(reg$get_treatment_allocator_args(1), list())
  expect_equal(reg$get_time_selector_args(1),        list())
  expect_equal(reg$get_data_modifier_args(1),        list())
})

## ---- Error cases ------------------------------------------------------------

test_that('regimen() errors on unnamed positional arg', {

  what_fn <- function(patient_data, prob) NULL
  when_fn <- function(patient_data) NULL
  how_fn  <- function(patient_data) NULL

  expect_error(regimen(what_fn, when_fn, how_fn, 0.5), regexp = 'must be named')
})

test_that('regimen() errors on arg unknown to all functions', {

  what_fn <- function(patient_data) NULL
  when_fn <- function(patient_data) NULL
  how_fn  <- function(patient_data) NULL

  expect_error(regimen(what_fn, when_fn, how_fn, xyz = 1), regexp = 'Unknown argument')
})

test_that('regimen() errors when a required arg is not in ...', {

  what_fn <- function(patient_data, prob) NULL  # prob required, no default
  when_fn <- function(patient_data) NULL
  how_fn  <- function(patient_data) NULL

  expect_error(regimen(what_fn, when_fn, how_fn), regexp = 'Missing required argument')
})

test_that('regimen() errors when a required arg in a list function is not in ...', {

  ## All three must be lists when using the list form
  what1 <- function(patient_data) NULL
  what2 <- function(patient_data) NULL
  when1 <- function(patient_data) NULL
  when2 <- function(patient_data) NULL
  how1  <- function(patient_data, d = 2) NULL  # d optional — fine
  how2  <- function(patient_data, c) NULL       # c required — must be in dots

  expect_error(
    regimen(list(what1, what2), list(when1, when2), list(how1, how2)),
    regexp = 'Missing required argument'
  )
})

## ---- Runtime output validation ----------------------------------------------

# Shared minimal trial factory used by runtime-validation tests.
make_trial_with_regimen <- function(reg){
  pfs_e <- endpoint(name = 'pfs', type = 'tte', generator = rexp, rate = log(2)/8)
  pbo   <- arm(name = 'placebo'); pbo$add_endpoints(pfs_e)
  trt   <- arm(name = 'trt');     trt$add_endpoints(pfs_e)
  tr <- trial(name = 't', n_patients = 20, seed = 9, duration = 50,
              enroller = StaggeredRecruiter,
              accrual_rate = data.frame(end_time = Inf, piecewise_rate = 20),
              silent = TRUE)
  tr$add_regimen(reg)
  tr$add_arms(sample_ratio = c(1, 1), pbo, trt)
  lst <- listener(silent = TRUE)
  lst$add_milestones(milestone(name = 'final', when = calendarTime(time = 50)))
  controller(tr, lst)
}

test_that('runtime error when what() does not return a data frame', {

  skip_if_not_installed('TrialSimulator')

  reg <- regimen(
    what = function(patient_data) 'not a data frame',
    when = function(patient_data) data.frame(patient_id = patient_data$patient_id, switch_time = 1),
    how  = function(patient_data) data.frame(patient_id = patient_data$patient_id)
  )
  expect_error(make_trial_with_regimen(reg)$run(n = 1, silent = TRUE, plot_event = FALSE),
               regexp = 'what\\(\\)')
})

test_that('runtime error when what() is missing required columns', {

  skip_if_not_installed('TrialSimulator')

  reg <- regimen(
    what = function(patient_data) data.frame(patient_id = patient_data$patient_id),  # new_treatment missing
    when = function(patient_data) data.frame(patient_id = patient_data$patient_id, switch_time = 1),
    how  = function(patient_data) data.frame(patient_id = patient_data$patient_id)
  )
  expect_error(make_trial_with_regimen(reg)$run(n = 1, silent = TRUE, plot_event = FALSE),
               regexp = 'new_treatment')
})

test_that('runtime error when what() returns duplicate patient IDs', {

  skip_if_not_installed('TrialSimulator')

  reg <- regimen(
    what = function(patient_data) data.frame(
      patient_id    = c(patient_data$patient_id, patient_data$patient_id[1]),
      new_treatment = c(rep('trt', nrow(patient_data)), 'trt')
    ),
    when = function(patient_data) data.frame(patient_id = patient_data$patient_id, switch_time = 1),
    how  = function(patient_data) data.frame(patient_id = patient_data$patient_id)
  )
  expect_error(make_trial_with_regimen(reg)$run(n = 1, silent = TRUE, plot_event = FALSE),
               regexp = 'duplicated')
})

test_that('runtime error when when() returns duplicate patient IDs', {

  skip_if_not_installed('TrialSimulator')

  reg <- regimen(
    what = function(patient_data) data.frame(patient_id = patient_data$patient_id, new_treatment = 'trt'),
    when = function(patient_data) data.frame(
      patient_id  = c(patient_data$patient_id, patient_data$patient_id[1]),
      switch_time = 1
    ),
    how  = function(patient_data) data.frame(patient_id = patient_data$patient_id)
  )
  expect_error(make_trial_with_regimen(reg)$run(n = 1, silent = TRUE, plot_event = FALSE),
               regexp = 'duplicated')
})

test_that('runtime error when how() returns duplicate patient IDs', {

  skip_if_not_installed('TrialSimulator')

  reg <- regimen(
    what = function(patient_data) data.frame(patient_id = patient_data$patient_id, new_treatment = 'trt'),
    when = function(patient_data) data.frame(patient_id = patient_data$patient_id, switch_time = 1),
    how  = function(patient_data) data.frame(
      patient_id = c(patient_data$patient_id, patient_data$patient_id[1]),
      os         = 1
    )
  )
  expect_error(make_trial_with_regimen(reg)$run(n = 1, silent = TRUE, plot_event = FALSE),
               regexp = 'duplicated')
})

test_that('runtime error when when() does not return a data frame', {

  skip_if_not_installed('TrialSimulator')

  reg <- regimen(
    what = function(patient_data) data.frame(patient_id = patient_data$patient_id, new_treatment = 'trt'),
    when = function(patient_data) 42,
    how  = function(patient_data) data.frame(patient_id = patient_data$patient_id)
  )
  expect_error(make_trial_with_regimen(reg)$run(n = 1, silent = TRUE, plot_event = FALSE),
               regexp = 'when\\(\\)')
})

test_that('runtime error when when() does not cover all patients returned by what()', {

  skip_if_not_installed('TrialSimulator')

  reg <- regimen(
    what = function(patient_data) data.frame(patient_id = patient_data$patient_id, new_treatment = 'trt'),
    when = function(patient_data) data.frame(patient_id = patient_data$patient_id[1], switch_time = 1),
    how  = function(patient_data) data.frame(patient_id = patient_data$patient_id)
  )
  expect_error(make_trial_with_regimen(reg)$run(n = 1, silent = TRUE, plot_event = FALSE),
               regexp = 'when\\(\\)')
})

test_that('runtime error when how() tries to modify a protected column', {

  skip_if_not_installed('TrialSimulator')

  reg <- regimen(
    what = function(patient_data) data.frame(patient_id = patient_data$patient_id, new_treatment = 'trt'),
    when = function(patient_data) data.frame(patient_id = patient_data$patient_id, switch_time = 1),
    how  = function(patient_data) data.frame(patient_id = patient_data$patient_id, arm = 'new_arm')
  )
  expect_error(make_trial_with_regimen(reg)$run(n = 1, silent = TRUE, plot_event = FALSE),
               regexp = 'must not be modified')
})

## ---- Integration: args actually reach functions during execution -------------

test_that('regimen() ... args are passed to what/when/how at execution time', {

  ## Use <<- to capture the arg value received inside each function.
  ## If do.call does not inject the args, the function call would error
  ## (missing required arg) or received_* would remain NULL.

  received_prob  <- NULL
  received_accel <- NULL

  pfs_e <- endpoint(name = 'pfs', type = 'tte', generator = rexp, rate = log(2)/5)
  os_e  <- endpoint(name = 'os',  type = 'tte', generator = rexp, rate = log(2)/14)
  pbo   <- arm(name = 'placebo')
  pbo$add_endpoints(pfs_e, os_e)

  pfs_e2 <- endpoint(name = 'pfs', type = 'tte', generator = rexp, rate = log(2)/7)
  os_e2  <- endpoint(name = 'os',  type = 'tte', generator = rexp, rate = log(2)/18)
  trt    <- arm(name = 'trt')
  trt$add_endpoints(pfs_e2, os_e2)

  what_fn <- function(patient_data, prob){
    received_prob <<- prob
    data.frame(
      patient_id    = patient_data$patient_id,
      new_treatment = ifelse(patient_data$arm == 'placebo', 'trt', NA_character_)
    )
  }

  when_fn <- function(patient_data){
    data.frame(patient_id = patient_data$patient_id, switch_time = patient_data$pfs)
  }

  how_fn <- function(patient_data, accel){
    received_accel <<- accel
    data.frame(
      patient_id = patient_data$patient_id,
      os = patient_data$switch_time + accel * pmax(patient_data$os - patient_data$switch_time, 0)
    )
  }

  reg <- regimen(what_fn, when_fn, how_fn, prob = 0.42, accel = 1.3)

  accrual_rate <- data.frame(end_time = Inf, piecewise_rate = 10)
  tr <- trial(
    name = 'test', n_patients = 50, duration = 40,
    enroller = StaggeredRecruiter, accrual_rate = accrual_rate,
    silent = TRUE
  )
  tr$add_regimen(reg)
  tr$add_arms(sample_ratio = c(1, 1), pbo, trt)

  final <- milestone(name = 'final', when = calendarTime(time = 40))
  lst   <- listener(silent = TRUE)
  lst$add_milestones(final)

  controller(tr, lst)$run(n = 1, silent = TRUE, plot_event = FALSE)

  expect_equal(received_prob,  0.42)
  expect_equal(received_accel, 1.3)
})
