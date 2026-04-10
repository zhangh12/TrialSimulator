
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
