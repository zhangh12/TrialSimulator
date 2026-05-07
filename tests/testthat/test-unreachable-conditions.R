# Regression tests for milestone triggering conditions where one or more
# branches cannot be reached within the trial duration.
#
# Bug fixed in 1.18.1: a composite triggering condition combined with 'or'
# previously errored out as soon as any single branch could not reach its
# target, even when another branch was reachable. The infinite-lock-time
# check was moved from the per-helper functions into Trials$lock_data().

make_simple_trial <- function(silent = TRUE){
  pfs <- endpoint(name = 'pfs', type = 'tte', generator = rexp, rate = log(2)/10)
  pbo <- arm(name = 'pbo'); pbo$add_endpoints(pfs)

  pfs <- endpoint(name = 'pfs', type = 'tte', generator = rexp, rate = log(2)/12)
  trt <- arm(name = 'trt'); trt$add_endpoints(pfs)

  accrual_rate <- data.frame(end_time = c(2, Inf), piecewise_rate = c(10, 20))

  tr <- trial(
    name = 'unreachable', n_patients = 60, duration = 30,
    enroller = StaggeredRecruiter, accrual_rate = accrual_rate,
    dropout = rweibull, shape = 1.32, scale = 114.4,
    seed = 42,
    silent = silent
  )
  tr$add_arms(sample_ratio = c(1, 1), pbo, trt)
  tr
}

run_milestone <- function(tr, m, silent = TRUE){
  l <- listener(silent = silent)
  l$add_milestones(m)
  controller(tr, l)$run(plot_event = FALSE, silent = silent)
}

test_that("'or' composite resolves to the reachable branch when the other is unreachable (event)", {

  tr <- make_simple_trial()

  m <- milestone(name = 'm',
                 when = eventNumber(endpoint = 'pfs', n = 100000) |
                        calendarTime(time = 20))

  expect_no_error(suppressWarnings(run_milestone(tr, m)))
  expect_equal(unname(tr$get_milestone_time('m')), 20)

})

test_that("'or' composite resolves to the reachable branch when the other is unreachable (enrollment)", {

  tr <- make_simple_trial()

  m <- milestone(name = 'm',
                 when = enrollment(n = 100000) |
                        calendarTime(time = 15))

  expect_no_error(suppressWarnings(run_milestone(tr, m)))
  expect_equal(unname(tr$get_milestone_time('m')), 15)

})

test_that("'and' composite errors when one branch is unreachable", {

  tr <- make_simple_trial()

  m <- milestone(name = 'm',
                 when = eventNumber(endpoint = 'pfs', n = 100000) &
                        calendarTime(time = 20))

  expect_error(suppressWarnings(run_milestone(tr, m)),
               regexp = 'finite calendar time')

})

test_that("All-unreachable composite errors with the new lock_data message", {

  tr <- make_simple_trial()

  m <- milestone(name = 'm',
                 when = eventNumber(endpoint = 'pfs', n = 100000) |
                        eventNumber(endpoint = 'pfs', n = 200000))

  expect_error(suppressWarnings(run_milestone(tr, m)),
               regexp = 'finite calendar time')

})

test_that("silent = TRUE suppresses unreachable-target warnings cleanly", {

  tr <- make_simple_trial(silent = TRUE)

  m <- milestone(name = 'm',
                 when = eventNumber(endpoint = 'pfs', n = 100000) |
                        calendarTime(time = 20))

  ws <- character()
  withCallingHandlers(
    run_milestone(tr, m),
    warning = function(w){
      ws <<- c(ws, conditionMessage(w))
      invokeRestart('muffleWarning')
    }
  )

  # The semantic warning should be silenced.
  expect_false(any(grepl('No enough events', ws)))
  # And no base-R "no non-missing arguments to min" warning should leak,
  # which would happen if the silent guard short-circuited the data check.
  expect_false(any(grepl('no non-missing arguments', ws, fixed = TRUE)))

})

test_that("silent = FALSE still emits the unreachable-target warning", {

  tr <- make_simple_trial(silent = FALSE)

  m <- milestone(name = 'm',
                 when = eventNumber(endpoint = 'pfs', n = 100000) |
                        calendarTime(time = 20))

  expect_warning(suppressMessages(run_milestone(tr, m, silent = FALSE)),
                 regexp = 'No enough events')

})
