## ---- tidy argument of controller$run() -------------------------------------

tidy_trial <- function(seed = 11){
  rng <- function(n){
    data.frame(biomarker = rbinom(n, 1, .5),
               pfs = rexp(n, log(2) / 10), pfs_event = 1)
  }
  mk_arm <- function(name){
    a <- arm(name = name)
    a$add_endpoints(endpoint(name = c('biomarker', 'pfs'),
                             type = c('baseline', 'tte'),
                             generator = rng))
    a
  }
  tr <- trial(name = 'tidy', n_patients = 100, duration = 30, seed = seed,
              enroller = StaggeredRecruiter,
              accrual_rate = data.frame(end_time = Inf, piecewise_rate = 20),
              silent = TRUE)
  tr$add_arms(sample_ratio = c(1, 1), mk_arm('pbo'), mk_arm('trt'))
  l <- listener(silent = TRUE)
  l$add_milestones(
    milestone(name = 'interim', when = calendarTime(time = 5)),
    milestone(name = 'final', when = calendarTime(time = 10)))
  list(trial = tr, listener = l)
}

arms_cols <- function(out){
  grep('^n_events_<.*>_<arms>$', names(out), value = TRUE)
}

test_that('run(tidy = TRUE) drops only the per-arm table, at every replicate', {
  x <- tidy_trial()
  ctrl <- controller(x$trial, x$listener)
  ctrl$run(n = 3, silent = TRUE, plot_event = FALSE, tidy = TRUE)
  out <- ctrl$get_output()

  expect_equal(nrow(out), 3)
  ## the per-arm table is absent in all replicates, not only the first one
  ## (trial$reset() restores the snapshot value of the underlying field)
  expect_length(arms_cols(out), 0)
  ## per-endpoint totals and milestone times are still saved
  expect_true(all(c('n_events_<interim>_<pfs>', 'n_events_<final>_<pfs>',
                    'n_events_<interim>_<biomarker>',
                    'n_events_<final>_<patient_id>',
                    'milestone_time_<interim>', 'milestone_time_<final>')
                  %in% names(out)))
  expect_true(all(out[['n_events_<final>_<patient_id>']] > 0))

  ## the full table is still attached to locked data (used by event_plot())
  n_events <- attr(attr(x$trial$get_locked_data('final'), 'lock_time'), 'n_events')
  expect_true('arms' %in% names(n_events))
  expect_s3_class(n_events$arms[[1]], 'data.frame')
})

test_that('run(tidy = FALSE) keeps the per-arm table and matches tidy = TRUE otherwise', {
  x1 <- tidy_trial(seed = 11)
  c1 <- controller(x1$trial, x1$listener)
  c1$run(n = 2, silent = TRUE, plot_event = FALSE)
  out1 <- c1$get_output()

  x2 <- tidy_trial(seed = 11)
  c2 <- controller(x2$trial, x2$listener)
  c2$run(n = 2, silent = TRUE, plot_event = FALSE, tidy = TRUE)
  out2 <- c2$get_output()

  expect_equal(arms_cols(out1), c('n_events_<interim>_<arms>', 'n_events_<final>_<arms>'))
  common <- setdiff(names(out1), arms_cols(out1))
  expect_equal(names(out2), common)
  expect_equal(out1[, common], out2[, common])
})

test_that('tidy is validated', {
  x <- tidy_trial()
  ctrl <- controller(x$trial, x$listener)
  expect_error(ctrl$run(n = 1, silent = TRUE, plot_event = FALSE, tidy = NA))
  expect_error(ctrl$run(n = 1, silent = TRUE, plot_event = FALSE, tidy = 'yes'))
})

test_that('run(tidy = TRUE) is honored on the parallel path', {
  skip_if_not_installed('mirai')
  # as in test-parallel.R: covr cannot trace the worker R processes spawned
  # by n_workers > 1, and the truncated trace breaks its merge step
  skip_if(Sys.getenv("R_COVR") == "true",
          "n_workers > 1 spawns R processes that covr cannot trace")
  x <- tidy_trial()
  ctrl <- controller(x$trial, x$listener)
  ctrl$run(n = 4, n_workers = 2, silent = TRUE, plot_event = FALSE, tidy = TRUE)
  out <- ctrl$get_output()
  expect_equal(nrow(out), 4)
  expect_length(arms_cols(out), 0)
  expect_true('n_events_<final>_<pfs>' %in% names(out))
})
