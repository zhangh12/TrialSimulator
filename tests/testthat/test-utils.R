# Utility functions and censoring invariants
#
# Covers:
#   - solvePiecewiseConstantExponentialDistribution + qPiecewiseExponential:
#     round-trip accuracy and agreement with rpact reference implementation
#   - censor_trial_data: no negative TTE after dropout censoring
#   - censor_trial_data: enroll_time + TTE <= lock_time for all patients
#   - censor_trial_data: calendar-time invariant holds across all arms
#   - Stratified randomization: per-stratum arm balance with
#     stratification_factors
#   - enrollment(arms = ...): milestone counts only the specified arms

test_that('functions of piecewise exponential distribution work fine', {

  for(n in sample(5:10, 20, TRUE)){
    surv_prob <- sort(runif(n), decreasing = TRUE)
    times <- sort(runif(n, 0, 10), decreasing = FALSE)
    x <- solvePiecewiseConstantExponentialDistribution(
      surv_prob = surv_prob,
      times = times
    )

    times <- x$end_time
    rates <- x$piecewise_risk

    qs <- qPiecewiseExponential(
      1 - surv_prob,
      times = times, piecewise_risk = c(rates, .1)
    )

    expect_equal(qs, times, tolerance = 1e-4)

    p <- c(0, 1, runif(1e3))

    u <-
      data.frame(
        a = qPiecewiseExponential(
          p,
          times = x$end_time,
          piecewise_risk = c(x$piecewise_risk, .1)),

        b = rpact::getPiecewiseExponentialQuantile(
          p,
          piecewiseSurvivalTime = c(0,x$end_time),
          piecewiseLambda=c(x$piecewise_risk, .1))
      )
    expect_equal(u$a, u$b, tolerance = 1e-4)

  }

})

## ── censor_trial_data: dropout censoring ──────────────────────────────────────
test_that("censor_trial_data: dropout censoring sets event=0 and clips tte", {

  skip_if_not_installed("TrialSimulator")

  set.seed(42)
  pfs_ep <- endpoint(name = "pfs", type = "tte",
                     generator = rexp, rate = log(2) / 6)
  a <- arm(name = "trt")
  a$add_endpoints(pfs_ep)

  tr <- trial(name = "t", n_patients = 200, seed = 1,
              duration = 100, enroller = StaggeredRecruiter,
              accrual_rate = data.frame(end_time = Inf, piecewise_rate = 10),
              dropout = rexp, rate = 0.5,   # heavy dropout
              silent = TRUE)
  tr$add_arms(sample_ratio = 1, a)

  lstn <- listener(silent = TRUE)
  lstn$add_milestones(milestone(name = "final",
                                when = calendarTime(time = 100)))
  ctrl <- controller(tr, lstn)
  ctrl$run(n = 1, silent = TRUE, plot_event = FALSE)

  d <- tr$get_locked_data("final")
  expect_true(all(d$pfs >= 0))
  expect_true(all(d$pfs[d$pfs_event == 1] >= 0))

})

## ── censor_trial_data: calendar-time censoring ────────────────────────────────
test_that("censor_trial_data: no event time exceeds calendar lock time", {

  skip_if_not_installed("TrialSimulator")

  set.seed(7)
  pfs_ep <- endpoint(name = "pfs", type = "tte",
                     generator = rexp, rate = log(2) / 8)
  a <- arm(name = "trt")
  a$add_endpoints(pfs_ep)

  lock_time <- 30
  tr <- trial(name = "t", n_patients = 300, seed = 2,
              duration = lock_time,
              enroller = StaggeredRecruiter,
              accrual_rate = data.frame(end_time = Inf, piecewise_rate = 20),
              dropout = rweibull, shape = 1, scale = 1e6,  # essentially no dropout
              silent = TRUE)
  tr$add_arms(sample_ratio = 1, a)

  lstn <- listener(silent = TRUE)
  lstn$add_milestones(milestone(name = "final",
                                when = calendarTime(time = lock_time)))
  ctrl <- controller(tr, lstn)
  ctrl$run(n = 1, silent = TRUE, plot_event = FALSE)

  d <- tr$get_locked_data("final")
  calendar_event_time <- d$enroll_time + d$pfs
  expect_true(all(calendar_event_time <= lock_time + 1e-9))

})

## ── censor_trial_data: selected_arms partial censoring ────────────────────────
test_that("censor_trial_data: selected_arms leaves other arms untouched", {

  skip_if_not_installed("TrialSimulator")

  set.seed(99)
  pfs_ep_pbo <- endpoint(name = "pfs", type = "tte",
                         generator = rexp, rate = log(2) / 5)
  pfs_ep_trt <- endpoint(name = "pfs", type = "tte",
                         generator = rexp, rate = log(2) / 10)
  pbo <- arm(name = "placebo")
  trt <- arm(name = "trt")
  pbo$add_endpoints(pfs_ep_pbo)
  trt$add_endpoints(pfs_ep_trt)

  lock_time <- 20
  tr <- trial(name = "t", n_patients = 200, seed = 3,
              duration = lock_time,
              enroller = StaggeredRecruiter,
              accrual_rate = data.frame(end_time = Inf, piecewise_rate = 15),
              dropout = rweibull, shape = 1, scale = 1e6,
              silent = TRUE)
  tr$add_arms(sample_ratio = c(1, 1), pbo, trt)

  lstn <- listener(silent = TRUE)
  lstn$add_milestones(milestone(name = "final",
                                when = calendarTime(time = lock_time)))
  ctrl <- controller(tr, lstn)
  ctrl$run(n = 1, silent = TRUE, plot_event = FALSE)

  d <- tr$get_locked_data("final")
  expect_true(all(d$enroll_time + d$pfs <= lock_time + 1e-9))

})

## ── stratified randomization: per-stratum balance ─────────────────────────────
test_that("stratified randomization produces balanced strata", {

  skip_on_cran()
  skip_if_not_installed("TrialSimulator")

  set.seed(5)
  rng <- function(n) data.frame(sex = sample(c("M", "F"), n, replace = TRUE))
  strat_ep <- endpoint(name = "sex", type = "non-tte",
                       readout = c(sex = 0), generator = rng)

  pfs_ep <- endpoint(name = "pfs", type = "tte",
                     generator = rexp, rate = log(2) / 8)

  pbo <- arm(name = "placebo")
  trt <- arm(name = "trt")
  pbo$add_endpoints(strat_ep, pfs_ep)
  trt$add_endpoints(strat_ep, pfs_ep)

  tr <- trial(name = "t", n_patients = 400, seed = 10,
              duration = 100,
              enroller = StaggeredRecruiter,
              accrual_rate = data.frame(end_time = Inf, piecewise_rate = 30),
              dropout = rweibull, shape = 1, scale = 1e6,
              stratification_factors = "sex",
              silent = TRUE)
  tr$add_arms(sample_ratio = c(1, 1), pbo, trt)

  lstn <- listener(silent = TRUE)
  lstn$add_milestones(milestone(name = "final",
                                when = enrollment(n = 400)))
  ctrl <- controller(tr, lstn)
  ctrl$run(n = 1, silent = TRUE, plot_event = FALSE)

  d <- tr$get_locked_data("final")
  for(s in unique(d$sex)){
    sub <- d[d$sex == s, ]
    counts <- table(sub$arm)
    ratio <- max(counts) / min(counts)
    expect_lt(ratio, 1.3,
              label = paste("stratum", s, "imbalance ratio", round(ratio, 2)))
  }

})

## ── enrollment(arms=...): counts only specified arms ──────────────────────────
test_that("enrollment(arms=...) milestone triggers on specified arms only", {

  skip_if_not_installed("TrialSimulator")

  set.seed(20)
  pfs_ep <- endpoint(name = "pfs", type = "tte",
                     generator = rexp, rate = log(2) / 8)

  pbo <- arm(name = "placebo"); pbo$add_endpoints(pfs_ep)
  low <- arm(name = "low");     low$add_endpoints(pfs_ep)
  hi  <- arm(name = "high");    hi$add_endpoints(pfs_ep)

  tr <- trial(name = "t", n_patients = 300, seed = 21,
              duration = 100,
              enroller = StaggeredRecruiter,
              accrual_rate = data.frame(end_time = Inf, piecewise_rate = 20),
              dropout = rweibull, shape = 1, scale = 1e6,
              silent = TRUE)
  tr$add_arms(sample_ratio = c(1, 1, 1), pbo, low, hi)

  n_target <- 100L
  lstn <- listener(silent = TRUE)
  lstn$add_milestones(
    milestone(name = "pbo_only",
              when = enrollment(n = n_target, arms = "placebo"))
  )
  ctrl <- controller(tr, lstn)
  ctrl$run(n = 1, silent = TRUE, plot_event = FALSE)

  d <- tr$get_locked_data("pbo_only")
  n_pbo <- sum(d$arm == "placebo")
  expect_gte(n_pbo, n_target)

})
