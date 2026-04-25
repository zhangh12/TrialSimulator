# Endpoint data generators and helpers
#
# Covers:
#   - weibullDropout: parameter recovery at the two reference times; input
#     validation branches
#   - PiecewiseConstantExponentialRNG: shape of returned data, event flag at
#     last window, recovery of piecewise hazard via exponential MLE
#   - CorrelatedPfsAndOs3: PFS <= OS pointwise; sample correlation roughly at
#     expected level; column renaming through (pfs_name, os_name)
#   - CorrelatedPfsAndOs4: monotone ordering response <= progression <= death,
#     censoring at `duration`, rejection of malformed transition matrices
#   - solveThreeStateModel: output class, columns, and rough recovery of
#     (h01, h02) when true hazards generated the reference data

test_that("weibullDropout recovers parameters at reference time points", {

  set.seed(1)
  for(i in 1:5){
    t <- sort(runif(2, 1, 30))
    r <- sort(runif(2, 0.05, 0.5))   # increasing rates over time
    ps <- weibullDropout(time = t, dropout_rate = r)

    expect_named(ps, c("shape", "scale"))
    expect_equal(1 - exp(-(t[1] / ps["scale"])^ps["shape"]),
                 r[1], tolerance = 1e-4, ignore_attr = TRUE)
    expect_equal(1 - exp(-(t[2] / ps["scale"])^ps["shape"]),
                 r[2], tolerance = 1e-4, ignore_attr = TRUE)
  }
})

test_that("weibullDropout input validation", {

  expect_error(weibullDropout(time = 1, dropout_rate = c(.1, .2)),
               "time should be of length 2")
  expect_error(weibullDropout(time = c(12, 18), dropout_rate = .2),
               "dropout_rate should be of length 2")
  expect_error(weibullDropout(time = c(-1, 18), dropout_rate = c(.1, .2)),
               "time should be positive")
  expect_error(weibullDropout(time = c(12, 18), dropout_rate = c(0, .2)),
               "dropout_rate should be between 0 and 1")
  expect_error(weibullDropout(time = c(12, 18), dropout_rate = c(.2, 1)),
               "dropout_rate should be between 0 and 1")
})

test_that("PiecewiseConstantExponentialRNG returns expected columns and events", {

  risk <- data.frame(
    end_time = c(1, 4.33, 26.0, 52.0),
    piecewise_risk = c(1, 1.01, 0.381, 0.150) * exp(-4.01)
  )

  set.seed(5)
  d <- PiecewiseConstantExponentialRNG(1000, risk, 'PFS')
  expect_named(d, c('PFS', 'PFS_event'))
  expect_equal(nrow(d), 1000)
  expect_true(all(d$PFS_event %in% c(0, 1)))
  expect_true(all(d$PFS <= max(risk$end_time) + 1e-8))
  # PFS_event==0 <=> PFS was censored at last end_time
  expect_true(all(d$PFS[d$PFS_event == 0] == max(risk$end_time)))
})

test_that("PiecewiseConstantExponentialRNG approximates a constant-hazard exponential", {

  skip_on_cran()

  risk <- data.frame(end_time = c(1000), piecewise_risk = 0.1)

  set.seed(101)
  d <- PiecewiseConstantExponentialRNG(5000, risk, 'ep')

  # with one window and rate 0.1, mean ~ 1/0.1 = 10 for uncensored samples
  # observed events should dominate since end_time is very large
  expect_true(mean(d$ep_event) > 0.99)
  expect_equal(mean(d$ep[d$ep_event == 1]), 10, tolerance = 0.5)
})

test_that("PiecewiseConstantExponentialRNG honors hazard_ratio column", {

  risk1 <- data.frame(end_time = 500,
                      piecewise_risk = 0.05,
                      hazard_ratio = 2)
  risk2 <- data.frame(end_time = 500,
                      piecewise_risk = 0.10)

  set.seed(77)
  d1 <- PiecewiseConstantExponentialRNG(4000, risk1, 'ep')
  set.seed(77)
  d2 <- PiecewiseConstantExponentialRNG(4000, risk2, 'ep')
  expect_equal(d1$ep, d2$ep, tolerance = 1e-10)
})

test_that("PiecewiseConstantExponentialRNG rejects bad risk tables", {

  expect_error(PiecewiseConstantExponentialRNG(10, list(end_time = 1), 'ep'),
               regexp = NULL)
  expect_error(PiecewiseConstantExponentialRNG(10,
                                               data.frame(end_time = 1), 'ep'),
               "Missed column")
  expect_error(PiecewiseConstantExponentialRNG(10,
                                               data.frame(piecewise_risk = .1),
                                               'ep'),
               "Missed column")
})

test_that("CorrelatedPfsAndOs3 produces PFS <= OS and renames columns", {

  skip_on_cran()

  set.seed(2024)
  d <- CorrelatedPfsAndOs3(5000, h01 = 0.10, h02 = 0.05, h12 = 0.12,
                           pfs_name = 'PFS', os_name = 'OS')
  expect_named(d, c('PFS', 'OS', 'PFS_event', 'OS_event'),
               ignore.order = TRUE)
  expect_true(all(d$PFS <= d$OS + 1e-10))
  expect_true(all(d$PFS_event == 1))
  expect_true(all(d$OS_event == 1))

  # median of PFS follows exp(h01 + h02) = 0.15 => median = log(2)/0.15 ~ 4.62
  expect_equal(median(d$PFS), log(2) / 0.15, tolerance = 0.3)

  # rough correlation in expected range (0.5-0.8 for these hazards)
  rho <- cor(d$PFS, d$OS)
  expect_gt(rho, 0.3)
  expect_lt(rho, 0.95)
})

test_that("CorrelatedPfsAndOs4 rejects bad transition matrices", {

  ok <- matrix(c(0.99, 0.0035, 0.0055, 0.0010,
                 0,    0.9900, 0.0052, 0.0048,
                 0,    0,      0.9960, 0.0040,
                 0,    0,      0,      1),
               nrow = 4, byrow = TRUE)

  expect_error(CorrelatedPfsAndOs4(10, matrix(0, 3, 3), duration = 10),
               "4x4 matrix")
  bad_neg <- ok
  bad_neg[1, 2] <- -0.1
  expect_error(CorrelatedPfsAndOs4(10, bad_neg, 10), "non-negative")

  bad_rows <- ok
  bad_rows[1, 1] <- 0.5
  expect_error(CorrelatedPfsAndOs4(10, bad_rows, 10), "sum to 1")

  bad_lower <- ok
  bad_lower[2, 1] <- 0.05
  bad_lower[2, 2] <- bad_lower[2, 2] - 0.05   # keep row sum = 1
  expect_error(CorrelatedPfsAndOs4(10, bad_lower, 10), "lower triangle")
})

test_that("CorrelatedPfsAndOs4 produces ordered states with censoring at duration", {

  m <- matrix(c(0.99, 0.0035, 0.0055, 0.0010,
                0,    0.9900, 0.0052, 0.0048,
                0,    0,      0.9960, 0.0040,
                0,    0,      0,      1),
              nrow = 4, byrow = TRUE)

  set.seed(42)
  d <- CorrelatedPfsAndOs4(500, m, duration = 200,
                           death_name = 'os',
                           progression_name = 'pfs',
                           response_name = 'or')

  expect_named(d,
               c('or', 'pfs', 'os', 'or_event', 'pfs_event', 'os_event'),
               ignore.order = TRUE)
  expect_equal(nrow(d), 500)

  # state times must be <= duration
  expect_true(all(d$or <= 200))
  expect_true(all(d$pfs <= 200))
  expect_true(all(d$os <= 200))

  # ordering when all three events happened
  full <- d[d$or_event == 1 & d$pfs_event == 1 & d$os_event == 1, ]
  if(nrow(full) > 0){
    expect_true(all(full$or <= full$pfs))
    expect_true(all(full$pfs <= full$os))
  }

  # censored rows should sit at duration
  expect_true(all(d$pfs[d$pfs_event == 0] == 200))
  expect_true(all(d$os[d$os_event == 0] == 200))
})

test_that("solveThreeStateModel returns an object with expected shape", {

  set.seed(9)
  ret <- solveThreeStateModel(median_pfs = 4.6, median_os = 9.6,
                              corr = c(0.5, 0.6, 0.7),
                              h12 = seq(.08, .2, length.out = 10))

  expect_s3_class(ret, "three_state_model")
  expect_named(ret, c("corr", "h01", "h02", "h12", "error"))
  expect_equal(nrow(ret), 3)
  expect_true(all(ret$h01 > 0))
  expect_true(all(ret$h02 > 0))
  expect_true(all(ret$h12 > 0))
  expect_true(all(ret$error >= 0))
  # error is |target - achieved|, should be reasonably small after grid search
  expect_true(all(ret$error < 0.1))
})

test_that("solveThreeStateModel validates inputs", {
  expect_error(solveThreeStateModel(median_pfs = c(1, 2),
                                    median_os = 9,
                                    corr = .5))
  expect_error(solveThreeStateModel(median_pfs = 4,
                                    median_os = 9,
                                    corr = 0))
  expect_error(solveThreeStateModel(median_pfs = 4,
                                    median_os = 9,
                                    corr = 1.1))
  expect_error(solveThreeStateModel(median_pfs = 4,
                                    median_os = 9,
                                    corr = .5,
                                    h12 = c(-.1, .2)))
})
