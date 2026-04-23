# Farrington-Manning test
#
# Covers:
#   - return shape (arm, placebo, estimate, p, info, z) and class
#   - sign of z and p follow alternative under clear treatment benefit
#   - loops over multiple treatment arms
#   - ... filter subsets the data
#   - input validation

test_that("fitFarringtonManning returns expected shape and class", {

  set.seed(1)
  n <- 400
  data <- data.frame(
    arm = rep(c("pbo", "trt"), each = n),
    ep  = c(rbinom(n, 1, 0.20), rbinom(n, 1, 0.40))
  )
  res <- fitFarringtonManning(endpoint = "ep", placebo = "pbo",
                              data = data, alternative = "greater")

  expect_s3_class(res, "fit_fm")
  expect_named(res, c("arm", "placebo", "estimate", "p", "info", "z"))
  expect_equal(nrow(res), 1)
  expect_equal(res$arm, "trt")
  expect_equal(res$placebo, "pbo")
  expect_equal(res$info, 2 * n)
  # estimate is p1 - p2; under treatment benefit it should be positive
  expect_gt(res$estimate, 0)
  expect_gt(res$z, 0)
  expect_lt(res$p, 0.05)
})

test_that("fitFarringtonManning respects alternative='less'", {

  set.seed(2)
  n <- 400
  data <- data.frame(
    arm = rep(c("pbo", "trt"), each = n),
    ep  = c(rbinom(n, 1, 0.40), rbinom(n, 1, 0.20))
  )
  res <- fitFarringtonManning(endpoint = "ep", placebo = "pbo",
                              data = data, alternative = "less")
  expect_lt(res$estimate, 0)
  expect_lt(res$z, 0)
  expect_lt(res$p, 0.05)
})

test_that("fitFarringtonManning fits one model per non-placebo arm", {

  set.seed(3)
  n <- 300
  data <- data.frame(
    arm = rep(c("pbo", "low", "high"), each = n),
    ep  = c(rbinom(n, 1, 0.20),
            rbinom(n, 1, 0.30),
            rbinom(n, 1, 0.45))
  )
  res <- fitFarringtonManning(endpoint = "ep", placebo = "pbo",
                              data = data, alternative = "greater")
  expect_equal(sort(res$arm), sort(c("high", "low")))
  expect_true(all(res$placebo == "pbo"))
  # high arm should give a stronger effect (smaller p, larger estimate)
  hi <- res[res$arm == "high", ]
  lo <- res[res$arm == "low", ]
  expect_gt(hi$estimate, lo$estimate)
})

test_that("fitFarringtonManning ... filter restricts the sample", {

  set.seed(4)
  n <- 300
  data <- data.frame(
    arm    = rep(c("pbo", "trt"), each = n),
    region = sample(c("A", "B"), 2 * n, replace = TRUE),
    ep     = c(rbinom(n, 1, 0.2), rbinom(n, 1, 0.4))
  )
  full <- fitFarringtonManning("ep", "pbo", data, "greater")
  sub  <- fitFarringtonManning("ep", "pbo", data, "greater", region == "A")
  expect_lt(sub$info, full$info)
  expect_equal(sub$info, sum(data$region == "A"))
})

test_that("fitFarringtonManning input validation", {

  d <- data.frame(arm = c("pbo", "trt"), ep = c(1, 0))
  expect_error(fitFarringtonManning(endpoint = c("ep", "ep2"),
                                    placebo = "pbo", data = d,
                                    alternative = "greater"),
               "single character")
  expect_error(fitFarringtonManning("ep", c("pbo", "x"), d, "greater"),
               "single character")
  expect_error(fitFarringtonManning("ep", "pbo", list(), "greater"),
               "data frame")
  expect_error(fitFarringtonManning("ep", "pbo", d, "two-sided"),
               regexp = NULL)
  expect_error(fitFarringtonManning("missing", "pbo", d, "greater"),
               "Columns")
  expect_error(fitFarringtonManning("ep", "pbo", d, "greater", delta = 2),
               "delta")
  expect_error(fitFarringtonManning("ep", "pbo", d, "greater",
                                    nonexistent_var > 0),
               "Error in filtering")
})

test_that("fitFarringtonManning errors when filtered data is empty", {

  d <- data.frame(arm = rep(c("pbo", "trt"), each = 10),
                  ep  = rbinom(20, 1, .5),
                  flag = 0)
  expect_error(fitFarringtonManning("ep", "pbo", d, "greater", flag == 1),
               "No data remaining")
})
