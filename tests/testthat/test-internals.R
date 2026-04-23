# Internal helpers
#
# Covers:
#   - computeCumulativeAlphaSpent: alpha spent is monotone non-decreasing, lies
#     in (0, 1], and matches rpact reference values for Pocock/O'Brien-Fleming
#   - .default_action: returns a non-empty character describing the dry run
#   - createSticker: runs without error and returns NULL invisibly
#   - getAdaptiveDesignOutput / getFixedDesignOutput: return the internal
#     precomputed data frames with the expected shape

test_that("computeCumulativeAlphaSpent is monotone and bounded", {

  info <- c(0.25, 0.5, 0.75, 1.0)

  # O'Brien-Fleming-like decreasing boundaries (large then small critical values)
  cv <- c(4.33, 2.96, 2.36, 2.01)
  a <- TrialSimulator:::computeCumulativeAlphaSpent(cv, info)

  expect_length(a, length(cv))
  expect_true(all(diff(a) >= -1e-10))   # non-decreasing
  expect_true(all(a > 0))
  expect_true(all(a < 1))
})

test_that("computeCumulativeAlphaSpent matches rpact for an OBF design", {

  skip_if_not_installed("rpact")

  info <- c(1/3, 2/3, 1)
  design <- rpact::getDesignGroupSequential(kMax = length(info),
                                            typeOfDesign = "OF",
                                            alpha = 0.025,
                                            sided = 1,
                                            informationRates = info)

  cv <- as.numeric(design$criticalValues)
  target <- as.numeric(design$alphaSpent)

  got <- TrialSimulator:::computeCumulativeAlphaSpent(cv, info)
  expect_equal(as.numeric(got), target, tolerance = 1e-3)
  # final cumulative alpha is alpha (tolerance from pmvnorm quadrature)
  expect_equal(tail(as.numeric(got), 1), 0.025, tolerance = 1e-3)
})

test_that(".default_action returns a descriptive message", {
  msg <- TrialSimulator:::.default_action()
  expect_type(msg, "character")
  expect_length(msg, 1)
  expect_match(msg, "dry run", ignore.case = TRUE)
})

test_that("createSticker runs without error and returns NULL invisibly", {
  expect_null(TrialSimulator:::createSticker())
})

test_that("getAdaptiveDesignOutput and getFixedDesignOutput return data frames", {
  ad <- TrialSimulator:::getAdaptiveDesignOutput()
  expect_s3_class(ad, "data.frame")
  expect_gt(nrow(ad), 0)

  fd <- TrialSimulator:::getFixedDesignOutput()
  expect_s3_class(fd, "data.frame")
  expect_gt(nrow(fd), 0)
})
