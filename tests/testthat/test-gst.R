# GroupSequentialTest R6 class
#
# Covers:
#   - constructor and accessors
#   - test() with only observed_info returns critical values / boundaries
#   - test() with p-values populates decisions and stage trajectory
#   - calling test() after completion errors; reset() restores state
#   - input validation (NA info, wrong length, non-whole numbers)
#   - asUser branch: alpha_spent length/monotonicity/final-sum checks

make_gst <- function(alpha = 0.025, spending = "asOF", max_info = 400L,
                     silent = TRUE) {
  GroupSequentialTest$new(alpha = alpha, alpha_spending = spending,
                          planned_max_info = max_info, silent = silent)
}


test_that("GroupSequentialTest accessors reflect initialization", {

  gst <- make_gst(alpha = 0.025, spending = "asOF", max_info = 400)
  expect_equal(gst$get_alpha(), 0.025)
  expect_equal(gst$get_alpha_spending(), "asOF")
  expect_equal(gst$get_max_info(), 400)
  expect_equal(gst$get_stage(), 1)
  expect_equal(gst$get_name(), "H0")
})


test_that("GroupSequentialTest computes boundaries without p-values", {

  skip_if_not_installed("rpact")

  gst <- make_gst(alpha = 0.025, spending = "asOF", max_info = 393)
  gst$test(observed_info = c(205L, 285L, 393L),
           is_final = c(FALSE, FALSE, TRUE))

  tr <- gst$get_trajectory()
  expect_equal(nrow(tr), 3)
  expect_named(tr, c("typeOfDesign", "stages", "informationRates",
                     "alpha", "sided", "alphaSpent", "criticalValues",
                     "stageLevels", "obs_p_value", "decision", "hypothesis"))
  # no p-values -> decision is NA or accept; critical values strictly decreasing
  expect_true(all(diff(tr$criticalValues) < 0))
  expect_true(all(is.na(tr$obs_p_value)))
})


test_that("GroupSequentialTest resets and then accepts staged p-values", {

  gst <- make_gst(alpha = 0.025, spending = "asOF", max_info = 393)
  gst$test(c(205L, 285L, 393L), c(FALSE, FALSE, TRUE))

  # second call errors until reset
  expect_error(gst$test(observed_info = 500L, is_final = FALSE),
               "completed")

  suppressMessages(gst$reset())
  expect_equal(gst$get_stage(), 1)

  gst$test(observed_info = c(205L, 285L, 393L),
           is_final = c(FALSE, FALSE, TRUE),
           p_values = c(0.09, 0.006, 0.002))

  tr <- gst$get_trajectory()
  expect_equal(tr$obs_p_value, c(0.09, 0.006, 0.002))
  expect_true(all(tr$decision %in% c("reject", "accept")))
  # last p-value is small enough to reject under OF at the final stage
  expect_equal(tail(tr$decision, 1), "reject")
})


test_that("GroupSequentialTest validates test() inputs", {

  gst <- make_gst()
  expect_error(gst$test(observed_info = NA_integer_, is_final = FALSE),
               "No NA is allowed in observed_info")

  gst <- make_gst()
  expect_error(gst$test(observed_info = 1.5, is_final = FALSE),
               "positive integers")

  gst <- make_gst()
  expect_error(gst$test(observed_info = c(100L, 200L), is_final = FALSE),
               "same length")

  gst <- make_gst()
  expect_error(gst$test(observed_info = 100L, is_final = NA),
               "No NA is allowed")

  gst <- make_gst(max_info = 100L)
  expect_error(gst$test(observed_info = 150L, is_final = FALSE),
               "greater than planned_max_info")

  gst <- make_gst()
  expect_error(gst$test(observed_info = c(100L, 200L),
                        is_final = c(FALSE, TRUE),
                        p_values = 0.1),
               "same length")
})


test_that("GroupSequentialTest 'asUser' requires consistent alpha_spent", {

  # asUser without alpha_spent -> error
  gst <- make_gst(spending = "asUser", max_info = 100L)
  expect_error(gst$test(observed_info = c(40L, 100L),
                        is_final = c(FALSE, TRUE),
                        p_values = c(.2, .02)),
               "alpha_spent cannot be NULL")

  # asOF with alpha_spent -> error
  gst <- make_gst(spending = "asOF", max_info = 100L)
  expect_error(gst$test(observed_info = c(40L, 100L),
                        is_final = c(FALSE, TRUE),
                        p_values = c(.2, .02),
                        alpha_spent = c(.01, .025)),
               "alpha_spent should not be specified")

  gst <- make_gst(spending = "asUser", max_info = 100L)
  # non-monotone alpha_spent
  expect_error(gst$test(observed_info = c(40L, 100L),
                        is_final = c(FALSE, TRUE),
                        p_values = c(.2, .02),
                        alpha_spent = c(.02, .01)),
               "monotonically increasing")

  gst <- make_gst(spending = "asUser", max_info = 100L)
  # out-of-range alpha_spent
  expect_error(gst$test(observed_info = c(40L, 100L),
                        is_final = c(FALSE, TRUE),
                        p_values = c(.2, .02),
                        alpha_spent = c(.01, .5)),
               "values between 0")

  # wrong length
  gst <- make_gst(spending = "asUser", max_info = 100L)
  expect_error(gst$test(observed_info = c(40L, 100L),
                        is_final = c(FALSE, TRUE),
                        p_values = c(.2, .02),
                        alpha_spent = 0.01),
               "same length")

  # final alpha_spent mismatches alpha
  gst <- make_gst(alpha = 0.025, spending = "asUser", max_info = 100L)
  expect_error(gst$test(observed_info = c(40L, 100L),
                        is_final = c(FALSE, TRUE),
                        p_values = c(.2, .02),
                        alpha_spent = c(.01, .02)),
               "accumulated alpha_spent")
})


test_that("GroupSequentialTest constructor validates alpha and planned_max_info", {
  expect_error(GroupSequentialTest$new(alpha = 1.5,
                                       planned_max_info = 100L))
  expect_error(GroupSequentialTest$new(alpha = 0.025,
                                       planned_max_info = 100.5))
  expect_error(GroupSequentialTest$new(alpha = 0.025,
                                       planned_max_info = 100L,
                                       alpha_spending = "bad"))
})


test_that("GroupSequentialTest asUser succeeds with valid alpha_spent", {

  alpha <- 0.025
  gst <- make_gst(alpha = alpha, spending = "asUser", max_info = 387L)
  gst$test(observed_info = c(205L, 285L, 387L),
           is_final = c(FALSE, FALSE, TRUE),
           alpha_spent = c(.005, .0125, alpha))
  tr <- gst$get_trajectory()
  expect_equal(nrow(tr), 3)
  expect_true(all(tr$alphaSpent >= 0))
})


test_that("GroupSequentialTest can adjust planned_max_info at final stage", {

  gst <- make_gst(alpha = 0.025, spending = "asOF", max_info = 393L)
  # over-running trial: final observed_info exceeds planned
  gst$test(observed_info = c(205L, 285L, 420L),
           is_final = c(FALSE, FALSE, TRUE),
           p_values = c(0.1, 0.01, 0.001))
  expect_equal(gst$get_max_info(), 420L)
  tr <- gst$get_trajectory()
  expect_equal(nrow(tr), 3)
})
