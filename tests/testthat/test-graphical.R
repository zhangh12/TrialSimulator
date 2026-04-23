# GraphicalTesting R6 class
#
# Covers:
#   - constructor validation (matrix shape, row sums, alpha range, lengths)
#   - accessors: get_alpha, get_weight, get_hypothesis_name, get_hid, is_in_graph,
#     has_testable_hypotheses
#   - reject_a_hypothesis propagates alpha and updates the transition matrix
#   - test() with a small 3-hypothesis Bonferroni-like graph produces correct
#     accept/reject decisions
#   - get_current_decision returns a named vector over all hypotheses
#   - reset restores initial state

make_gt <- function(alpha = c(.01, .01, .005),
                    transition = NULL,
                    asf = rep("asOF", 3),
                    max_info = c(200L, 200L, 200L),
                    hs = c("H1", "H2", "H3"),
                    silent = TRUE) {

  if(is.null(transition)){
    transition <- matrix(c(0,  .5, .5,
                           .5, 0,  .5,
                           .5, .5, 0),
                         nrow = 3, byrow = TRUE)
  }

  GraphicalTesting$new(alpha, transition, asf, max_info, hs, silent = silent)
}


test_that("GraphicalTesting initializes and exposes accessors", {

  gt <- make_gt()
  expect_equal(gt$get_number_hypotheses(), 3)
  expect_equal(gt$get_hypothesis_name(1), "H1")
  expect_equal(gt$get_hid("H2"), 2)
  expect_equal(gt$get_alpha(1), 0.01)
  expect_equal(gt$get_weight(1, 2), 0.5)
  expect_true(gt$is_in_graph(1))
  expect_true(gt$has_testable_hypotheses())
  expect_equal(gt$get_hypotheses_ids(), 1:3)
  expect_equal(gt$get_testable_hypotheses(), 1:3)
  expect_equal(gt$get_hids_not_in_graph(), integer(0))
})


test_that("GraphicalTesting validates arguments at construction", {

  good_tx <- matrix(c(0, .5, .5,
                      .5, 0, .5,
                      .5, .5, 0), nrow = 3, byrow = TRUE)

  # non-square transition
  expect_error(GraphicalTesting$new(alpha = c(.01, .01),
                                    transition = matrix(0, 2, 3),
                                    alpha_spending = rep("asOF", 2),
                                    planned_max_info = c(100L, 100L),
                                    silent = TRUE))

  # row sums not 0 or 1
  bad_rows <- good_tx
  bad_rows[1, ] <- c(0, .3, .3)
  expect_error(GraphicalTesting$new(alpha = c(.01, .01, .005),
                                    transition = bad_rows,
                                    alpha_spending = rep("asOF", 3),
                                    planned_max_info = c(100L, 100L, 100L),
                                    silent = TRUE))

  # NA in alpha
  expect_error(GraphicalTesting$new(alpha = c(NA, .01, .005),
                                    transition = good_tx,
                                    alpha_spending = rep("asOF", 3),
                                    planned_max_info = c(100L, 100L, 100L),
                                    silent = TRUE),
               "NA is not allowed in alpha")

  # NA in transition
  bad_tx <- good_tx
  bad_tx[1, 2] <- NA
  expect_error(GraphicalTesting$new(alpha = c(.01, .01, .005),
                                    transition = bad_tx,
                                    alpha_spending = rep("asOF", 3),
                                    planned_max_info = c(100L, 100L, 100L),
                                    silent = TRUE),
               "NA is not allowed in transition")

  # total alpha > 1
  expect_error(GraphicalTesting$new(alpha = c(.5, .5, .5),
                                    transition = good_tx,
                                    alpha_spending = rep("asOF", 3),
                                    planned_max_info = c(100L, 100L, 100L),
                                    silent = TRUE))

  # bad alpha spending function
  expect_error(GraphicalTesting$new(alpha = c(.01, .01, .005),
                                    transition = good_tx,
                                    alpha_spending = c("asOF", "asOF", "bad"),
                                    planned_max_info = c(100L, 100L, 100L),
                                    silent = TRUE))
})


test_that("reject_a_hypothesis transfers alpha along the graph", {

  gt <- make_gt()
  a1 <- gt$get_alpha(1)
  a2 <- gt$get_alpha(2)
  a3 <- gt$get_alpha(3)
  w12 <- gt$get_weight(1, 2)
  w13 <- gt$get_weight(1, 3)

  gt$reject_a_hypothesis("H1")

  # H1 should be out of graph and have alpha 0
  expect_false(gt$is_in_graph(1))
  expect_equal(gt$get_alpha(1), 0)

  # alpha of H2 and H3 should increase by a1 * w12 and a1 * w13, respectively
  expect_equal(gt$get_alpha(2), a2 + a1 * w12, tolerance = 1e-10)
  expect_equal(gt$get_alpha(3), a3 + a1 * w13, tolerance = 1e-10)
})


test_that("reject_a_hypothesis errors if hypothesis already out of graph", {

  gt <- make_gt()
  gt$reject_a_hypothesis("H1")
  expect_error(gt$reject_a_hypothesis("H1"),
               "not in the graph")
})


test_that("set_alpha and set_weight enforce value ranges", {
  gt <- make_gt()
  expect_error(gt$set_alpha(1, -0.1))
  expect_error(gt$set_alpha(1, 1.5))
  expect_error(gt$set_weight(1, 1, 0.5))   # diagonal must be 0
  gt$set_weight(1, 2, 0)
  expect_equal(gt$get_weight(1, 2), 0)
})


test_that("test() runs to completion on a small independent design", {

  # Three totally independent, one-stage hypotheses (no transition needed for
  # independence, but constructor requires row sums of 0 or 1 so give trivial
  # transitions).
  transition <- matrix(c(0,  1,  0,
                         0,  0,  1,
                         1,  0,  0),
                       nrow = 3, byrow = TRUE)
  gt <- make_gt(alpha = c(.01, .01, .005),
                transition = transition)

  stats <- data.frame(
    order      = c(1L, 1L, 1L),
    hypotheses = c("H1", "H2", "H3"),
    p          = c(0.0005, 0.2, 0.4),
    info       = c(200L, 200L, 200L),
    is_final   = c(TRUE, TRUE, TRUE),
    max_info   = c(200L, 200L, 200L),
    alpha_spent = c(NA_real_, NA_real_, NA_real_)
  )

  res <- suppressMessages(gt$test(stats))
  expect_s3_class(res, "data.frame")
  expect_true("hypothesis" %in% names(res))

  decisions <- gt$get_current_decision()
  expect_named(decisions, c("H1", "H2", "H3"))
  expect_equal(decisions["H1"], c(H1 = "reject"))
  expect_equal(decisions["H2"], c(H2 = "accept"))
  expect_equal(decisions["H3"], c(H3 = "accept"))

  # H1 should be out of graph after rejection; H2 inherits its alpha
  expect_false(gt$is_in_graph(1))
  expect_true(gt$is_in_graph(2))
  expect_true(gt$get_alpha(2) > 0.01 - 1e-10)  # at least the original
})


test_that("test() errors if stats lacks required columns", {
  gt <- make_gt()
  expect_error(suppressMessages(gt$test(data.frame(hypotheses = "H1",
                                                   p = 0.01,
                                                   info = 100L))),
               "missing in stats")
})


test_that("reset restores GraphicalTesting to initial state", {

  gt <- make_gt()
  a1_before <- gt$get_alpha(1)
  gt$reject_a_hypothesis("H1")
  expect_equal(gt$get_alpha(1), 0)

  suppressMessages(gt$reset())
  expect_equal(gt$get_alpha(1), a1_before)
  expect_true(gt$is_in_graph(1))
})


test_that("print respects silent=TRUE at construction", {
  gt <- make_gt()
  # silent => graph=FALSE, so no device is opened; just trajectory (NULL)
  pdf(NULL); on.exit(dev.off())
  expect_no_error(gt$print(graph = FALSE, trajectory = FALSE))
})
