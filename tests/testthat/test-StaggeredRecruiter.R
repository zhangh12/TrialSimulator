## Unit tests for StaggeredRecruiter (piecewise-constant accrual with pauses)

test_that("required columns are enforced", {
  expect_error(StaggeredRecruiter(10, data.frame(end_time = Inf)),
               "must be a data.frame with columns")
  expect_error(StaggeredRecruiter(10, data.frame(piecewise_rate = 10)),
               "must be a data.frame with columns")
  expect_error(StaggeredRecruiter(10, list(end_time = Inf, piecewise_rate = 10)),
               "must be a data.frame with columns")
})

test_that("n is validated", {
  ar <- data.frame(end_time = Inf, piecewise_rate = 10)
  expect_error(StaggeredRecruiter(0, ar), "positive integer")
  expect_error(StaggeredRecruiter(-1, ar), "positive integer")
  expect_error(StaggeredRecruiter(2.5, ar), "positive integer")
  expect_error(StaggeredRecruiter(NA, ar), "positive integer")
  expect_error(StaggeredRecruiter(c(1, 2), ar), "positive integer")
})

test_that("schedule sanity is validated", {
  expect_error(
    StaggeredRecruiter(10, data.frame(end_time = c(6, Inf),
                                      piecewise_rate = c(-1, 10))),
    "non-negative")
  expect_error(
    StaggeredRecruiter(10, data.frame(end_time = c(6, 6, Inf),
                                      piecewise_rate = c(10, 10, 10))),
    "strictly increasing")
  expect_error(
    StaggeredRecruiter(10, data.frame(end_time = c(Inf, 12),
                                      piecewise_rate = c(10, 10))),
    "must be Inf")
  ## the last window must be Inf so the schedule can supply any n
  expect_error(
    StaggeredRecruiter(10, data.frame(end_time = c(6, 12),
                                      piecewise_rate = c(10, 10))),
    "must be Inf")
  ## a pause cannot be the final, n-filling window
  expect_error(
    StaggeredRecruiter(10, data.frame(end_time = c(6, Inf),
                                      piecewise_rate = c(10, 0))),
    "must be positive")
})

test_that("first patient enrolls at time 0 when the first window is active", {
  r <- StaggeredRecruiter(50, data.frame(end_time = c(6, Inf),
                                         piecewise_rate = c(20, 20)))
  expect_equal(r[1], 0)
})

test_that("returns exactly n strictly increasing times", {
  for(N in c(1L, 30L, 200L, 600L)){
    r <- StaggeredRecruiter(N, data.frame(end_time = c(1, 2, 6, 12, Inf),
                                          piecewise_rate = c(5, 10, 15, 20, 25)))
    expect_length(r, N)
    if(N > 1L) expect_true(all(diff(r) > 0))
  }
})

test_that("enrollment is equidistant within each window (spacing = 1/rate)", {
  ar <- data.frame(end_time = c(2, 5, Inf), piecewise_rate = c(10, 4, 20))
  r <- StaggeredRecruiter(200, ar)
  ## within the first window [0, 2], spacing is 1/10
  w1 <- r[r < 2]
  expect_true(all(abs(diff(w1) - 1 / 10) < 1e-9))
})

test_that("an integer-capacity window holds exactly length * rate patients", {
  ## cumulative capacity increases by (window length * rate) across a window;
  ## when that product is an integer the window holds exactly that many (no
  ## per-window truncation). For a non-integer product the count is rounded by
  ## phase, so this exact equality only holds for integer capacity.
  ar <- data.frame(end_time = c(10, Inf), piecewise_rate = c(3, 5))
  r <- StaggeredRecruiter(100, ar)
  expect_equal(sum(r < 10), 10 * 3)   # window length 10 x rate 3 = 30
})

test_that("a zero-rate window pauses enrollment and advances calendar time", {
  ar <- data.frame(end_time = c(12, 18, Inf), piecewise_rate = c(30, 0, 30))
  r <- StaggeredRecruiter(600, ar)
  expect_equal(r[1], 0)
  expect_equal(sum(r > 12 & r < 18), 0)      # nobody enrolls during the pause
  expect_equal(min(r[r >= 12]), 18)          # enrollment resumes at the pause end
  expect_true(all(diff(r) > 0))
})

test_that("a leading pause defers first enrollment to the pause end", {
  ar <- data.frame(end_time = c(6, Inf), piecewise_rate = c(0, 30))
  r <- StaggeredRecruiter(100, ar)
  expect_false(any(r < 6))
  expect_equal(r[1], 6)
})

test_that("consecutive pause windows are supported", {
  ar <- data.frame(end_time = c(6, 12, 18, Inf),
                   piecewise_rate = c(30, 0, 0, 30))
  r <- StaggeredRecruiter(300, ar)
  expect_equal(sum(r > 6 & r < 18), 0)
  expect_equal(min(r[r >= 6]), 18)           # resumes after the two pauses
  expect_true(all(diff(r) > 0))
})

test_that("more than two consecutive leading pauses defer the first enrollment", {
  ar <- data.frame(end_time = c(3, 6, 9, Inf),
                   piecewise_rate = c(0, 0, 0, 30))
  r <- StaggeredRecruiter(100, ar)
  expect_false(any(r < 9))               # nobody before the three pauses end
  expect_equal(r[1], 9)                  # enrollment resumes at the last pause end
  expect_length(r, 100)
  expect_true(all(diff(r) > 0))
})

test_that("a positive rate too low to fit its window is rejected", {
  ## A finite window expecting < 1 patient (1/rate > width) is almost always a
  ## mistake -- historically faked as a "pause" with a tiny rate. It is now an
  ## error that points the user to piecewise_rate = 0 instead.
  ar <- data.frame(end_time = c(12, 18, Inf), piecewise_rate = c(30, 0.001, 30))
  expect_error(StaggeredRecruiter(600, ar), "too low")

  ## boundary: exactly one expected patient (width * rate == 1) is allowed
  ar_ok <- data.frame(end_time = c(12, 18, Inf), piecewise_rate = c(30, 1 / 6, 30))
  expect_silent(r <- StaggeredRecruiter(600, ar_ok))
  expect_length(r, 600)

  ## width 49 with rate 1/49: 49 * (1/49) rounds just below 1 in floating point,
  ## but it is an intended one-patient window and must NOT be rejected.
  expect_true(49 * (1 / 49) < 1)   # documents the floating-point hazard
  ar_fp <- data.frame(end_time = c(49, Inf), piecewise_rate = c(1 / 49, 10))
  expect_silent(StaggeredRecruiter(20, ar_fp))
})

test_that("a pause window (rate 0) does not trigger the low-rate error", {
  ## rate 0 is an intentional pause, not an under-specified window
  expect_silent(
    StaggeredRecruiter(50, data.frame(end_time = c(12, 18, Inf),
                                      piecewise_rate = c(30, 0, 30)))
  )
})

test_that("a small (valid) rate enrolls few patients within its window", {
  ## "Few in the window" holds when the window expects >= 1 patient, i.e.
  ## rate >= 1/W. Then the count is ~ W * rate, small for a small rate -- and
  ## far below a full-rate window of equal width.
  W <- 6
  r_small <- 0.5                       # width * rate = 3 >= 1, so accepted
  slow <- data.frame(end_time = c(12, 12 + W, Inf),
                     piecewise_rate = c(30, r_small, 30))
  e <- StaggeredRecruiter(600, slow)
  n_win <- sum(e > 12 & e <= 12 + W)

  expect_lte(n_win, ceiling(W * r_small) + 1)   # ~ W*rate = 3, small

  ## sanity: a normal-rate window of the same width enrolls many more
  full <- data.frame(end_time = c(12, 12 + W, Inf),
                     piecewise_rate = c(30, 30, 30))
  e_full <- StaggeredRecruiter(600, full)
  expect_lt(n_win, sum(e_full > 12 & e_full <= 12 + W))
})
