## trial() / Trials enforce StaggeredRecruiter as the only enroller.

make_trial <- function(...) {
  trial(name = "t", n_patients = 10, duration = 50,
        accrual_rate = data.frame(end_time = Inf, piecewise_rate = 5),
        silent = TRUE, ...)
}

test_that("enroller defaults to StaggeredRecruiter when omitted", {
  expect_error(make_trial(), NA)
})

test_that("explicitly passing StaggeredRecruiter is accepted", {
  expect_error(make_trial(enroller = StaggeredRecruiter), NA)
})

test_that("a non-StaggeredRecruiter enroller is rejected by trial()", {
  expect_error(
    trial(name = "t", n_patients = 10, duration = 50,
          enroller = rexp, rate = 0.1, silent = TRUE),
    "must be StaggeredRecruiter")

  custom <- function(n, ...) sort(stats::runif(n))
  expect_error(
    trial(name = "t", n_patients = 10, duration = 50,
          enroller = custom, silent = TRUE),
    "must be StaggeredRecruiter")
})

test_that("set_enroller() also enforces StaggeredRecruiter after construction", {
  tr <- make_trial()
  expect_error(tr$set_enroller(rexp, rate = 0.1), "must be StaggeredRecruiter")
  expect_error(
    tr$set_enroller(StaggeredRecruiter,
                    accrual_rate = data.frame(end_time = Inf, piecewise_rate = 5)),
    NA)
})
