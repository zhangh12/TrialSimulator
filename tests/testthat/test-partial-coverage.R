# Targeted tests for files with partial coverage
#
# Covers:
#   - CorrelatedPfsAndOs2: generator works when copula is installed, and
#     rejects negative Kendall / non-positive medians
#   - doNothing: returns NULL, errors on extra args
#   - solveMixtureExponentialDistribution: both branches and error paths
#   - EnrollmentCountCondition: print() variants (arms, min_treatment_duration,
#     filter conditions)
#   - Condition: abstract class print() and abstract method errors;
#     & and | operator overloading wire into ConditionCombiner
#   - Endpoints: test_generator, get_name/get_type, print
#   - Arms: add_endpoints uniqueness check, get_number_endpoints, has_endpoint,
#     print


## CorrelatedPfsAndOs2 -----------------------------------------------------

test_that("CorrelatedPfsAndOs2 returns a properly named data frame", {

  skip_if_not_installed("copula")

  set.seed(9)
  d <- CorrelatedPfsAndOs2(500, median_pfs = 5, median_os = 11,
                           kendall = 0.6,
                           pfs_name = "PFS", os_name = "OS")

  expect_equal(nrow(d), 500)
  expect_named(d, c("PFS", "OS", "PFS_event", "OS_event"), ignore.order = TRUE)
  expect_true(all(d$PFS <= d$OS + 1e-10))
  expect_true(all(d$PFS_event == 1))
  expect_true(all(d$OS_event == 1))
})

test_that("CorrelatedPfsAndOs2 rejects bad inputs", {
  expect_error(CorrelatedPfsAndOs2(100, 5, 10, kendall = -0.1), "negative")
  expect_error(CorrelatedPfsAndOs2(100, -1, 10, kendall = 0.5), "Median of PFS")
  expect_error(CorrelatedPfsAndOs2(100, 5, -5, kendall = 0.5), "Median of OS")
  # infeasible Kendall for these medians
  expect_error(CorrelatedPfsAndOs2(100, 5, 10, kendall = 0.0001),
               "too small")
})


## doNothing ---------------------------------------------------------------

test_that("doNothing returns NULL and rejects extra arguments", {
  expect_null(doNothing(trial = NULL))
  expect_error(doNothing(trial = NULL, foo = 1), "should not be specified")
})


## solveMixtureExponentialDistribution -------------------------------------

test_that("solveMixtureExponentialDistribution computes median2 and overall_median", {

  # Given overall_median, compute median2
  m2 <- solveMixtureExponentialDistribution(weight1 = 0.3,
                                            median1 = 10,
                                            overall_median = 8)
  expect_named(m2, "median2")
  expect_gt(as.numeric(m2), 0)

  # Round-trip: feed median2 back, get overall_median close to the original
  om <- solveMixtureExponentialDistribution(weight1 = 0.3,
                                            median1 = 10,
                                            median2 = as.numeric(m2))
  expect_named(om, "overall_median")
  expect_equal(as.numeric(om), 8, tolerance = 1e-3)
})

test_that("solveMixtureExponentialDistribution input validation", {

  expect_error(solveMixtureExponentialDistribution(weight1 = 0.3,
                                                   median1 = 10),
               "Only one value")
  expect_error(solveMixtureExponentialDistribution(weight1 = 0.3,
                                                   median1 = 10,
                                                   median2 = 4,
                                                   overall_median = 8),
               "Only one value")
})


## EnrollmentCountCondition print ------------------------------------------

test_that("enrollment() prints a human-readable description", {

  cond <- enrollment(n = 100)
  out <- capture.output(print(cond))
  expect_true(any(grepl("Number of randomized patients >= +100", out)))

  cond2 <- enrollment(n = 100, arms = c("pbo", "trt"))
  out2 <- capture.output(print(cond2))
  expect_true(any(grepl("arms", out2)))
  expect_true(any(grepl("pbo", out2)))

  cond3 <- enrollment(n = 100, min_treatment_duration = 5)
  out3 <- capture.output(print(cond3))
  expect_true(any(grepl("treated", out3)))

  cond4 <- enrollment(n = 100, sex == "F")
  out4 <- capture.output(print(cond4))
  expect_true(any(grepl("conditions", out4)))
})

test_that("enrollment() validates n and min_treatment_duration", {
  expect_error(enrollment(n = 1.5))
  expect_error(enrollment(n = 10, min_treatment_duration = -1))
})


## Condition abstract class ------------------------------------------------

test_that("Condition abstract class exposes a generic print and errors on get_trigger_time", {

  cond <- TrialSimulator:::Condition$new()
  out <- capture.output(print(cond))
  expect_true(any(grepl("Condition object", out)))

  expect_error(cond$get_trigger_time(trial = NULL),
               "Abstract method")
})


test_that("Condition & / | operators route through ConditionCombiner", {

  c1 <- calendarTime(time = 10)
  c2 <- calendarTime(time = 20)

  combined_or  <- c1 | c2
  combined_and <- c1 & c2
  expect_s3_class(combined_or, "ConditionCombiner")
  expect_s3_class(combined_and, "ConditionCombiner")
})


## Endpoints / Arms --------------------------------------------------------

test_that("Endpoints exposes test_generator, accessors and print", {

  ep <- endpoint(name = "pfs", type = "tte",
                 generator = rexp, rate = log(2) / 5)

  expect_equal(ep$get_name(), "pfs")
  expect_equal(ep$get_type(), "tte")
  expect_null(ep$get_readout())

  d <- ep$test_generator(200)
  expect_true(nrow(d) == 200)

  out <- capture.output(print(ep))
  expect_true(length(out) > 0)
})

test_that("Endpoints name/type/readout checks fire on bad inputs", {

  # type must be tte or non-tte
  expect_error(endpoint(name = "pfs", type = "bad", generator = rexp))

  # non-tte requires readout
  expect_error(endpoint(name = "orr", type = "non-tte", generator = rnorm))

  # readout names must match endpoint names
  expect_error(endpoint(name = "orr", type = "non-tte",
                        readout = c(other = 0), generator = rnorm))
})

test_that("Arms rejects duplicate endpoints and reports counts", {

  a <- arm(name = "trt")
  expect_false(a$has_endpoint())
  expect_equal(a$get_number_endpoints(), 0)

  ep <- endpoint(name = "pfs", type = "tte", generator = rexp, rate = 0.1)
  a$add_endpoints(ep)
  expect_true(a$has_endpoint())
  expect_equal(a$get_number_endpoints(), 1)

  # same endpoint twice -> error
  expect_error(a$add_endpoints(ep), "already in the arm")

  # print exercises its own path
  out <- capture.output(print(a))
  expect_true(length(out) > 0)
})

test_that("Arms inclusion filter restricts generated data", {

  set.seed(1)
  rng <- function(n) data.frame(x = rnorm(n), sex = sample(c("M", "F"), n, replace = TRUE))
  ep <- endpoint(name = c("x", "sex"), type = c("non-tte", "non-tte"),
                 readout = c(x = 0, sex = 0), generator = rng)

  a <- arm(name = "trt", x > 0, sex == "F")
  a$add_endpoints(ep)

  d <- a$generate_data(2000)
  expect_true(all(d$x > 0))
  expect_true(all(d$sex == "F"))
})
