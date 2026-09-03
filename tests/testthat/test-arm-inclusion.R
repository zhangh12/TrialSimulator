# Inclusion criteria of arm(): generate_data() applies the conditions in
# ... as a logical mask with dplyr::filter() semantics.

incl_rng <- function(n) {
  data.frame(pfs = rexp(n, .2), os = rexp(n, .1), pfs_event = 1, os_event = 1,
             score = ifelse(runif(n) < .1, NA_real_, rnorm(n)))
}

test_that("generate_data() honors inclusion criteria and matches dplyr::filter", {

  a <- arm(name = "trt", pfs <= os, score > -1)
  a$add_endpoints(endpoint(name = c("pfs", "os", "score"),
                           type = c("tte", "tte", "baseline"),
                           generator = incl_rng))

  set.seed(11)
  d <- a$generate_data(150)
  expect_identical(nrow(d), 150L)
  expect_true(all(d$pfs <= d$os))
  expect_true(all(!is.na(d$score) & d$score > -1)) ## NA rows are dropped
  expect_identical(rownames(d), as.character(seq_len(150)))

  ## reference: the same generated rows filtered by dplyr::filter()
  set.seed(11)
  raw <- incl_rng(150)
  ref <- dplyr::filter(raw, pfs <= os, score > -1)
  expect_identical(head(d, nrow(ref))[names(ref)], ref)
})

test_that("generate_data() supports the .data pronoun in inclusion criteria", {

  a <- arm(name = "trt", .data$pfs <= .data$os)
  a$add_endpoints(endpoint(name = c("pfs", "os", "score"),
                           type = c("tte", "tte", "baseline"),
                           generator = incl_rng))
  set.seed(3)
  d <- a$generate_data(50)
  expect_true(all(d$pfs <= d$os))
})

test_that("generate_data() reports the inclusion criteria on a failing condition", {

  a <- arm(name = "trt", pfs <= os, not_a_column > 0)
  a$add_endpoints(endpoint(name = c("pfs", "os", "score"),
                           type = c("tte", "tte", "baseline"),
                           generator = incl_rng))
  expect_error(a$generate_data(20),
               "Inclusion criteria: \n\\(pfs <= os\\) & \\(not_a_column > 0\\)")

  b <- arm(name = "trt", pfs > 1e6)
  b$add_endpoints(endpoint(name = c("pfs", "os", "score"),
                           type = c("tte", "tte", "baseline"),
                           generator = incl_rng))
  expect_error(b$generate_data(20), "No data meets inclusion criteria")
})
