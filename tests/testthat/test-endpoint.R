# endpoint() / Endpoints$new()
#
# Covers the "baseline" type sugar handled in endpoint():
#   - "baseline" collapses to "non-tte" with readout 0 and emits a *_readout
#     column equal to 0
#   - works alongside other endpoints in a multi-endpoint generator
#   - specifying a readout for a "baseline" endpoint (even 0) errors
#   - a plain "non-tte" endpoint with readout 0 is still accepted
#   - Endpoints$new() itself does not accept "baseline"

test_that('"baseline" collapses to non-tte with readout 0', {

  set.seed(1)
  bm <- endpoint(name = 'biomarker', type = 'baseline', generator = rnorm)

  expect_equal(bm$get_type(), 'non-tte')
  expect_equal(bm$get_readout(), c(biomarker = 0))

  dat <- bm$test_generator(n = 5)
  expect_true(all(c('biomarker', 'biomarker_readout') %in% names(dat)))
  expect_equal(unique(dat$biomarker_readout), 0)
})

test_that('"baseline" works in a multi-endpoint generator with other readouts', {

  rng <- function(n){
    data.frame(sub = rbinom(n, 1, .5), orr = rbinom(n, 1, .4))
  }

  ep <- endpoint(name = c('sub', 'orr'),
                 type = c('baseline', 'non-tte'),
                 readout = c(orr = 3),
                 generator = rng)

  expect_equal(ep$get_type(), c('non-tte', 'non-tte'))
  expect_equal(ep$get_readout()[['sub']], 0)
  expect_equal(ep$get_readout()[['orr']], 3)

  dat <- ep$test_generator(n = 4)
  expect_equal(unique(dat$sub_readout), 0)
  expect_equal(unique(dat$orr_readout), 3)
})

test_that('a length-1 "baseline" type recycles over multiple names', {

  rng <- function(n){
    data.frame(age = rnorm(n), sex = rbinom(n, 1, .5))
  }

  ep <- endpoint(name = c('age', 'sex'),
                 type = 'baseline',
                 generator = rng)

  expect_equal(ep$get_type(), c('non-tte', 'non-tte'))
  expect_equal(ep$get_readout()[['age']], 0)
  expect_equal(ep$get_readout()[['sex']], 0)
})

test_that('specifying a readout for a "baseline" endpoint errors', {

  expect_error(
    endpoint(name = 'bm', type = 'baseline',
             readout = c(bm = 1), generator = rnorm),
    'must not be given a readout'
  )

  ## even a redundant readout = 0 is rejected
  expect_error(
    endpoint(name = 'bm', type = 'baseline',
             readout = c(bm = 0), generator = rnorm),
    'must not be given a readout'
  )
})

test_that('a plain non-tte endpoint with readout 0 is still accepted', {

  ep <- endpoint(name = 'x', type = 'non-tte',
                 readout = c(x = 0), generator = rnorm)

  expect_equal(ep$get_type(), 'non-tte')
  expect_equal(ep$get_readout(), c(x = 0))
})

test_that('Endpoints$new() does not accept "baseline"', {

  expect_error(
    Endpoints$new(name = 'bm', type = 'baseline', generator = rnorm),
    'can only be either "tte" or "non-tte"'
  )
})
