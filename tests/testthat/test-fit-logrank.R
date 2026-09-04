## ---- fitLogrank(): survdiff-based statistic ---------------------------------
##
## Up to 1.35.7 fitLogrank() fitted two Cox models per treatment arm: one for
## the sign of the log hazard ratio and one with ties = 'exact' whose score
## test gave the log rank chi-square. The reference implementation below
## reproduces that computation. The score magnitudes agree, but under ties
## the Efron coefficient used by the old code can have the wrong sign for the
## exact log-rank score.

old_logrank_z <- function(formula, data, placebo, trt_arm){
  sub_data <- data[data$arm %in% c(placebo, trt_arm), , drop = FALSE]
  sub_data$arm <- factor(sub_data$arm, levels = c(placebo, trt_arm))
  fit_cox <- coxph(formula, data = sub_data, model = TRUE)
  lr <- coxph(formula, data = sub_data, ties = 'exact')
  sfit <- summary(fit_cox)
  coef_row <- grep(paste0('^arm', trt_arm, '$'), rownames(sfit$coef))
  sgn <- ifelse(sfit$coef[coef_row, 'coef'] > 0, 1, -1)
  mf <- model.frame(fit_cox)
  status <- model.response(mf)[, 'status']
  list(z = unname(sqrt(summary(lr)$sctest['test']) * sgn),
       info = sum(status == 1),
       info_pbo = sum(status == 1 & mf$arm %in% placebo),
       info_trt = sum(status == 1 & mf$arm %in% trt_arm),
       n_pbo = sum(mf$arm %in% placebo),
       n_trt = sum(mf$arm %in% trt_arm))
}

sim_data <- function(n, tie = FALSE, arms = c('pbo', 'trt'), n_strata = 3){
  arm <- sample(arms, n, replace = TRUE)
  rate <- c(pbo = .14, trt = .10, low = .12)[arm]
  stratum <- sample(letters[seq_len(n_strata)], n, replace = TRUE)
  t <- rexp(n, rate * ifelse(stratum == 'a', 1.5, 1))
  cens <- runif(n, 0, 15)
  d <- data.frame(arm = arm, stratum = stratum,
                  pfs = pmin(t, cens), pfs_event = as.integer(t <= cens),
                  x = rnorm(n))
  if(tie) d$pfs <- ceiling(d$pfs)  # heavy ties
  d
}

test_that('fitLogrank agrees with the exact score magnitude and uses O-E direction', {
  set.seed(20260904)
  formulas <- list(Surv(pfs, pfs_event) ~ arm,
                   Surv(pfs, pfs_event) ~ arm + strata(stratum))
  for(tie in c(FALSE, TRUE)){
    for(f in formulas){
      for(alt in c('less', 'greater')){
        d <- sim_data(300, tie)
        new <- fitLogrank(f, placebo = 'pbo', data = d, alternative = alt, tidy = FALSE)
        old <- old_logrank_z(f, d, 'pbo', 'trt')
        lr <- survdiff(f, data = d)
        obs <- if(is.matrix(lr$obs)) rowSums(lr$obs) else lr$obs
        exp <- if(is.matrix(lr$exp)) rowSums(lr$exp) else lr$exp
        expect_equal(abs(new$z), abs(old$z), tolerance = 1e-10)
        expect_equal(sign(new$z), sign(obs[2] - exp[2]))
        expect_equal(new$p,
                     if(alt == 'greater') pnorm(new$z, lower.tail = FALSE) else pnorm(new$z),
                     tolerance = 1e-10)
        expect_identical(new$info, old$info)
        expect_identical(new$info_pbo, old$info_pbo)
        expect_identical(new$info_trt, old$info_trt)
        expect_identical(new$n_pbo, old$n_pbo)
        expect_identical(new$n_trt, old$n_trt)
      }
    }
  }
})

test_that('fitLogrank handles several treatment arms and subset conditions', {
  set.seed(3)
  d <- sim_data(450, arms = c('pbo', 'trt', 'low'))
  fit <- fitLogrank(Surv(pfs, pfs_event) ~ arm, placebo = 'pbo', data = d,
                    alternative = 'less')
  expect_s3_class(fit, 'fit_logrank')
  expect_identical(fit$arm, c('low', 'trt'))  # sorted treatment arms
  expect_identical(names(fit), c('arm', 'placebo', 'p', 'info', 'z'))
  for(a in c('low', 'trt')){
    expect_equal(fit$z[fit$arm == a], old_logrank_z(Surv(pfs, pfs_event) ~ arm, d, 'pbo', a)$z,
                 tolerance = 1e-10)
  }

  ## subset conditions in ... restrict the data before the test
  fit_sub <- fitLogrank(Surv(pfs, pfs_event) ~ arm, placebo = 'pbo', data = d,
                        alternative = 'less', x > 0, arm %in% c('pbo', 'trt'))
  expect_identical(fit_sub$arm, 'trt')
  expect_equal(fit_sub$z,
               old_logrank_z(Surv(pfs, pfs_event) ~ arm, d[d$x > 0, ], 'pbo', 'trt')$z,
               tolerance = 1e-10)
})

test_that('fitLogrank returns z = 0 and p = 0.5 with a warning when the statistic is undefined', {
  d <- data.frame(arm = rep(c('pbo', 'trt'), each = 10),
                  pfs = runif(20, 1, 5), pfs_event = 0L)  # no event at all
  expect_warning(
    fit <- fitLogrank(Surv(pfs, pfs_event) ~ arm, placebo = 'pbo', data = d,
                      alternative = 'less', tidy = FALSE),
    'variance is zero')
  expect_identical(fit$z, 0)
  expect_identical(fit$p, 0.5)
  expect_identical(fit$info, 0L)
  expect_identical(fit$n_pbo, 10L)

  ## survdiff() itself errors while inverting the zero variance in this case;
  ## fitLogrank() should still apply the documented degenerate-result policy.
  all_tied <- data.frame(arm = rep(c('pbo', 'trt'), each = 2),
                         pfs = 1, pfs_event = 1L)
  expect_warning(
    tied_fit <- fitLogrank(Surv(pfs, pfs_event) ~ arm, placebo = 'pbo',
                           data = all_tied, alternative = 'less', tidy = FALSE),
    'variance is zero')
  expect_identical(tied_fit$z, 0)
  expect_identical(tied_fit$p, 0.5)
  expect_identical(tied_fit$info, 4L)
  expect_identical(tied_fit$info_pbo, 2L)
  expect_identical(tied_fit$info_trt, 2L)

  all_tied$stratum <- rep(c('a', 'b'), 2)
  expect_warning(
    stratified_fit <- fitLogrank(
      Surv(pfs, pfs_event) ~ arm + strata(stratum), placebo = 'pbo',
      data = all_tied, alternative = 'greater', tidy = FALSE),
    'variance is zero')
  expect_identical(stratified_fit$z, 0)
  expect_identical(stratified_fit$p, 0.5)
})

test_that('fitLogrank uses the score sign when ties reverse the old Cox sign', {
  d <- data.frame(
    pfs = c(2, 3, 3, 5, 5, 5),
    pfs_event = c(1, 1, 0, 1, 1, 0),
    arm = c('trt', 'trt', 'trt', 'pbo', 'pbo', 'trt')
  )

  fit <- fitLogrank(Surv(pfs, pfs_event) ~ arm, placebo = 'pbo', data = d,
                    alternative = 'greater')
  old <- old_logrank_z(Surv(pfs, pfs_event) ~ arm, d, 'pbo', 'trt')

  expect_equal(fit$z, 0.0805822964, tolerance = 1e-9)
  expect_gt(fit$z, 0)
  expect_lt(old$z, 0)
  expect_equal(fit$p, pnorm(fit$z, lower.tail = FALSE))
})

test_that('fitLogrank validates the two sides of each comparison', {
  only_pbo <- data.frame(arm = rep('pbo', 4), pfs = 1:4,
                         pfs_event = c(1L, 0L, 1L, 0L))
  expect_error(
    fitLogrank(Surv(pfs, pfs_event) ~ arm, 'pbo', only_pbo, 'less'),
    'No treatment arm')

  only_trt <- transform(only_pbo, arm = 'trt')
  expect_error(
    fitLogrank(Surv(pfs, pfs_event) ~ arm, 'pbo', only_trt, 'less'),
    'placebo arm <pbo> is not present')
})

test_that('an event-free arm is tested normally when risk sets are informative', {
  d <- data.frame(arm = c('pbo', 'pbo', 'trt', 'trt'),
                  pfs = 1:4, pfs_event = c(1L, 1L, 0L, 0L))
  expect_no_warning(
    fit <- fitLogrank(Surv(pfs, pfs_event) ~ arm, 'pbo', d, 'less',
                      tidy = FALSE))
  expect_true(is.finite(fit$z))
  expect_lt(fit$z, 0)
  expect_lt(fit$p, 0.05)
  expect_identical(fit$info_trt, 0L)
})
