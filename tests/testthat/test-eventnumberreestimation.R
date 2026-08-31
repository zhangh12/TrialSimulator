# Event number re-estimation from conditional power
#
# Covers: the internal solver .event_number_reestimation(), verified by the
# crossing property against the oracle-validated .conditional_power() (the
# returned D is the smallest whole number with CP >= target, including when
# CP is nonmonotone) and against rpact::getConditionalPower directly; cap
# semantics; and the method
# Trials$eventNumberReestimationFromConditionalPower() (wiring to locked
# data, alpha/target_cp/D_cap shape rules, boundary-crossed and unbounded
# paths).

solver <- TrialSimulator:::.event_number_reestimation
cp_internal <- TrialSimulator:::.conditional_power

## conditional power from rpact for a design spending no alpha at interim;
## same helper as in test-conditionalpower.R
rpact_cp <- function(z, d, D, alpha, thetaH1 = NULL,
                     allocationRatioPlanned = 1, directionUpper = FALSE){
  design <- suppressMessages(rpact::getDesignGroupSequential(
    kMax = 2, alpha = alpha, sided = 1,
    informationRates = c(d / D, 1),
    typeOfDesign = 'asUser',
    userAlphaSpending = c(0, alpha)))
  dat <- rpact::getDataset(events = d, logRanks = z)
  sr <- rpact::getStageResults(design, dataInput = dat,
                               directionUpper = directionUpper)
  args <- list(sr, nPlanned = D - d,
               allocationRatioPlanned = allocationRatioPlanned)
  if(!is.null(thetaH1)){
    args$thetaH1 <- thetaH1
  }
  res <- suppressMessages(do.call(rpact::getConditionalPower, args))
  res$conditionalPower[2]
}


test_that(".event_number_reestimation returns the smallest D reaching the target", {

  grid <- expand.grid(z = c(-0.5, -1.2, -1.8),
                      d = c(80, 200),
                      alpha = c(0.01, 0.025),
                      target = c(0.5, 0.8, 0.95))

  for(effect in list('trend', 0.75)){
    omega <- if(is.numeric(effect)) 0.25 else NA
    for(i in seq_len(nrow(grid))){
      z <- grid$z[i]; d <- grid$d[i]
      alpha <- grid$alpha[i]; target <- grid$target[i]

      D <- solver(z = z, d = d, alpha = alpha, target_cp = target,
                  D_cap = Inf, effect = effect, omega = omega,
                  alternative = 'less')

      expect_true(D > d)
      expect_true(TrialSimulator:::is.wholenumber(D))
      expect_gte(cp_internal(z, d, D, alpha, effect, omega, 'less'), target)
      if(D > d + 1){
        expect_lt(cp_internal(z, d, D - 1, alpha, effect, omega, 'less'),
                  target)
      }
    }
  }

  ## Crossing the final boundary at interim does not permit D = d. If the
  ## crossing is slight, CP can fall below the target at d + 1 and the first
  ## future event number reaching the target can be substantially later.
  crossed_z <- qnorm(0.025) - 0.001
  crossed_D <- solver(z = crossed_z, d = 100, alpha = 0.025,
                      target_cp = 0.9, D_cap = Inf, effect = 'trend',
                      omega = NA, alternative = 'less')
  expect_equal(crossed_D, 220)
  expect_lt(cp_internal(crossed_z, 100, 101, 0.025, 'trend', NA, 'less'),
            0.9)
  expect_gte(cp_internal(crossed_z, 100, crossed_D, 0.025, 'trend', NA,
                         'less'), 0.9)
  expect_lt(cp_internal(crossed_z, 100, crossed_D - 1, 0.025, 'trend', NA,
                        'less'), 0.9)

  ## Boundary-crossed and ongoing comparisons can be solved together.
  mixed <- solver(z = c(crossed_z, -1.2), d = c(100, 100),
                  alpha = c(0.025, 0.025), target_cp = c(0.9, 0.9),
                  D_cap = c(Inf, Inf), effect = 'trend', omega = NA,
                  alternative = 'less')
  expect_equal(mixed[1], crossed_D)
  expect_equal(
    mixed[2],
    solver(z = -1.2, d = 100, alpha = 0.025, target_cp = 0.9,
           D_cap = Inf, effect = 'trend', omega = NA,
           alternative = 'less')
  )
})


test_that(".event_number_reestimation finds the first of multiple crossings", {

  ## A weak beneficial fixed effect can make CP rise above the target near
  ## the interim, fall below it, and recover much later. The three continuous
  ## crossings are around D = 107, 183 and 8675; the rule selects the first.
  z <- -1.8; d <- 100; alpha <- 0.025; target <- 0.2
  effect <- 0.98; omega <- 0.25

  cp_at <- function(D){
    cp_internal(z, d, D, alpha, effect, omega, 'less')
  }
  brute_Ds <- (d + 1):10000
  first <- brute_Ds[which(cp_at(brute_Ds) >= target)[1]]
  expect_equal(first, 107)

  ## Infinite and finite searches must both retain the early crossing, even
  ## though CP at the finite cap is below the target.
  expect_equal(
    solver(z, d, alpha, target, D_cap = Inf, effect = effect,
           omega = omega, alternative = 'less'),
    first
  )
  expect_lt(cp_at(5000), target)
  expect_equal(
    solver(z, d, alpha, target, D_cap = 5000, effect = effect,
           omega = omega, alternative = 'less'),
    first
  )

  expect_lt(cp_at(first - 1), target)
  expect_gte(cp_at(first), target)

  ## Direction mirroring preserves every crossing and hence the earliest one.
  expect_equal(
    solver(-z, d, alpha, target, D_cap = Inf, effect = 1 / effect,
           omega = omega, alternative = 'greater'),
    first
  )
})


test_that(".event_number_reestimation crossing agrees with rpact", {

  ## the trend of a favorable interim
  z <- -1.2; d <- 100; alpha <- 0.025; target <- 0.9
  D <- solver(z = z, d = d, alpha = alpha, target_cp = target, D_cap = Inf,
              effect = 'trend', omega = NA, alternative = 'less')
  expect_gte(rpact_cp(z, d, D, alpha), target)
  expect_lt(rpact_cp(z, d, D - 1, alpha), target)

  ## a fixed hazard ratio under 2:1 allocation (omega = 2 / 9)
  D <- solver(z = z, d = d, alpha = alpha, target_cp = target, D_cap = Inf,
              effect = 0.7, omega = 2 / 9, alternative = 'less')
  expect_gte(rpact_cp(z, d, D, alpha, thetaH1 = 0.7,
                      allocationRatioPlanned = 2), target)
  expect_lt(rpact_cp(z, d, D - 1, alpha, thetaH1 = 0.7,
                     allocationRatioPlanned = 2), target)
})


test_that(".event_number_reestimation mirrors between 'less' and 'greater'", {

  d <- 150; alpha <- 0.023; target <- 0.85

  expect_equal(
    solver(z = -1.4, d = d, alpha = alpha, target_cp = target, D_cap = Inf,
           effect = 'trend', omega = NA, alternative = 'less'),
    solver(z = 1.4, d = d, alpha = alpha, target_cp = target, D_cap = Inf,
           effect = 'trend', omega = NA, alternative = 'greater')
  )

  expect_equal(
    solver(z = -1.4, d = d, alpha = alpha, target_cp = target, D_cap = Inf,
           effect = 0.75, omega = 0.25, alternative = 'less'),
    solver(z = 1.4, d = d, alpha = alpha, target_cp = target, D_cap = Inf,
           effect = 1 / 0.75, omega = 0.25, alternative = 'greater')
  )
})


test_that(".event_number_reestimation is monotone in the target", {

  z <- -1.2; d <- 100; alpha <- 0.025
  targets <- c(0.3, 0.5, 0.7, 0.9, 0.99)
  Ds <- sapply(targets, function(target){
    solver(z = z, d = d, alpha = alpha, target_cp = target, D_cap = Inf,
           effect = 'trend', omega = NA, alternative = 'less')
  })
  expect_true(all(diff(Ds) >= 0))
})


test_that(".event_number_reestimation respects the cap", {

  z <- -1.2; d <- 100; alpha <- 0.025; target <- 0.9

  D0 <- solver(z = z, d = d, alpha = alpha, target_cp = target, D_cap = Inf,
               effect = 'trend', omega = NA, alternative = 'less')

  ## a cap at or above the solution does not change it
  expect_equal(
    solver(z, d, alpha, target, D_cap = D0, effect = 'trend', omega = NA,
           alternative = 'less'),
    D0
  )
  expect_equal(
    solver(z, d, alpha, target, D_cap = D0 + 50, effect = 'trend',
           omega = NA, alternative = 'less'),
    D0
  )

  ## no solution is returned when the cap is below the first crossing
  expect_true(is.na(
    solver(z, d, alpha, target, D_cap = D0 - 1, effect = 'trend',
           omega = NA, alternative = 'less')
  ))
  expect_lt(cp_internal(z, d, D0 - 1, alpha, 'trend', NA, 'less'), target)

  ## The cap also applies when the interim z has already crossed the final
  ## boundary but no future event number through the cap reaches the target.
  crossed_z <- qnorm(alpha) - 0.001
  expect_true(is.na(
    solver(crossed_z, d, alpha, target, D_cap = 150, effect = 'trend',
           omega = NA, alternative = 'less')
  ))

  ## A non-beneficial drift with no early crossing has no finite solution,
  ## with or without a finite cap.
  expect_true(is.na(
    solver(z = 0.5, d = d, alpha = alpha, target_cp = target, D_cap = Inf,
           effect = 'trend', omega = NA, alternative = 'less')
  ))
  expect_true(is.na(
    solver(z = 0.5, d = d, alpha = alpha, target_cp = target, D_cap = 500,
           effect = 'trend', omega = NA, alternative = 'less')
  ))

  ## Conversely, an adverse assumed effect may still have an early solution
  ## supported by a strong interim result; the unbounded search must retain it.
  expect_equal(
    solver(z = -3, d = d, alpha = alpha, target_cp = target, D_cap = Inf,
           effect = 1.1, omega = 0.25, alternative = 'less'),
    d + 1
  )

  ## With zero drift, CP tends to alpha. An unbounded search still has a
  ## finite solution when the target is below that limit.
  zero_drift_D <- solver(z = 0.5, d = d, alpha = 0.4, target_cp = 0.3,
                         D_cap = Inf, effect = 1, omega = 0.25,
                         alternative = 'less')
  expect_equal(zero_drift_D, 525)
  expect_gte(cp_internal(0.5, d, zero_drift_D, 0.4, 1, 0.25, 'less'), 0.3)
  expect_lt(cp_internal(0.5, d, zero_drift_D - 1, 0.4, 1, 0.25, 'less'),
            0.3)
})


test_that(".event_number_reestimation is vectorized over comparisons", {

  z <- c(-1.2, -0.8); d <- c(100, 150); alpha <- c(0.025, 0.01)
  target <- c(0.9, 0.8); D_cap <- c(Inf, 2000); omega <- c(0.25, 2 / 9)

  vec <- solver(z = z, d = d, alpha = alpha, target_cp = target,
                D_cap = D_cap, effect = 0.75, omega = omega,
                alternative = 'less')
  one <- sapply(1:2, function(i){
    solver(z = z[i], d = d[i], alpha = alpha[i], target_cp = target[i],
           D_cap = D_cap[i], effect = 0.75, omega = omega[i],
           alternative = 'less')
  })
  expect_equal(vec, one)
})


## ---- the method: wiring, shapes and error paths ---------------------------

make_arm <- function(name, median) {
  ep <- endpoint(name = 'pfs', type = 'tte', generator = rexp,
                 rate = log(2) / median)
  a <- arm(name = name)
  a$add_endpoints(ep)
  a
}

make_trial <- function(seed = 31416, n_patients = 400, duration = 40) {
  accrual <- data.frame(end_time = Inf, piecewise_rate = 30)
  trial(name = "t", n_patients = n_patients, duration = duration, seed = seed,
        enroller = StaggeredRecruiter, accrual_rate = accrual,
        dropout = rweibull, shape = 1, scale = 1e6,
        silent = TRUE)
}

run_two_arm_trial <- function(sample_ratio = c(1, 1), seed = 31416) {
  pbo <- make_arm("pbo", 10)
  trt <- make_arm("trt", 14)
  tr <- make_trial(seed = seed)
  add_arms(tr, sample_ratio = sample_ratio, pbo, trt)

  lstn <- listener(silent = TRUE)
  lstn$add_milestones(
    milestone(name = "interim", when = calendarTime(time = 15)),
    milestone(name = "final", when = calendarTime(time = 40))
  )
  controller(tr, lstn)$run(n = 1, silent = TRUE, plot_event = FALSE)
  tr
}

run_three_arm_trial <- function(seed = 27183) {
  pbo <- make_arm("pbo", 10)
  trt1 <- make_arm("trt1", 12)
  trt2 <- make_arm("trt2", 16)
  tr <- make_trial(seed = seed)
  add_arms(tr, sample_ratio = c(1, 1, 1), pbo, trt1, trt2)

  lstn <- listener(silent = TRUE)
  lstn$add_milestones(
    milestone(name = "interim", when = calendarTime(time = 15)),
    milestone(name = "final", when = calendarTime(time = 40))
  )
  controller(tr, lstn)$run(n = 1, silent = TRUE, plot_event = FALSE)
  tr
}


test_that("the method extracts z and d and inverts conditionalPower (two arms)", {

  tr <- run_two_arm_trial()

  fit <- fitLogrank(Surv(pfs, pfs_event) ~ arm, 'pbo',
                    tr$get_locked_data('interim'), 'less', tidy = FALSE)

  res <- tr$eventNumberReestimationFromConditionalPower(
    'interim', Surv(pfs, pfs_event) ~ arm,
    placebo = 'pbo', alternative = 'less',
    alpha = 0.022, target_cp = 0.9, effect = 'trend')

  expect_s3_class(res, 'data.frame')
  expect_equal(nrow(res), 1)
  expect_named(res, c('arm', 'placebo', 'z', 'd', 'D', 'D_cap', 'alpha',
                      'effect', 'target_cp', 'achieved_cp',
                      'target_reached'))
  expect_equal(res$arm, 'trt')
  expect_equal(res$placebo, 'pbo')
  expect_equal(res$z, fit$z)
  expect_equal(res$d, fit$info)
  expect_equal(res$D_cap, Inf)
  expect_true(res$target_reached)

  ## The NULL default and an explicit Inf request the same unbounded search.
  res_null <- tr$eventNumberReestimationFromConditionalPower(
    'interim', Surv(pfs, pfs_event) ~ arm,
    placebo = 'pbo', alternative = 'less',
    alpha = 0.022, target_cp = 0.9, effect = 'trend', D_cap = NULL)
  res_inf <- tr$eventNumberReestimationFromConditionalPower(
    'interim', Surv(pfs, pfs_event) ~ arm,
    placebo = 'pbo', alternative = 'less',
    alpha = 0.022, target_cp = 0.9, effect = 'trend', D_cap = Inf)
  expect_equal(res_null, res)
  expect_equal(res_inf, res)

  ## the returned D inverts conditionalPower(): CP(D) >= target > CP(D - 1)
  cp_at <- function(D){
    tr$conditionalPower('interim', Surv(pfs, pfs_event) ~ arm,
                        placebo = 'pbo', alternative = 'less',
                        alpha = 0.022, D = D, effect = 'trend')$cp
  }
  expect_equal(res$achieved_cp, cp_at(res$D))
  expect_gte(res$achieved_cp, 0.9)
  expect_lt(cp_at(res$D - 1), 0.9)
})


test_that("a finite cap below the solution returns no solution", {

  tr <- run_two_arm_trial()

  res0 <- tr$eventNumberReestimationFromConditionalPower(
    'interim', Surv(pfs, pfs_event) ~ arm,
    placebo = 'pbo', alternative = 'less',
    alpha = 0.022, target_cp = 0.9, effect = 'trend')

  res <- tr$eventNumberReestimationFromConditionalPower(
    'interim', Surv(pfs, pfs_event) ~ arm,
    placebo = 'pbo', alternative = 'less',
    alpha = 0.022, target_cp = 0.9, effect = 'trend',
    D_cap = res0$D - 10)

  expect_true(is.na(res$D))
  expect_equal(res$D_cap, res0$D - 10)
  expect_true(is.na(res$achieved_cp))
  expect_equal(res$target_cp, 0.9)
  expect_false(res$target_reached)

  ## A cap above the solution remains unchanged in D_cap; D contains the
  ## solution rather than the cap.
  res2 <- tr$eventNumberReestimationFromConditionalPower(
    'interim', Surv(pfs, pfs_event) ~ arm,
    placebo = 'pbo', alternative = 'less',
    alpha = 0.022, target_cp = 0.9, effect = 'trend', D_cap = res0$D + 10)
  expect_equal(res2$D, res0$D)
  expect_equal(res2$D_cap, res0$D + 10)
  expect_true(res2$target_reached)
})


test_that("'less' vs placebo and 'greater' vs the swapped reference agree", {

  tr <- run_two_arm_trial()

  res_less <- tr$eventNumberReestimationFromConditionalPower(
    'interim', Surv(pfs, pfs_event) ~ arm,
    placebo = 'pbo', alternative = 'less',
    alpha = 0.022, target_cp = 0.9, effect = 'trend')

  ## swapping the reference arm negates z; the logrank comparison and the
  ## re-estimated event number are unchanged
  res_greater <- tr$eventNumberReestimationFromConditionalPower(
    'interim', Surv(pfs, pfs_event) ~ arm,
    placebo = 'trt', alternative = 'greater',
    alpha = 0.022, target_cp = 0.9, effect = 'trend')

  expect_equal(res_greater$z, -res_less$z)
  expect_equal(res_greater$d, res_less$d)
  expect_equal(res_greater$D, res_less$D)
  expect_equal(res_greater$achieved_cp, res_less$achieved_cp)
})


test_that("a numeric effect uses the allocation ratio recorded at the milestone", {

  tr <- run_two_arm_trial(sample_ratio = c(1, 2))

  fit <- fitLogrank(Surv(pfs, pfs_event) ~ arm, 'pbo',
                    tr$get_locked_data('interim'), 'less', tidy = FALSE)

  res <- tr$eventNumberReestimationFromConditionalPower(
    'interim', Surv(pfs, pfs_event) ~ arm,
    placebo = 'pbo', alternative = 'less',
    alpha = 0.022, target_cp = 0.9, effect = 0.75)

  ## r = 2 for trt vs pbo, omega = 2 / 9
  expect_equal(
    res$D,
    solver(z = fit$z, d = fit$info, alpha = 0.022, target_cp = 0.9,
           D_cap = Inf, effect = 0.75, omega = 2 / 9,
           alternative = 'less')
  )
})


test_that("multiple arms follow the named-vector rules of conditionalPower", {

  tr <- run_three_arm_trial()

  ## the interim z of both arms is around -2 to -2.6 (seed 27183), so use
  ## strict boundaries that are not yet crossed
  res <- tr$eventNumberReestimationFromConditionalPower(
    'interim', Surv(pfs, pfs_event) ~ arm,
    placebo = 'pbo', alternative = 'less',
    alpha = c(trt1 = 0.005, trt2 = 0.002),
    target_cp = c(trt2 = 0.85, trt1 = 0.9),
    effect = 'trend')

  expect_equal(nrow(res), 2)
  expect_equal(res$arm, c('trt1', 'trt2'))
  ## matched by name, not position
  expect_equal(res$target_cp, c(0.9, 0.85))
  expect_equal(res$alpha, c(0.005, 0.002))
  expect_equal(res$D_cap, c(Inf, Inf))

  ## a subset of arms restricts the comparisons
  res_sub <- tr$eventNumberReestimationFromConditionalPower(
    'interim', Surv(pfs, pfs_event) ~ arm,
    placebo = 'pbo', alternative = 'less',
    alpha = c(trt1 = 0.005), target_cp = c(trt1 = 0.9),
    effect = 'trend')
  expect_equal(nrow(res_sub), 1)
  expect_equal(res_sub$D, res$D[res$arm == 'trt1'])

  ## per-arm caps, with Inf entries for uncapped comparisons
  res_cap <- tr$eventNumberReestimationFromConditionalPower(
    'interim', Surv(pfs, pfs_event) ~ arm,
    placebo = 'pbo', alternative = 'less',
    alpha = c(trt1 = 0.005, trt2 = 0.002),
    target_cp = c(trt1 = 0.9, trt2 = 0.85),
    effect = 'trend',
    D_cap = c(trt2 = 10000, trt1 = res$D[1] - 10))
  expect_equal(res_cap$D_cap, c(res$D[1] - 10, 10000))
  expect_true(is.na(res_cap$D[1]))
  expect_true(is.na(res_cap$achieved_cp[1]))
  expect_false(res_cap$target_reached[1])
  expect_equal(res_cap$D[2], res$D[2])
  expect_true(res_cap$target_reached[2])
})


test_that("the method validates alpha, target_cp and D_cap shapes", {

  tr <- run_three_arm_trial()
  frm <- Surv(pfs, pfs_event) ~ arm

  enr <- function(...){
    tr$eventNumberReestimationFromConditionalPower(
      'interim', frm, 'pbo', 'less', ...)
  }

  ## unnamed scalars with more than one available arm
  expect_error(enr(alpha = 0.025, target_cp = 0.9, effect = 'trend'),
               'unnamed scalars')

  ## length mismatch
  expect_error(enr(alpha = c(trt1 = 0.025, trt2 = 0.02),
                   target_cp = c(trt1 = 0.9), effect = 'trend'),
               'same length')

  ## one named, one not
  expect_error(enr(alpha = c(trt1 = 0.025, trt2 = 0.02),
                   target_cp = c(0.9, 0.85), effect = 'trend'),
               'both named')

  ## different name sets
  expect_error(enr(alpha = c(trt1 = 0.025, pbo = 0.02),
                   target_cp = c(trt1 = 0.9, trt2 = 0.85),
                   effect = 'trend'),
               'same set of treatment arms')

  ## nonexistent arm
  expect_error(enr(alpha = c(trt3 = 0.025), target_cp = c(trt3 = 0.9),
                   effect = 'trend'),
               'not among the treatment arms available')

  ## duplicated names
  expect_error(enr(alpha = c(trt1 = 0.025, trt1 = 0.02),
                   target_cp = c(trt1 = 0.9, trt2 = 0.85),
                   effect = 'trend'),
               'Duplicated')

  ## invalid target_cp values
  expect_error(enr(alpha = c(trt1 = 0.025), target_cp = c(trt1 = 1.2),
                   effect = 'trend'),
               'in \\(0, 1\\)')

  ## D_cap: finite unnamed scalar with more than one comparison
  expect_error(enr(alpha = c(trt1 = 0.025, trt2 = 0.02),
                   target_cp = c(trt1 = 0.9, trt2 = 0.85),
                   effect = 'trend', D_cap = 600),
               'scalar Inf')

  ## D_cap: wrong name set
  expect_error(enr(alpha = c(trt1 = 0.025, trt2 = 0.02),
                   target_cp = c(trt1 = 0.9, trt2 = 0.85),
                   effect = 'trend', D_cap = c(trt1 = 600, trt3 = 700)),
               'same set of treatment arms as alpha')

  ## D_cap: not whole numbers
  expect_error(enr(alpha = c(trt1 = 0.025), target_cp = c(trt1 = 0.9),
                   effect = 'trend', D_cap = c(trt1 = 600.5)),
               'whole number')

  ## D_cap: negative or NA
  expect_error(enr(alpha = c(trt1 = 0.025), target_cp = c(trt1 = 0.9),
                   effect = 'trend', D_cap = c(trt1 = -600)),
               'whole number')
  expect_error(enr(alpha = c(trt1 = 0.025), target_cp = c(trt1 = 0.9),
                   effect = 'trend', D_cap = c(trt1 = NA_real_)),
               'whole number')

  ## d already reaches D_cap
  fit <- fitLogrank(frm, 'pbo', tr$get_locked_data('interim'), 'less',
                    tidy = FALSE)
  small_cap <- floor(fit$info[fit$arm == 'trt1'])
  expect_error(enr(alpha = c(trt1 = 0.025), target_cp = c(trt1 = 0.9),
                   effect = 'trend', D_cap = c(trt1 = small_cap)),
               'observed events d')
})


test_that("the method validates milestone and effect", {

  tr <- run_two_arm_trial()
  frm <- Surv(pfs, pfs_event) ~ arm

  expect_error(
    tr$eventNumberReestimationFromConditionalPower(
      c('interim', 'final'), frm, 'pbo', 'less',
      alpha = 0.022, target_cp = 0.9, effect = 'trend'),
    'single character')

  expect_error(
    tr$eventNumberReestimationFromConditionalPower(
      'nonexistent', frm, 'pbo', 'less',
      alpha = 0.022, target_cp = 0.9, effect = 'trend'),
    'cannot be found')

  ## effect is required
  expect_error(
    tr$eventNumberReestimationFromConditionalPower(
      'interim', frm, 'pbo', 'less', alpha = 0.022, target_cp = 0.9),
    'missing')

  ## 'null' is rejected with a pointer to conditionalPower()
  expect_error(
    tr$eventNumberReestimationFromConditionalPower(
      'interim', frm, 'pbo', 'less',
      alpha = 0.022, target_cp = 0.9, effect = 'null'),
    'not supported')

  expect_error(
    tr$eventNumberReestimationFromConditionalPower(
      'interim', frm, 'pbo', 'less',
      alpha = 0.022, target_cp = 0.9, effect = 'trends'),
    'must be "trend"')

  expect_error(
    tr$eventNumberReestimationFromConditionalPower(
      'interim', frm, 'pbo', 'less',
      alpha = 0.022, target_cp = 0.9, effect = -0.5),
    'positive finite')
})


test_that("boundary-crossed and no-solution requests search D > d", {

  tr <- run_two_arm_trial()
  frm <- Surv(pfs, pfs_event) ~ arm

  ## A lenient boundary already crossed at interim: alpha = 0.4 puts the
  ## final critical value above the observed z. The search nevertheless starts
  ## at the next event number and computes its actual conditional power.
  fit <- fitLogrank(frm, 'pbo', tr$get_locked_data('interim'), 'less',
                    tidy = FALSE)
  crossed <- tr$eventNumberReestimationFromConditionalPower(
    'interim', frm, 'pbo', 'less',
    alpha = 0.4, target_cp = 0.9, effect = 'trend')
  expect_gt(crossed$D, fit$info)
  expect_gte(crossed$achieved_cp, 0.9)
  expect_true(crossed$target_reached)

  ## An unfavorable trend (the reference is the better arm) cannot reach
  ## the target for any D. The unbounded request reports no finite solution.
  res_unbounded <- tr$eventNumberReestimationFromConditionalPower(
    'interim', frm, 'trt', 'less',
    alpha = 0.022, target_cp = 0.9, effect = 'trend')
  expect_true(is.na(res_unbounded$D))
  expect_equal(res_unbounded$D_cap, Inf)
  expect_true(is.na(res_unbounded$achieved_cp))
  expect_equal(res_unbounded$target_cp, 0.9)
  expect_false(res_unbounded$target_reached)

  res <- tr$eventNumberReestimationFromConditionalPower(
    'interim', frm, 'trt', 'less',
    alpha = 0.022, target_cp = 0.9, effect = 'trend', D_cap = 500)
  expect_true(is.na(res$D))
  expect_equal(res$D_cap, 500)
  expect_true(is.na(res$achieved_cp))
  expect_equal(res$target_cp, 0.9)
  expect_false(res$target_reached)

  ## A non-beneficial hazard ratio with no early crossing likewise reports no
  ## finite solution when the search is unbounded.
  res_numeric <- tr$eventNumberReestimationFromConditionalPower(
    'interim', frm, 'pbo', 'less',
    alpha = 0.001, target_cp = 0.9, effect = 1.1)
  expect_true(is.na(res_numeric$D))
  expect_equal(res_numeric$D_cap, Inf)
  expect_true(is.na(res_numeric$achieved_cp))
  expect_false(res_numeric$target_reached)
})


test_that("subset conditions in ... are passed to fitLogrank", {

  tr <- run_two_arm_trial()

  res_all <- tr$eventNumberReestimationFromConditionalPower(
    'interim', Surv(pfs, pfs_event) ~ arm,
    placebo = 'pbo', alternative = 'less',
    alpha = 0.022, target_cp = 0.9, effect = 'trend')

  ## a tautological filter reproduces the unfiltered result
  res_sub <- tr$eventNumberReestimationFromConditionalPower(
    'interim', Surv(pfs, pfs_event) ~ arm,
    placebo = 'pbo', alternative = 'less',
    alpha = 0.022, target_cp = 0.9, effect = 'trend',
    pfs >= 0)
  expect_equal(res_sub, res_all)

  ## a strict filter changes the interim information
  res_f <- tr$eventNumberReestimationFromConditionalPower(
    'interim', Surv(pfs, pfs_event) ~ arm,
    placebo = 'pbo', alternative = 'less',
    alpha = 0.022, target_cp = 0.9, effect = 'trend',
    enroll_time <= 10)
  expect_lt(res_f$d, res_all$d)
})
