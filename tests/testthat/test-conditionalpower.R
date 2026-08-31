# Conditional power at an interim milestone
#
# Covers: the internal formula .conditional_power(), validated against two
# independent oracles (rpact::getConditionalPower, analytic, and
# gsDesign::gsCP, numerical integration) plus convention-free property
# checks (mirror symmetry, consistency across effect modes, monotonicity,
# boundary behavior); and the method Trials$conditionalPower() (extraction
# of z and d from locked data, omega from the sample ratio recorded at the
# milestone, D/alpha shape rules, subsetting, error paths).

cp_internal <- TrialSimulator:::.conditional_power

## conditional power from rpact for a design spending no alpha at interim;
## in this special case total alpha equals the nominal level corresponding
## to the final boundary. thetaH1 = NULL extrapolates the interim trend
## (rpact's default)
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


test_that(".conditional_power agrees with rpact (analytic oracle)", {

  cases <- data.frame(z = c(-2, -1.2, -2.8),
                      d = c(100, 80, 250),
                      D = c(200, 350, 400),
                      alpha = c(0.025, 0.01, 0.045))

  for(i in seq_len(nrow(cases))){
    z <- cases$z[i]; d <- cases$d[i]; D <- cases$D[i]; alpha <- cases$alpha[i]

    ## trend
    expect_equal(
      cp_internal(z, d, D, alpha, effect = 'trend', omega = NA,
                  alternative = 'less'),
      rpact_cp(z, d, D, alpha),
      tolerance = 1e-8
    )

    ## null
    expect_equal(
      cp_internal(z, d, D, alpha, effect = 'null', omega = NA,
                  alternative = 'less'),
      rpact_cp(z, d, D, alpha, thetaH1 = 1),
      tolerance = 1e-8
    )

    ## fixed hazard ratio, 1:1 and 2:1 allocation
    expect_equal(
      cp_internal(z, d, D, alpha, effect = 0.75, omega = 0.25,
                  alternative = 'less'),
      rpact_cp(z, d, D, alpha, thetaH1 = 0.75),
      tolerance = 1e-8
    )
    expect_equal(
      cp_internal(z, d, D, alpha, effect = 0.75, omega = 2 / 9,
                  alternative = 'less'),
      rpact_cp(z, d, D, alpha, thetaH1 = 0.75, allocationRatioPlanned = 2),
      tolerance = 1e-8
    )
  }

  ## 'greater': positive z favors treatment, HR > 1 under H1
  expect_equal(
    cp_internal(z = 2, d = 100, D = 200, alpha = 0.025, effect = 'trend',
                omega = NA, alternative = 'greater'),
    rpact_cp(z = 2, d = 100, D = 200, alpha = 0.025, directionUpper = TRUE),
    tolerance = 1e-8
  )
  expect_equal(
    cp_internal(z = 2, d = 100, D = 200, alpha = 0.025, effect = 4 / 3,
                omega = 0.25, alternative = 'greater'),
    rpact_cp(z = 2, d = 100, D = 200, alpha = 0.025, thetaH1 = 4 / 3,
             directionUpper = TRUE),
    tolerance = 1e-8
  )
})


test_that(".conditional_power agrees with gsDesign::gsCP (independent oracle)", {

  skip_if_not_installed('gsDesign')

  z <- -2; d <- 100; D <- 200; alpha <- 0.025
  hr <- 0.75

  ## two-look design, one-sided, with (essentially) no interim alpha spent:
  ## total alpha therefore equals the final boundary's nominal level, and
  ## the boundary reproduces qnorm(1 - alpha)
  x <- gsDesign::gsDesign(k = 2, test.type = 1, alpha = alpha,
                          n.I = c(d, D), maxn.IPlan = D,
                          sfu = gsDesign::sfPoints, sfupar = c(1e-6, 1))
  expect_equal(x$upper$bound[2], qnorm(1 - alpha), tolerance = 1e-5)

  ## gsCP works on the positive-favors-treatment scale with per-event
  ## drift theta (E[Z_k] = theta * sqrt(n.I[k])): map z -> -z and
  ## HR -> theta = -log(hr) * sqrt(omega)
  zi <- -z
  gs_cp <- function(g) sum(g$upper$prob[, 1])

  expect_equal(
    cp_internal(z, d, D, alpha, effect = 'trend', omega = NA,
                alternative = 'less'),
    gs_cp(gsDesign::gsCP(x, i = 1, zi = zi)),
    tolerance = 1e-5
  )
  expect_equal(
    cp_internal(z, d, D, alpha, effect = 'null', omega = NA,
                alternative = 'less'),
    gs_cp(gsDesign::gsCP(x, theta = 0, i = 1, zi = zi)),
    tolerance = 1e-5
  )
  expect_equal(
    cp_internal(z, d, D, alpha, effect = hr, omega = 0.25,
                alternative = 'less'),
    gs_cp(gsDesign::gsCP(x, theta = -log(hr) * sqrt(0.25), i = 1, zi = zi)),
    tolerance = 1e-5
  )
  expect_equal(
    cp_internal(z, d, D, alpha, effect = hr, omega = 2 / 9,
                alternative = 'less'),
    gs_cp(gsDesign::gsCP(x, theta = -log(hr) * sqrt(2 / 9), i = 1, zi = zi)),
    tolerance = 1e-5
  )
})


test_that("alpha is the nominal level corresponding to the final boundary", {

  ## O'Brien-Fleming type spending with an efficacy interim: the final
  ## nominal level corresponding to the boundary differs from the total
  ## design alpha, and it is the former that conditionalPower() consumes
  z <- -2; d <- 100; D <- 200
  total_alpha <- 0.025

  design <- rpact::getDesignGroupSequential(
    kMax = 2, alpha = total_alpha, sided = 1,
    informationRates = c(d / D, 1),
    typeOfDesign = 'asOF')

  nominal_final <- 1 - pnorm(design$criticalValues[2])
  expect_lt(nominal_final, total_alpha)   # the distinction is real

  dat <- rpact::getDataset(events = d, logRanks = z)
  sr <- rpact::getStageResults(design, dataInput = dat,
                               directionUpper = FALSE)
  oracle <- suppressMessages(
    rpact::getConditionalPower(sr, nPlanned = D - d)
  )$conditionalPower[2]

  ## feeding the nominal final level reproduces the oracle; feeding the
  ## total design alpha does not
  expect_equal(
    cp_internal(z, d, D, alpha = nominal_final, effect = 'trend',
                omega = NA, alternative = 'less'),
    oracle,
    tolerance = 1e-8
  )
  expect_false(isTRUE(all.equal(
    cp_internal(z, d, D, alpha = total_alpha, effect = 'trend',
                omega = NA, alternative = 'less'),
    oracle
  )))
})


test_that(".conditional_power is internally consistent across effect modes", {

  z <- -1.8; d <- 150; D <- 400; alpha <- 0.023

  ## 'trend' equals a numeric effect at the interim estimate
  ## HR_hat = exp(z / sqrt(omega * d)), for any omega
  for(omega in c(0.25, 2 / 9)){
    hr_hat <- exp(z / sqrt(omega * d))
    expect_equal(
      cp_internal(z, d, D, alpha, effect = hr_hat, omega = omega,
                  alternative = 'less'),
      cp_internal(z, d, D, alpha, effect = 'trend', omega = omega,
                  alternative = 'less')
    )
  }

  ## 'null' equals a numeric effect at HR = 1, for any omega
  expect_equal(
    cp_internal(z, d, D, alpha, effect = 1, omega = 0.25,
                alternative = 'less'),
    cp_internal(z, d, D, alpha, effect = 'null', omega = NA,
                alternative = 'less')
  )
  expect_equal(
    cp_internal(z, d, D, alpha, effect = 1, omega = 2 / 9,
                alternative = 'less'),
    cp_internal(z, d, D, alpha, effect = 'null', omega = NA,
                alternative = 'less')
  )
})


test_that(".conditional_power is mirror-symmetric in the alternative", {

  z <- -1.4; d <- 90; D <- 250; alpha <- 0.025

  ## trend and null: negate z
  for(effect in c('trend', 'null')){
    expect_equal(
      cp_internal(z, d, D, alpha, effect = effect, omega = NA,
                  alternative = 'less'),
      cp_internal(-z, d, D, alpha, effect = effect, omega = NA,
                  alternative = 'greater')
    )
  }

  ## numeric effect: negate z and invert the hazard ratio
  hr <- 0.65; omega <- 0.25
  expect_equal(
    cp_internal(z, d, D, alpha, effect = hr, omega = omega,
                alternative = 'less'),
    cp_internal(-z, d, D, alpha, effect = 1 / hr, omega = omega,
                alternative = 'greater')
  )
})


test_that(".conditional_power behaves monotonically and at boundaries", {

  d <- 100; D <- 200; alpha <- 0.025

  ## more favorable z (more negative under 'less') increases CP
  zs <- c(-3, -2, -1, 0, 1)
  cps <- cp_internal(zs, d, D, alpha, effect = 'trend', omega = NA,
                     alternative = 'less')
  expect_true(all(diff(cps) < 0))

  ## a stronger assumed effect (smaller HR under 'less') increases CP
  cp_strong <- cp_internal(-1, d, D, alpha, effect = 0.6, omega = 0.25,
                           alternative = 'less')
  cp_weak <- cp_internal(-1, d, D, alpha, effect = 0.9, omega = 0.25,
                         alternative = 'less')
  expect_gt(cp_strong, cp_weak)

  ## as d -> D, CP degenerates by the side of z relative to the boundary
  expect_gt(cp_internal(-3, 999, 1000, alpha, effect = 'trend', omega = NA,
                        alternative = 'less'), 0.999)
  expect_lt(cp_internal(0, 999, 1000, alpha, effect = 'trend', omega = NA,
                        alternative = 'less'), 0.001)

  ## unbalanced allocation carries less information per event: under the
  ## same beneficial HR, 2:1 (omega = 2/9) yields lower CP than 1:1 (1/4)
  expect_lt(
    cp_internal(-1, d, D, alpha, effect = 0.7, omega = 2 / 9,
                alternative = 'less'),
    cp_internal(-1, d, D, alpha, effect = 0.7, omega = 0.25,
                alternative = 'less')
  )

  ## guards of the internal function
  expect_error(
    cp_internal(-1, 200, 200, alpha, effect = 'trend', omega = NA,
                alternative = 'less')
  )
  expect_error(
    cp_internal(-1, d, D, alpha, effect = 0.7, omega = NA,
                alternative = 'less')
  )
})


## ---- method-level tests ----------------------------------------------------
## Formula correctness is delegated to the oracle tests above; these tests
## verify the wiring of Trials$conditionalPower(): z and d extracted from
## the milestone's locked data via fitLogrank(), omega derived from the
## sample ratio recorded at the milestone, and D/alpha shape rules.

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

run_three_arm_trial <- function(seed = 27183, remove_at_8 = NULL) {
  pbo <- make_arm("pbo", 10)
  trt1 <- make_arm("trt1", 12)
  trt2 <- make_arm("trt2", 16)
  tr <- make_trial(seed = seed)
  add_arms(tr, sample_ratio = c(1, 1, 1), pbo, trt1, trt2)

  milestones <- list(
    milestone(name = "interim", when = calendarTime(time = 15)),
    milestone(name = "final", when = calendarTime(time = 40))
  )
  if(!is.null(remove_at_8)){
    arm_to_drop <- remove_at_8
    milestones <- c(
      list(milestone(name = "select", when = calendarTime(time = 8),
                     action = function(trial){
                       remove_arms(trial, arm_to_drop)
                     })),
      milestones
    )
  }

  lstn <- listener(silent = TRUE)
  do.call(lstn$add_milestones, milestones)
  controller(tr, lstn)$run(n = 1, silent = TRUE, plot_event = FALSE)
  tr
}


test_that("conditionalPower extracts z and d from locked data (two arms)", {

  tr <- run_two_arm_trial()

  fit <- fitLogrank(Surv(pfs, pfs_event) ~ arm, 'pbo',
                    tr$get_locked_data('interim'), 'less', tidy = FALSE)

  res <- tr$conditionalPower('interim', Surv(pfs, pfs_event) ~ arm,
                             placebo = 'pbo', alternative = 'less',
                             alpha = 0.025, D = 800)

  expect_s3_class(res, 'data.frame')
  expect_equal(nrow(res), 1)
  expect_equal(
    colnames(res),
    c('arm', 'placebo', 'z', 'd', 'D', 'info_fraction', 'alpha',
      'effect', 'cp')
  )
  expect_equal(res$arm, 'trt')
  expect_equal(res$placebo, 'pbo')
  expect_equal(res$z, fit$z)
  expect_equal(res$d, fit$info)
  expect_equal(res$D, 800)
  expect_equal(res$info_fraction, fit$info / 800)
  expect_equal(res$effect, 'trend')
  expect_gt(res$d, 0)

  ## end-to-end against the rpact oracle on the extracted z and d
  expect_equal(res$cp, rpact_cp(fit$z, fit$info, 800, 0.025),
               tolerance = 1e-8)

  ## null effect
  res0 <- tr$conditionalPower('interim', Surv(pfs, pfs_event) ~ arm,
                              placebo = 'pbo', alternative = 'less',
                              alpha = 0.025, D = 800, effect = 'null')
  expect_equal(res0$cp, rpact_cp(fit$z, fit$info, 800, 0.025, thetaH1 = 1),
               tolerance = 1e-8)
})


test_that("conditionalPower converts a hazard ratio using the sample ratio", {

  ## 1:1 -> omega = 1/4
  tr <- run_two_arm_trial(sample_ratio = c(1, 1))
  fit <- fitLogrank(Surv(pfs, pfs_event) ~ arm, 'pbo',
                    tr$get_locked_data('interim'), 'less', tidy = FALSE)
  res <- tr$conditionalPower('interim', Surv(pfs, pfs_event) ~ arm,
                             placebo = 'pbo', alternative = 'less',
                             alpha = 0.025, D = 800, effect = 0.75)
  expect_equal(res$effect, 0.75)
  expect_equal(res$cp,
               rpact_cp(fit$z, fit$info, 800, 0.025, thetaH1 = 0.75),
               tolerance = 1e-8)

  ## 2:1 (trt : pbo) -> omega = 2/9
  tr2 <- run_two_arm_trial(sample_ratio = c(1, 2))
  fit2 <- fitLogrank(Surv(pfs, pfs_event) ~ arm, 'pbo',
                     tr2$get_locked_data('interim'), 'less', tidy = FALSE)
  res2 <- tr2$conditionalPower('interim', Surv(pfs, pfs_event) ~ arm,
                               placebo = 'pbo', alternative = 'less',
                               alpha = 0.025, D = 800, effect = 0.75)
  expect_equal(res2$cp,
               rpact_cp(fit2$z, fit2$info, 800, 0.025, thetaH1 = 0.75,
                        allocationRatioPlanned = 2),
               tolerance = 1e-8)
})


test_that("conditionalPower uses the sample ratio recorded at the milestone", {

  ## 1:1 at the interim, updated to 1:4 at a later milestone: conditional
  ## power at each milestone uses the ratio recorded at ITS lock
  pbo <- make_arm("pbo", 10)
  trt <- make_arm("trt", 14)
  tr <- make_trial()
  add_arms(tr, sample_ratio = c(1, 1), pbo, trt)

  lstn <- listener(silent = TRUE)
  lstn$add_milestones(
    milestone(name = "interim", when = calendarTime(time = 15)),
    milestone(name = "update", when = calendarTime(time = 20),
              action = function(trial){
                update_sample_ratio(trial, arm_names = c('pbo', 'trt'),
                                    sample_ratios = c(1, 4))
              }),
    milestone(name = "final", when = calendarTime(time = 40))
  )
  controller(tr, lstn)$run(n = 1, silent = TRUE, plot_event = FALSE)

  frm <- Surv(pfs, pfs_event) ~ arm

  ## at the interim (before the update): ratio 1:1
  fit <- fitLogrank(frm, 'pbo', tr$get_locked_data('interim'), 'less',
                    tidy = FALSE)
  res <- tr$conditionalPower('interim', frm, 'pbo', 'less',
                             alpha = 0.025, D = 800, effect = 0.75)
  expect_equal(res$cp,
               rpact_cp(fit$z, fit$info, 800, 0.025, thetaH1 = 0.75,
                        allocationRatioPlanned = 1),
               tolerance = 1e-8)

  ## at the update milestone itself: its data was locked before the action
  ## changed the ratio, so it is still 1:1
  fit_u <- fitLogrank(frm, 'pbo', tr$get_locked_data('update'), 'less',
                      tidy = FALSE)
  res_u <- tr$conditionalPower('update', frm, 'pbo', 'less',
                               alpha = 0.025, D = 800, effect = 0.75)
  expect_equal(res_u$cp,
               rpact_cp(fit_u$z, fit_u$info, 800, 0.025, thetaH1 = 0.75,
                        allocationRatioPlanned = 1),
               tolerance = 1e-8)

  ## at the final milestone (after the update): ratio 1:4. Whether the
  ## calculation is legitimate here is the user's responsibility; the
  ## method computes it under the ratio recorded at the final's lock
  fit_f <- fitLogrank(frm, 'pbo', tr$get_locked_data('final'), 'less',
                      tidy = FALSE)
  res_f <- tr$conditionalPower('final', frm, 'pbo', 'less',
                               alpha = 0.025, D = 800, effect = 0.75)
  expect_equal(res_f$cp,
               rpact_cp(fit_f$z, fit_f$info, 800, 0.025, thetaH1 = 0.75,
                        allocationRatioPlanned = 4),
               tolerance = 1e-8)
})


test_that("per-arm renumbering does not change the allocation ratio", {

  ## c(1, 1) -> c(2, 2): per-arm numbers change but the actual allocation
  ## ratio does not; omega is unaffected
  pbo <- make_arm("pbo", 10)
  trt <- make_arm("trt", 14)
  tr <- make_trial()
  add_arms(tr, sample_ratio = c(1, 1), pbo, trt)

  lstn <- listener(silent = TRUE)
  lstn$add_milestones(
    milestone(name = "renumber", when = calendarTime(time = 10),
              action = function(trial){
                update_sample_ratio(trial, arm_names = c('pbo', 'trt'),
                                    sample_ratios = c(2, 2))
              }),
    milestone(name = "interim", when = calendarTime(time = 15)),
    milestone(name = "final", when = calendarTime(time = 40))
  )
  controller(tr, lstn)$run(n = 1, silent = TRUE, plot_event = FALSE)

  fit <- fitLogrank(Surv(pfs, pfs_event) ~ arm, 'pbo',
                    tr$get_locked_data('interim'), 'less', tidy = FALSE)
  res <- tr$conditionalPower('interim', Surv(pfs, pfs_event) ~ arm,
                             placebo = 'pbo', alternative = 'less',
                             alpha = 0.025, D = 800, effect = 0.75)
  expect_equal(res$cp,
               rpact_cp(fit$z, fit$info, 800, 0.025, thetaH1 = 0.75,
                        allocationRatioPlanned = 1),
               tolerance = 1e-8)
})


test_that("conditionalPower handles multiple arms with named D and alpha", {

  tr <- run_three_arm_trial()

  fit <- fitLogrank(Surv(pfs, pfs_event) ~ arm, 'pbo',
                    tr$get_locked_data('interim'), 'less', tidy = FALSE)

  ## names in different orders: matched by arm name, not position
  res <- tr$conditionalPower('interim', Surv(pfs, pfs_event) ~ arm,
                             placebo = 'pbo', alternative = 'less',
                             alpha = c(trt2 = 0.02, trt1 = 0.025),
                             D = c(trt1 = 600, trt2 = 700))

  expect_equal(nrow(res), 2)
  expect_equal(res$arm, c('trt1', 'trt2'))
  expect_equal(res$alpha, c(0.025, 0.02))
  expect_equal(res$D, c(600, 700))

  for(i in 1:2){
    fit_i <- fit[fit$arm == res$arm[i], ]
    expect_equal(res$z[i], fit_i$z)
    expect_equal(res$d[i], fit_i$info)
    expect_equal(res$cp[i],
                 rpact_cp(fit_i$z, fit_i$info, res$D[i], res$alpha[i]),
                 tolerance = 1e-8)
  }

  ## a subset of arms restricts the comparisons
  res_sub <- tr$conditionalPower('interim', Surv(pfs, pfs_event) ~ arm,
                                 placebo = 'pbo', alternative = 'less',
                                 alpha = c(trt2 = 0.02), D = c(trt2 = 700))
  expect_equal(nrow(res_sub), 1)
  expect_equal(res_sub$arm, 'trt2')
  expect_equal(res_sub$cp, res$cp[res$arm == 'trt2'])
})


test_that("numeric-effect conditionalPower works for arms removed after the milestone", {

  ## trt1 is removed at time 20, after the interim at 15: at the interim
  ## the comparison was valid, and the ratio recorded at the interim still
  ## contains trt1
  pbo <- make_arm("pbo", 10)
  trt1 <- make_arm("trt1", 12)
  trt2 <- make_arm("trt2", 16)
  tr <- make_trial(seed = 27183)
  add_arms(tr, sample_ratio = c(1, 1, 1), pbo, trt1, trt2)

  lstn <- listener(silent = TRUE)
  lstn$add_milestones(
    milestone(name = "interim", when = calendarTime(time = 15)),
    milestone(name = "select", when = calendarTime(time = 20),
              action = function(trial){ remove_arms(trial, 'trt1') }),
    milestone(name = "final", when = calendarTime(time = 40))
  )
  controller(tr, lstn)$run(n = 1, silent = TRUE, plot_event = FALSE)

  fit <- fitLogrank(Surv(pfs, pfs_event) ~ arm, 'pbo',
                    tr$get_locked_data('interim'), 'less', tidy = FALSE)

  res <- tr$conditionalPower('interim', Surv(pfs, pfs_event) ~ arm,
                             placebo = 'pbo', alternative = 'less',
                             alpha = c(trt1 = 0.025, trt2 = 0.02),
                             D = c(trt1 = 600, trt2 = 700),
                             effect = 0.75)

  expect_equal(nrow(res), 2)
  for(i in 1:2){
    fit_i <- fit[fit$arm == res$arm[i], ]
    expect_equal(res$cp[i],
                 rpact_cp(fit_i$z, fit_i$info, res$D[i], res$alpha[i],
                          thetaH1 = 0.75, allocationRatioPlanned = 1),
                 tolerance = 1e-8)
  }
})


test_that("arms removed before the milestone: trend works, hazard ratio errors", {

  tr <- run_three_arm_trial(remove_at_8 = 'trt1')

  frm <- Surv(pfs, pfs_event) ~ arm
  fit <- fitLogrank(frm, 'pbo', tr$get_locked_data('interim'), 'less',
                    tidy = FALSE)

  ## trt1 was removed at time 8, before the interim at 15. Its z and d are
  ## historical quantities and conditional power is still computed;
  ## interpretation is users' responsibility
  res <- tr$conditionalPower('interim', frm, 'pbo', 'less',
                             alpha = c(trt1 = 0.025, trt2 = 0.02),
                             D = c(trt1 = 600, trt2 = 700))
  expect_equal(nrow(res), 2)
  for(i in 1:2){
    fit_i <- fit[fit$arm == res$arm[i], ]
    expect_equal(res$z[i], fit_i$z)
    expect_equal(res$d[i], fit_i$info)
    expect_equal(res$cp[i],
                 rpact_cp(fit_i$z, fit_i$info, res$D[i], res$alpha[i]),
                 tolerance = 1e-8)
  }

  ## a numeric effect needs the pair's allocation ratio at the milestone,
  ## which no longer exists for the removed arm
  expect_error(
    tr$conditionalPower('interim', frm, 'pbo', 'less',
                        alpha = c(trt1 = 0.025, trt2 = 0.02),
                        D = c(trt1 = 600, trt2 = 700),
                        effect = 0.75),
    'not in the sample ratio recorded'
  )

  ## the remaining arm alone works with a numeric effect
  fit2 <- fit[fit$arm == 'trt2', ]
  res2 <- tr$conditionalPower('interim', frm, 'pbo', 'less',
                              alpha = c(trt2 = 0.02), D = c(trt2 = 700),
                              effect = 0.75)
  expect_equal(res2$cp,
               rpact_cp(fit2$z, fit2$info, 700, 0.02, thetaH1 = 0.75,
                        allocationRatioPlanned = 1),
               tolerance = 1e-8)
})


test_that("conditionalPower validates D and alpha shapes", {

  tr <- run_three_arm_trial()
  frm <- Surv(pfs, pfs_event) ~ arm

  ## unnamed scalars with more than one available arm
  expect_error(
    tr$conditionalPower('interim', frm, 'pbo', 'less',
                        alpha = 0.025, D = 600),
    'unnamed scalars'
  )

  ## unnamed vectors of length > 1
  expect_error(
    tr$conditionalPower('interim', frm, 'pbo', 'less',
                        alpha = c(0.025, 0.02), D = c(600, 700)),
    'Name their entries'
  )

  ## length mismatch
  expect_error(
    tr$conditionalPower('interim', frm, 'pbo', 'less',
                        alpha = c(trt1 = 0.025),
                        D = c(trt1 = 600, trt2 = 700)),
    'same length'
  )

  ## one named, one unnamed
  expect_error(
    tr$conditionalPower('interim', frm, 'pbo', 'less',
                        alpha = c(0.025, 0.02),
                        D = c(trt1 = 600, trt2 = 700)),
    'both named'
  )

  ## name sets differ
  expect_error(
    tr$conditionalPower('interim', frm, 'pbo', 'less',
                        alpha = c(trt1 = 0.025, pbo = 0.02),
                        D = c(trt1 = 600, trt2 = 700)),
    'same set of treatment arms'
  )

  ## nonexistent arm
  expect_error(
    tr$conditionalPower('interim', frm, 'pbo', 'less',
                        alpha = c(trt3 = 0.025), D = c(trt3 = 600)),
    'not among the treatment arms available'
  )

  ## duplicated names
  expect_error(
    tr$conditionalPower('interim', frm, 'pbo', 'less',
                        alpha = c(trt1 = 0.025, trt1 = 0.02),
                        D = c(trt1 = 600, trt2 = 700)),
    'Duplicated'
  )
})


test_that("conditionalPower reports fitting failures with context", {

  ## an endpoint that never has an event before the interim: the failure
  ## from fitLogrank()/coxph() is re-signaled with conditional power
  ## context instead of crashing with an unrelated message
  no_event_gen <- function(n) rep(1000, n)

  make_no_event_arm <- function(name){
    ep <- endpoint(name = 'pfs', type = 'tte', generator = no_event_gen)
    a <- arm(name = name)
    a$add_endpoints(ep)
    a
  }

  pbo <- make_no_event_arm('pbo')
  trt <- make_no_event_arm('trt')
  tr <- make_trial()
  add_arms(tr, sample_ratio = c(1, 1), pbo, trt)

  lstn <- listener(silent = TRUE)
  lstn$add_milestones(
    milestone(name = "interim", when = calendarTime(time = 15)),
    milestone(name = "final", when = calendarTime(time = 40))
  )
  ## lock_data() warns about the event-free snapshots; that warning is not
  ## under test here
  suppressWarnings(
    controller(tr, lstn)$run(n = 1, silent = TRUE, plot_event = FALSE)
  )

  expect_error(
    tr$conditionalPower('interim', Surv(pfs, pfs_event) ~ arm,
                        placebo = 'pbo', alternative = 'less',
                        alpha = 0.025, D = 800),
    'Unable to fit logrank models'
  )

  ## invalid placebo and formula are caught by fitLogrank(), whose message
  ## is preserved inside the re-signaled error
  tr2 <- run_two_arm_trial()
  expect_error(
    tr2$conditionalPower('interim', Surv(pfs, pfs_event) ~ arm,
                         placebo = c('pbo', 'trt'), alternative = 'less',
                         alpha = 0.025, D = 800),
    'single character string'
  )
  expect_error(
    tr2$conditionalPower('interim', 'Surv(pfs, pfs_event) ~ arm',
                         placebo = 'pbo', alternative = 'less',
                         alpha = 0.025, D = 800),
    'formula'
  )
})


test_that("conditionalPower rejects d >= D with an informative message", {

  tr <- run_two_arm_trial()

  fit <- fitLogrank(Surv(pfs, pfs_event) ~ arm, 'pbo',
                    tr$get_locked_data('interim'), 'less', tidy = FALSE)
  expect_gt(fit$info, 5)

  err <- tryCatch(
    tr$conditionalPower('interim', Surv(pfs, pfs_event) ~ arm,
                        placebo = 'pbo', alternative = 'less',
                        alpha = 0.025, D = 5),
    error = function(e) conditionMessage(e)
  )
  expect_match(err, 'arm <trt>')
  expect_match(err, paste0('d = ', fit$info))
  expect_match(err, 'D = 5')
})


test_that("conditionalPower validates its remaining arguments", {

  tr <- run_two_arm_trial()
  frm <- Surv(pfs, pfs_event) ~ arm

  ## untriggered milestone
  expect_error(
    tr$conditionalPower('nonexistent', frm, 'pbo', 'less',
                        alpha = 0.025, D = 800),
    'cannot be found'
  )

  ## milestone must be a single character
  expect_error(
    tr$conditionalPower(c('interim', 'final'), frm, 'pbo', 'less',
                        alpha = 0.025, D = 800),
    'single character'
  )

  ## invalid alpha and D
  expect_error(
    tr$conditionalPower('interim', frm, 'pbo', 'less',
                        alpha = 1.5, D = 800),
    'in \\(0, 1\\)'
  )
  expect_error(
    tr$conditionalPower('interim', frm, 'pbo', 'less',
                        alpha = 0.025, D = 800.5),
    'whole number'
  )
  expect_error(
    tr$conditionalPower('interim', frm, 'pbo', 'less',
                        alpha = 0.025, D = Inf),
    'whole number'
  )

  ## invalid effect
  expect_error(
    tr$conditionalPower('interim', frm, 'pbo', 'less',
                        alpha = 0.025, D = 800, effect = 'oracle'),
    'trend'
  )
  expect_error(
    tr$conditionalPower('interim', frm, 'pbo', 'less',
                        alpha = 0.025, D = 800, effect = -0.5),
    'positive'
  )
  expect_error(
    tr$conditionalPower('interim', frm, 'pbo', 'less',
                        alpha = 0.025, D = 800, effect = c(0.7, 0.8)),
    'single'
  )
})
