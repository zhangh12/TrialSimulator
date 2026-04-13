# Parallelization
#
# Covers:
#   - Reproducibility: running n_workers > 1 with seeds extracted from a
#     multi-replicate run reproduces identical results under n_workers = 1.
#     This verifies that the Mersenne-Twister stream reset (instead of
#     L'Ecuyer-CMRG) keeps single- and multi-process outputs in sync.

test_that('package behaves the same under single- and multi-process modes with same seed', {


  foo <- function(n, n_workers, seed = NULL){
    pfs <- endpoint(name = 'pfs', type = 'tte', generator = rexp, rate = log(2)/10)
    os <- endpoint(name = 'os', type = 'tte', generator = rexp, rate = log(2)/17)
    or <- endpoint(name = 'or', type = 'non-tte', readout = c(or = 1), generator = rnorm)

    pbo <- arm(name = 'pbo')
    pbo$add_endpoints(pfs, os, or)

    pfs <- endpoint(name = 'pfs', type = 'tte', generator = rexp, rate = log(2)/12)
    os <- endpoint(name = 'os', type = 'tte', generator = rexp, rate = log(2)/18)
    or <- endpoint(name = 'or', type = 'non-tte', readout = c(or = 1), generator = rnorm)

    trt1 <- arm(name = 'trt1')
    trt1$add_endpoints(pfs, os, or)


    pfs <- endpoint(name = 'pfs', type = 'tte', generator = rexp, rate = log(2)/13)
    os <- endpoint(name = 'os', type = 'tte', generator = rexp, rate = log(2)/18.5)
    or <- endpoint(name = 'or', type = 'non-tte', readout = c(or = 1), generator = rnorm)

    trt2 <- arm(name = 'trt2')
    trt2$add_endpoints(pfs, os, or)


    accrual_rate <- data.frame(end_time = c(1, 2, 6, 12, Inf),
                               piecewise_rate = c(2, 8, 20, 25, 50))

    trial <- trial(
      name = 'test', n_patients = 1000, duration = 40,
      enroller = StaggeredRecruiter, accrual_rate = accrual_rate,
      dropout = rweibull, shape = 1.32, scale = 114.4,
      seed = seed,
      silent = TRUE
    )

    trial$add_arms(sample_ratio = c(1, 1, 2), pbo, trt1, trt2)

    interim1 <- milestone(name = 'interim1',
                          when = eventNumber(endpoint = 'or', n = 200),
                          action = function(trial){trial$remove_arms('trt1')})

    action2 <- function(trial){

      locked_data <- trial$get_locked_data('interim2')
      fit1 <- fitLogrank(Surv(pfs, pfs_event) ~ arm,
                         placebo = 'pbo',
                         data = locked_data,
                         alternative = 'less')
      fit2 <- fitCoxph(Surv(os, os_event) ~ arm,
                       placebo = 'pbo',
                       data = locked_data,
                       alternative = 'less',
                       scale = 'hazard ratio',
                       arm != 'trt1')
      fit3 <- fitLinear(or ~ arm,
                        placebo = 'pbo',
                        data = locked_data,
                        alternative = 'less', arm != 'trt2')
      trial$save(fit1$p[1], 'IA_pval11')
      trial$save(fit1$p[2], 'IA_pval12')
      trial$save(fit2$p[1], 'IA_pval2')
      trial$save(fit3$p[1], 'IA_pval3')
    }

    interim2 <- milestone(name = 'interim2',
                          when = eventNumber(endpoint = 'pfs', n = 240) &
                            eventNumber(endpoint = 'os', n = 170),
                          action = action2)

    action3 <- function(trial){

      locked_data <- trial$get_locked_data('final')
      fit1 <- fitLogrank(Surv(pfs, pfs_event) ~ arm,
                         placebo = 'pbo',
                         data = locked_data,
                         alternative = 'less')
      fit2 <- fitCoxph(Surv(os, os_event) ~ arm,
                       placebo = 'pbo',
                       data = locked_data,
                       alternative = 'less',
                       scale = 'hazard ratio',
                       arm != 'trt1')
      fit3 <- fitLinear(or ~ arm,
                        placebo = 'pbo',
                        data = locked_data,
                        alternative = 'less', arm != 'trt2')
      trial$save(fit1$p[1], 'FA_pval11')
      trial$save(fit1$p[2], 'FA_pval12')
      trial$save(fit2$p[1], 'FA_pval2')
      trial$save(fit3$p[1], 'FA_pval3')
    }

    final <- milestone(name = 'final',
                       when = calendarTime(time = 40),
                       action = action3)

    listener <- listener(silent = TRUE)
    listener$add_milestones(interim1, interim2, final)

    controller <- controller(trial, listener)
    controller$run(n = n, n_workers = n_workers, plot_event = FALSE, silent = TRUE)
    controller$get_output(c('seed', 'IA_pval11', 'IA_pval12', 'IA_pval2', 'IA_pval3',
                            'FA_pval11', 'FA_pval12', 'FA_pval2', 'FA_pval3'))
  }

  op2 <- foo(n = 10, n_workers = 2, seed = NULL)

  op1 <- NULL
  for(i in 1:10){
    op1 <- rbind(op1, foo(n = 1, n_workers = 1, seed = op2$seed[i]))
  }

  expect_equal(op1, op2)

})
