# Extracted from test-trial.R:179

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "TrialSimulator", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
rng <- function(n, medians, prop_high){

    ## os
    median_os_low <- TrialSimulator::solveMixtureExponentialDistribution(
      weight1 = prop_high,
      median1 = medians['os_high'],
      overall_median = medians['os_all']
    )

    median_pfs_low <- TrialSimulator::solveMixtureExponentialDistribution(
      weight1 = prop_high,
      median1 = medians['pfs_high'],
      overall_median = medians['pfs_all']
    )

    median_pro_low <- TrialSimulator::solveMixtureExponentialDistribution(
      weight1 = prop_high,
      median1 = medians['pro_high'],
      overall_median = medians['pro_all']
    )

    dll3 <- sample(c('high', 'low'), size = n, replace = TRUE, prob = c(prop_high, 1 - prop_high))

    os_high <- rexp(n, rate = log(2) / medians['os_high'])
    os_low <- rexp(n, rate = log(2) / median_os_low)

    pfs_high <- rexp(n, rate = log(2) / medians['pfs_high'])
    pfs_low <- rexp(n, rate = log(2) / median_pfs_low)

    pro_high <- rexp(n, rate = log(2) / medians['pro_high'])
    pro_low <- rexp(n, rate = log(2) / median_pro_low)

    data.frame(dll3,
               x = rnorm(n),
               os = ifelse(dll3 %in% 'high', os_high, os_low),
               pfs = ifelse(dll3 %in% 'high', pfs_high, pfs_low),
               pro = ifelse(dll3 %in% 'high', pro_high, pro_low),
               os_event = 1,
               pfs_event = 1,
               pro_event = 1)
  }
simulate <- function(fwer,
                       sample_size,
                       final_event_number,
                       interim_info_fraction,
                       prop_dll3_high,
                       median_soc,
                       median_trt){


    ep_soc <- endpoint(name = c('pfs', 'os', 'pro', 'dll3', 'x'),
                       type = c('tte', 'tte', 'tte', 'non-tte', 'non-tte'),
                       readout = c(dll3 = 0, x = 0),
                       generator = rng,
                       medians = median_soc,
                       prop_high = prop_dll3_high)

    soc <- arm(name = 'soc')
    soc$add_endpoints(ep_soc)

    ep_trt <- endpoint(name = c('pfs', 'os', 'pro', 'dll3', 'x'),
                       type = c('tte', 'tte', 'tte', 'non-tte', 'non-tte'),
                       readout = c(dll3 = 0, x = 0),
                       generator = rng,
                       medians = median_trt,
                       prop_high = prop_dll3_high)

    trt <- arm(name = 'trt')
    trt$add_endpoints(ep_trt)


    accrual_rate <- data.frame(end_time = c(1:17, Inf),
                               piecewise_rate = c(1,2,5,8,12,17,22,28,32,37,40,43,46,47,50,50,51,52))

    trial <- trial(
      name = '1438', n_patients = sample_size, duration = 200,
      enroller = StaggeredRecruiter, accrual_rate = accrual_rate,
      dropout = rexp, rate = -log(1 - .15)/12,
      silent = TRUE
    )

    trial$add_arms(sample_ratio = c(1, 1), soc, trt)

    interim <- milestone(name = 'interim', when = enrollment(n = 100, dll3 == 'high', x > -2),
                         action = interim_action)

    final <- milestone(name = 'final',
                       when =
                         eventNumber('pfs', n = 100, dll3 == 'high', x > -2) &
                         eventNumber('pfs', n = 150) &
                         eventNumber('os', n = 25, dll3 != 'high'),
                       action = final_action)

    listener <- listener(silent = TRUE)
    listener$add_milestones(interim, final)

    controller <- controller(trial, listener)
    controller$run(n = 10, plot_event = FALSE, silent = TRUE)

    controller$get_output()

  }
