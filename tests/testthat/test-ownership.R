# Ownership of registered R6 objects
#
# The trial captures independent deep copies at registration: add_arms()
# and add_regimen() clone their inputs, so external objects and the trial
# never share mutable state, and reset() reinstalls fresh clones of the
# snapshot arms every replicate.

own_rng <- function(n, prev) {
  data.frame(biomarker = rbinom(n, 1, prev),
             pfs = rexp(n, log(2) / 10), pfs_event = 1)
}

own_arm <- function(name, prev = .5) {
  a <- arm(name = name)
  a$add_endpoints(
    endpoint(name = c("biomarker", "pfs"), type = c("baseline", "tte"),
             generator = own_rng, prev = prev)
  )
  a
}

own_trial <- function(seed = 1, n_patients = 400) {
  trial(name = "t", n_patients = n_patients, duration = 30, seed = seed,
        enroller = StaggeredRecruiter,
        accrual_rate = data.frame(end_time = Inf, piecewise_rate = 30),
        silent = TRUE)
}

arm_prev <- function(a) {
  set.seed(1)
  mean(a$get_endpoints()[[1]]$get_generator()(500)$biomarker)
}


test_that("add_arms captures independent copies of external arms", {

  ext_pbo <- own_arm("pbo")
  ext_trt <- own_arm("trt")
  tr_a <- own_trial(seed = 1)
  tr_b <- own_trial(seed = 2)
  add_arms(tr_a, sample_ratio = c(1, 1), ext_pbo, ext_trt)
  add_arms(tr_b, sample_ratio = c(1, 1), ext_pbo, ext_trt)

  priv_a <- tr_a$.__enclos_env__$private
  priv_b <- tr_b$.__enclos_env__$private

  ## registration clones the arm and its endpoints
  expect_false(identical(priv_a$arms[["trt"]], ext_trt))
  expect_false(identical(priv_a$arms[["trt"]]$get_endpoints()[[1]],
                         ext_trt$get_endpoints()[[1]]))
  expect_false(identical(priv_a$arms[["trt"]]$get_endpoints()[[1]],
                         priv_b$arms[["trt"]]$get_endpoints()[[1]]))

  ## mutating the external arm after registration does not reach the trial
  ext_trt$update_endpoint_generator(c("biomarker", "pfs"), own_rng, prev = 1)
  expect_lt(arm_prev(priv_a$arms[["trt"]]), .7)

  ## an in-trial update_generator() does not reach the external arm or the
  ## other trial sharing the same input objects
  m <- milestone(name = "m", when = calendarTime(time = 5),
                 action = function(trial) {
                   trial$update_generator("pbo", c("biomarker", "pfs"),
                                          own_rng, prev = 1)
                 })
  lstn <- listener(silent = TRUE)
  lstn$add_milestones(m, milestone(name = "final", when = calendarTime(time = 10)))
  controller(tr_a, lstn)$run(n = 1, silent = TRUE, plot_event = FALSE)

  expect_lt(arm_prev(ext_pbo), .7)
  expect_lt(arm_prev(priv_b$arms[["pbo"]]), .7)
})


test_that("re-adding a pre-created arm each replicate stays as-designed", {

  ext_exp <- own_arm("exp")
  pbo <- own_arm("pbo")
  trt <- own_arm("trt")
  tr <- own_trial()
  add_arms(tr, sample_ratio = c(1, 1), pbo, trt)

  add_exp <- milestone(name = "add_exp", when = calendarTime(time = 5),
                       action = function(trial) {
                         trial$add_arms(1, ext_exp)
                       })
  bump <- milestone(name = "bump", when = calendarTime(time = 15),
                    action = function(trial) {
                      locked <- trial$get_locked_data("bump")
                      exp_rows <- locked[locked$arm == "exp", ]
                      trial$save(mean(exp_rows$biomarker), "exp_prev")
                      trial$update_generator("exp", c("biomarker", "pfs"),
                                             own_rng, prev = 1)
                    })
  lstn <- listener(silent = TRUE)
  lstn$add_milestones(add_exp, bump,
                      milestone(name = "final", when = calendarTime(time = 30)))
  ctrl <- controller(tr, lstn)
  ctrl$run(n = 2, silent = TRUE, plot_event = FALSE)
  out <- ctrl$get_output()

  ## replicate 1's update_generator() on the trial-owned copy must reach
  ## neither the external arm nor replicate 2 (which re-adds it)
  expect_lt(out$exp_prev[1], .7)
  expect_lt(out$exp_prev[2], .7)
  expect_lt(arm_prev(ext_exp), .7)
})


test_that("reset installs fresh clones distinct from the snapshot", {

  pbo <- own_arm("pbo")
  trt <- own_arm("trt")
  tr <- own_trial()
  add_arms(tr, sample_ratio = c(1, 1), pbo, trt)

  lstn <- listener(silent = TRUE)
  lstn$add_milestones(milestone(name = "final", when = calendarTime(time = 10)))
  controller(tr, lstn)$run(n = 1, silent = TRUE, plot_event = FALSE)

  priv <- tr$.__enclos_env__$private
  tr$reset()
  for (arm_ in c("pbo", "trt")) {
    expect_false(identical(priv$arms[[arm_]], priv$.snapshot$arms[[arm_]]))
    expect_false(identical(priv$arms[[arm_]]$get_endpoints()[[1]],
                           priv$.snapshot$arms[[arm_]]$get_endpoints()[[1]]))
  }
})


test_that("add_arms registration is transactional", {

  pbo <- own_arm("pbo")
  trt <- own_arm("trt")
  bad <- arm(name = "bad")
  bad$add_endpoints(endpoint(name = "os", type = "tte",
                             generator = rexp, rate = log(2) / 10))
  tr <- own_trial()

  ## the batch fails on the endpoint-set mismatch of the last arm; the
  ## valid arms in the same batch must not have been installed
  expect_error(add_arms(tr, sample_ratio = c(1, 1, 1), pbo, trt, bad),
               "different from other arm")
  expect_identical(tr$get_arms_name(), NULL)
  expect_length(tr$get_sample_ratio(), 0)
})


test_that("add_regimen captures an independent copy of the regimen", {

  what_fn <- function(patient_data) {
    data.frame(patient_id = patient_data$patient_id,
               new_treatment = NA_character_)
  }
  when_fn <- function(patient_data) {
    data.frame(patient_id = patient_data$patient_id,
               switch_time = patient_data$pfs)
  }
  how_fn <- function(patient_data) data.frame(patient_id = patient_data$patient_id)

  rg <- regimen(what_fn, when_fn, how_fn)
  tr <- own_trial()
  tr$add_regimen(rg)

  priv <- tr$.__enclos_env__$private
  expect_false(identical(priv$regimen, rg))
  expect_false(identical(priv$.snapshot$regimen, rg))
  expect_false(identical(priv$regimen, priv$.snapshot$regimen))
})


test_that("crossover() leaves the externally supplied regimen unchanged", {

  cross_arm <- function(name) {
    a <- arm(name = name)
    a$add_endpoints(endpoint(name = "pfs", type = "tte",
                             generator = rexp, rate = log(2) / 8),
                    endpoint(name = "os", type = "tte",
                             generator = rexp, rate = log(2) / 16))
    a
  }
  what_setup <- function(patient_data) {
    data.frame(patient_id = patient_data$patient_id,
               new_treatment = NA_character_)
  }
  when_setup <- function(patient_data) {
    data.frame(patient_id = patient_data$patient_id,
               switch_time = patient_data$pfs)
  }
  how_id <- function(patient_data) data.frame(patient_id = patient_data$patient_id)
  what_switch <- function(patient_data) {
    data.frame(patient_id = patient_data$patient_id,
               new_treatment = ifelse(patient_data$arm == "pbo",
                                      "trt", NA_character_))
  }

  rg <- regimen(what_setup, when_setup, how_id)
  pbo <- cross_arm("pbo")
  trt <- cross_arm("trt")
  tr <- own_trial()
  tr$add_regimen(rg)
  add_arms(tr, sample_ratio = c(1, 1), pbo, trt)

  m <- milestone(name = "m", when = calendarTime(time = 5),
                 action = function(trial) {
                   trial$crossover(what = what_switch, how = how_id)
                 })
  lstn <- listener(silent = TRUE)
  lstn$add_milestones(m, milestone(name = "final", when = calendarTime(time = 10)))
  controller(tr, lstn)$run(n = 1, silent = TRUE, plot_event = FALSE)

  ## the in-run triplet reached only the trial-owned regimen
  expect_identical(rg$get_number_treatment_allocator(), 1L)
  expect_identical(
    tr$.__enclos_env__$private$regimen$get_number_treatment_allocator(), 2L)
})


test_that("re-adding the name of a removed arm fails atomically", {

  pbo <- own_arm("pbo")
  trt <- own_arm("trt")
  tr <- own_trial()
  add_arms(tr, sample_ratio = c(1, 1), pbo, trt)

  m <- milestone(
    name = "m",
    when = calendarTime(time = 5),
    action = function(trial) {
      trial$remove_arms("trt")
      expect_error(trial$add_arms(1, own_arm("trt")),
                   "Re-adding an arm of the same name is not supported")
      ## the failed call must not have touched any trial state
      expect_identical(trial$get_arms_name(), "pbo")
      expect_identical(names(trial$get_sample_ratio()), "pbo")
    })
  lstn <- listener(silent = TRUE)
  lstn$add_milestones(m, milestone(name = "final", when = calendarTime(time = 10)))
  expect_no_error(
    controller(tr, lstn)$run(n = 1, silent = TRUE, plot_event = FALSE)
  )
})


test_that("arms named after add_arms() arguments survive reset", {

  ## the snapshot list is keyed by arm name; reset() must not let these
  ## names be matched to the enforce/sample_ratio arguments of add_arms()
  a1 <- own_arm("enforce")
  a2 <- own_arm("sample_ratio")
  tr <- own_trial()
  add_arms(tr, sample_ratio = c(1, 1), a1, a2)

  lstn <- listener(silent = TRUE)
  lstn$add_milestones(milestone(name = "final", when = calendarTime(time = 10)))
  ctrl <- controller(tr, lstn)
  expect_no_error(ctrl$run(n = 2, silent = TRUE, plot_event = FALSE))
  expect_identical(nrow(ctrl$get_output()), 2L)
  expect_setequal(tr$get_arms_name(), c("enforce", "sample_ratio"))
})


test_that("a replicate re-adding a pre-created arm reproduces from its seed", {

  mk_setup <- function(seed) {
    ext_exp <- own_arm("exp")
    pbo <- own_arm("pbo")
    trt <- own_arm("trt")
    tr <- own_trial(seed = seed)
    add_arms(tr, sample_ratio = c(1, 1), pbo, trt)

    add_exp <- milestone(name = "add_exp", when = calendarTime(time = 5),
                         action = function(trial) {
                           trial$add_arms(1, ext_exp)
                         })
    bump <- milestone(name = "bump", when = calendarTime(time = 15),
                      action = function(trial) {
                        locked <- trial$get_locked_data("bump")
                        exp_rows <- locked[locked$arm == "exp", ]
                        trial$save(mean(exp_rows$biomarker), "exp_prev")
                        trial$update_generator("exp", c("biomarker", "pfs"),
                                               own_rng, prev = 1)
                      })
    lstn <- listener(silent = TRUE)
    lstn$add_milestones(add_exp, bump,
                        milestone(name = "final", when = calendarTime(time = 30)))
    controller(tr, lstn)
  }

  ctrl <- mk_setup(seed = 1)
  ctrl$run(n = 2, silent = TRUE, plot_event = FALSE)
  out <- ctrl$get_output()

  ctrl2 <- mk_setup(seed = out$seed[2])
  ctrl2$run(n = 1, silent = TRUE, plot_event = FALSE)
  expect_identical(ctrl2$get_output()$exp_prev, out$exp_prev[2])
})


test_that("run() restores a listener reused from an earlier controller", {

  mk_trial_pair <- function(seed) {
    pbo <- own_arm("pbo")
    trt <- own_arm("trt")
    tr <- own_trial(seed = seed)
    add_arms(tr, sample_ratio = c(1, 1), pbo, trt)
    tr
  }
  m <- milestone(name = "m", when = calendarTime(time = 5),
                 action = function(trial) trial$save(1, "m_ran"))
  lstn <- listener(silent = TRUE)
  lstn$add_milestones(m, milestone(name = "final", when = calendarTime(time = 10)))

  controller(mk_trial_pair(1), lstn)$run(n = 1, silent = TRUE, plot_event = FALSE)

  ## the same listener in a new controller: its milestones must be restored,
  ## not silently skipped as already-triggered
  ctrl2 <- controller(mk_trial_pair(2), lstn)
  ctrl2$run(n = 1, silent = TRUE, plot_event = FALSE)
  expect_identical(ctrl2$get_output("m_ran"), 1)
})
