# update_milestone(): updating when/action of a not-yet-triggered milestone
# from within an action function.
#
# Covers: raising the target event number of the final analysis (conditional
# power use case), switching the triggering condition type (calendarTime ->
# eventNumber), replacing the action (with fixed arguments), restoration of
# the as-designed milestone between replicates, same-pass effectiveness on
# the immediately next milestone, and the validation matrix.

make_arm <- function(name, rate) {
  ep <- endpoint(name = 'pfs', type = 'tte', generator = rexp,
                 rate = log(2) / rate)
  a <- arm(name = name)
  a$add_endpoints(ep)
  a
}

make_trial <- function(seed = 1, n_patients = 400, duration = 40) {
  accrual <- data.frame(end_time = Inf, piecewise_rate = 30)
  trial(name = "t", n_patients = n_patients, duration = duration, seed = seed,
        enroller = StaggeredRecruiter, accrual_rate = accrual,
        dropout = rweibull, shape = 1, scale = 1e6,
        silent = TRUE)
}

run_two_milestones <- function(interim_action, final_when,
                               final_action = doNothing, seed = 1) {
  pbo <- make_arm("pbo", 10)
  trt <- make_arm("trt", 12)
  tr <- make_trial(seed = seed)
  add_arms(tr, sample_ratio = c(1, 1), pbo, trt)

  interim <- milestone(name = "interim", when = calendarTime(time = 8),
                       action = interim_action)
  final <- milestone(name = "final", when = final_when,
                     action = final_action)
  lstn <- listener(silent = TRUE)
  lstn$add_milestones(interim, final)
  controller(tr, lstn)$run(n = 1, silent = TRUE, plot_event = FALSE)
  tr
}


test_that("raising the final event target postpones the final analysis", {

  ## as designed: final at the 100th pfs event
  tr0 <- run_two_milestones(doNothing, eventNumber(endpoint = 'pfs', n = 100))
  t0 <- unname(tr0$get_milestone_time('final'))
  d0 <- tr0$get_locked_data('final')
  expect_equal(sum(d0$pfs_event == 1), 100)

  ## conditional power too low at interim: raise the target to 150
  tr1 <- run_two_milestones(
    function(trial) {
      update_milestone(trial, 'final',
                       when = eventNumber(endpoint = 'pfs', n = 150))
    },
    eventNumber(endpoint = 'pfs', n = 100))
  t1 <- unname(tr1$get_milestone_time('final'))
  d1 <- tr1$get_locked_data('final')

  expect_equal(sum(d1$pfs_event == 1), 150)
  expect_gt(t1, t0)
})


test_that("triggering condition can switch from calendarTime to eventNumber", {

  tr <- run_two_milestones(
    function(trial) {
      update_milestone(trial, 'final',
                       when = eventNumber(endpoint = 'pfs', n = 130))
    },
    calendarTime(time = 25))

  t_final <- unname(tr$get_milestone_time('final'))
  d <- tr$get_locked_data('final')

  expect_equal(sum(d$pfs_event == 1), 130)
  ## the final fired at the 130th event time, not at the planned calendar 25
  event_times <- sort((d$enroll_time + d$pfs)[d$pfs_event == 1])
  expect_equal(t_final, event_times[130], tolerance = 1e-9)
  expect_false(isTRUE(all.equal(t_final, 25)))
})


test_that("action of a future milestone can be replaced, with fixed arguments", {

  new_action <- function(trial, tag) {
    trial$save(tag, 'final_marker')
  }

  tr <- run_two_milestones(
    function(trial) {
      update_milestone(trial, 'final', action = new_action, tag = 'updated')
    },
    calendarTime(time = 25),
    final_action = function(trial) { trial$save('original', 'final_marker') })

  expect_equal(unname(unlist(tr$get_output('final_marker'))), 'updated')
})


test_that("as-designed when/action are restored between replicates", {

  pbo <- make_arm("pbo", 10)
  trt <- make_arm("trt", 12)
  tr <- make_trial()
  add_arms(tr, sample_ratio = c(1, 1), pbo, trt)

  final_ms <- milestone(name = "final",
                        when = eventNumber(endpoint = 'pfs', n = 100))
  when_orig <- final_ms$.__enclos_env__$private$get_trigger_condition()
  action_orig <- final_ms$.__enclos_env__$private$get_action()

  interim <- milestone(name = "interim", when = calendarTime(time = 8),
                       action = function(trial) {
                         update_milestone(trial, 'final',
                                          when = eventNumber(endpoint = 'pfs',
                                                             n = 150),
                                          action = function(trial) invisible(NULL))
                       })
  lstn <- listener(silent = TRUE)
  lstn$add_milestones(interim, final_ms)
  ctrl <- controller(tr, lstn)

  ## two replicates run cleanly (updates re-applied each replicate)
  expect_no_error(ctrl$run(n = 2, silent = TRUE, plot_event = FALSE))

  ## after the last replicate the update from that replicate is in place ...
  expect_false(identical(final_ms$.__enclos_env__$private$get_trigger_condition(), when_orig))
  expect_false(identical(final_ms$.__enclos_env__$private$get_action(), action_orig))

  ## ... and reset() restores the as-designed milestone exactly
  lstn$reset()
  expect_identical(final_ms$.__enclos_env__$private$get_trigger_condition(), when_orig)
  expect_identical(final_ms$.__enclos_env__$private$get_action(), action_orig)
})


test_that("an update to the immediately next milestone is already effective", {

  pbo <- make_arm("pbo", 10)
  trt <- make_arm("trt", 12)
  tr <- make_trial()
  add_arms(tr, sample_ratio = c(1, 1), pbo, trt)

  m1 <- milestone(name = "m1", when = calendarTime(time = 5),
                  action = function(trial) {
                    update_milestone(trial, 'm2',
                                     when = calendarTime(time = 20))
                  })
  m2 <- milestone(name = "m2", when = calendarTime(time = 10))
  final <- milestone(name = "final", when = calendarTime(time = 30))
  lstn <- listener(silent = TRUE)
  lstn$add_milestones(m1, m2, final)
  controller(tr, lstn)$run(n = 1, silent = TRUE, plot_event = FALSE)

  expect_equal(unname(tr$get_milestone_time('m2')), 20)
})


test_that("update_milestone validates its arguments at scheduling time", {

  pbo <- make_arm("pbo", 10)
  trt <- make_arm("trt", 12)
  tr <- make_trial()
  add_arms(tr, sample_ratio = c(1, 1), pbo, trt)

  # cannot be called before any milestone has been triggered
  expect_error(
    tr$update_milestone('final', when = calendarTime(time = 5)),
    "within an action function")

  # emulate a triggered milestone to reach the argument checks
  pr <- tr$.__enclos_env__$private
  pr$set_current_time(5)
  pr$save_milestone_time(5, "checkpoint")

  expect_error(tr$update_milestone(c('a', 'b'), when = calendarTime(time = 9)),
               "single character")
  expect_error(tr$update_milestone('final'),
               "At least one of")
  expect_error(tr$update_milestone('final', when = 10),
               "created by functions")
  expect_error(tr$update_milestone('final', tag = 'x'),
               "only be used when action is")
  expect_error(tr$update_milestone('final', action = 'not a function'),
               "must be a function")
})


test_that("update_milestone validates its target at apply time", {

  # unknown milestone name
  expect_error(
    run_two_milestones(
      function(trial) {
        update_milestone(trial, 'no_such_milestone',
                         when = calendarTime(time = 20))
      },
      calendarTime(time = 25)),
    "not registered with the trial")

  # target already triggered: the final action tries to update the interim;
  # the error names the milestone whose action scheduled the update
  expect_error(
    run_two_milestones(
      doNothing,
      calendarTime(time = 25),
      final_action = function(trial) {
        update_milestone(trial, 'interim', when = calendarTime(time = 30))
      }),
    "called in the action function of milestone <final>.*already been triggered")

  # invalid signature of the replacement action surfaces at apply time
  expect_error(
    run_two_milestones(
      function(trial) {
        update_milestone(trial, 'final', action = function(x) 1)
      },
      calendarTime(time = 25)),
    "first argument")
})


test_that("a milestone action must be a function (NULL no longer allowed)", {

  expect_error(
    milestone(name = 'm', when = calendarTime(time = 5), action = NULL),
    "must be a function")
  expect_error(
    Milestones$new(name = 'm', trigger_condition = calendarTime(time = 5),
                   action = NULL),
    "must be a function")
})


test_that("old fixed arguments never leak into the replacement action", {

  ## the original final action carries a fixed argument tag = 'original';
  ## the replacement is supplied WITHOUT ... — if the old args leaked, the
  ## replacement would see tag = 'original' instead of its own default
  pbo <- make_arm("pbo", 10)
  trt <- make_arm("trt", 12)
  tr <- make_trial()
  add_arms(tr, sample_ratio = c(1, 1), pbo, trt)

  interim <- milestone(name = "interim", when = calendarTime(time = 8),
                       action = function(trial) {
                         update_milestone(trial, 'final',
                                          action = function(trial, tag = 'fallback') {
                                            trial$save(tag, 'marker')
                                          })
                       })
  final <- milestone(name = "final", when = calendarTime(time = 25),
                     action = function(trial, tag) { trial$save(tag, 'marker') },
                     tag = 'original')
  lstn <- listener(silent = TRUE)
  lstn$add_milestones(interim, final)
  controller(tr, lstn)$run(n = 1, silent = TRUE, plot_event = FALSE)

  expect_equal(unname(unlist(tr$get_output('marker'))), 'fallback')
})


test_that("a replacement action with a required argument must receive it", {

  expect_error(
    run_two_milestones(
      function(trial) {
        ## new action requires `tag` but ... is empty: caught at apply time
        update_milestone(trial, 'final',
                         action = function(trial, tag) trial$save(tag, 'm'))
      },
      calendarTime(time = 25)),
    "Missing required argument")
})


test_that("construction validation covers unnamed and unknown fixed arguments", {

  # unnamed extra arguments are rejected
  expect_error(
    milestone(name = 'm', when = calendarTime(time = 5),
              action = doNothing, 5),
    "must be named")

  # arguments unknown to the action are rejected
  expect_error(
    milestone(name = 'm', when = calendarTime(time = 5),
              action = function(trial, a) invisible(NULL), b = 1),
    "Unknown argument")
})


test_that("milestone setters defend against direct misuse", {

  # a triggered milestone rejects direct setter calls
  pbo <- make_arm("pbo", 10)
  trt <- make_arm("trt", 12)
  tr <- make_trial()
  add_arms(tr, sample_ratio = c(1, 1), pbo, trt)
  ms <- milestone(name = "only", when = calendarTime(time = 5))
  lstn <- listener(silent = TRUE)
  lstn$add_milestones(ms)
  controller(tr, lstn)$run(n = 1, silent = TRUE, plot_event = FALSE)

  expect_error(ms$set_trigger_condition(calendarTime(time = 9)),
               "already been triggered")
  expect_error(ms$set_action_function(doNothing),
               "already been triggered")

  # an untriggered milestone still validates the replacement itself
  ms2 <- milestone(name = "fresh", when = calendarTime(time = 5))
  expect_error(ms2$set_trigger_condition(10),
               "should be created by functions")
  expect_error(ms2$set_action_function("not a function"),
               "must be a function")
})


test_that("updating two future milestones commutes in call order", {

  build_and_run <- function(interim_action) {
    pbo <- make_arm("pbo", 10)
    trt <- make_arm("trt", 12)
    tr <- make_trial()
    add_arms(tr, sample_ratio = c(1, 1), pbo, trt)

    interim <- milestone(name = "interim", when = calendarTime(time = 8),
                         action = interim_action)
    m2 <- milestone(name = "m2", when = calendarTime(time = 12))
    final <- milestone(name = "final",
                       when = eventNumber(endpoint = 'pfs', n = 100))
    lstn <- listener(silent = TRUE)
    lstn$add_milestones(interim, m2, final)
    controller(tr, lstn)$run(n = 1, silent = TRUE, plot_event = FALSE)
    tr
  }

  tr_ab <- build_and_run(function(trial) {
    update_milestone(trial, 'm2', when = calendarTime(time = 10))
    update_milestone(trial, 'final',
                     when = eventNumber(endpoint = 'pfs', n = 120))
  })
  tr_ba <- build_and_run(function(trial) {
    update_milestone(trial, 'final',
                     when = eventNumber(endpoint = 'pfs', n = 120))
    update_milestone(trial, 'm2', when = calendarTime(time = 10))
  })

  expect_identical(tr_ab$get_output(), tr_ba$get_output())
  expect_identical(tr_ab$get_milestone_time(), tr_ba$get_milestone_time())
})


test_that("condition-update and action-update of one milestone commute", {

  new_act <- function(trial, tag) { trial$save(tag, 'marker') }

  build_and_run <- function(interim_action) {
    pbo <- make_arm("pbo", 10)
    trt <- make_arm("trt", 12)
    tr <- make_trial()
    add_arms(tr, sample_ratio = c(1, 1), pbo, trt)

    interim <- milestone(name = "interim", when = calendarTime(time = 8),
                         action = interim_action)
    final <- milestone(name = "final",
                       when = eventNumber(endpoint = 'pfs', n = 100))
    lstn <- listener(silent = TRUE)
    lstn$add_milestones(interim, final)
    controller(tr, lstn)$run(n = 1, silent = TRUE, plot_event = FALSE)
    tr
  }

  tr_wa <- build_and_run(function(trial) {
    update_milestone(trial, 'final',
                     when = eventNumber(endpoint = 'pfs', n = 120))
    update_milestone(trial, 'final', action = new_act, tag = 'x')
  })
  tr_aw <- build_and_run(function(trial) {
    update_milestone(trial, 'final', action = new_act, tag = 'x')
    update_milestone(trial, 'final',
                     when = eventNumber(endpoint = 'pfs', n = 120))
  })

  ## both requests are applied regardless of order ...
  d <- tr_wa$get_locked_data('final')
  expect_equal(sum(d$pfs_event == 1), 120)
  expect_equal(unname(unlist(tr_wa$get_output('marker'))), 'x')

  ## ... and the two orders yield identical simulation results
  expect_identical(tr_wa$get_output(), tr_aw$get_output())
  expect_identical(tr_wa$get_milestone_time(), tr_aw$get_milestone_time())
})


test_that("an update breaking chronological order is caught by the guard", {

  ## milestones must trigger in registration order; an update that makes a
  ## later-registered milestone fire before an earlier one aborts with the
  ## established monotonicity error of save_milestone_time()
  expect_error(
    {
      pbo <- make_arm("pbo", 10)
      trt <- make_arm("trt", 12)
      tr <- make_trial()
      add_arms(tr, sample_ratio = c(1, 1), pbo, trt)
      interim <- milestone(name = "interim", when = calendarTime(time = 8),
                           action = function(trial) {
                             update_milestone(trial, 'm2',
                                              when = calendarTime(time = 15))
                             update_milestone(trial, 'final',
                                              when = eventNumber(endpoint = 'pfs',
                                                                 n = 120))
                           })
      m2 <- milestone(name = "m2", when = calendarTime(time = 12))
      final <- milestone(name = "final",
                         when = eventNumber(endpoint = 'pfs', n = 100))
      lstn <- listener(silent = TRUE)
      lstn$add_milestones(interim, m2, final)
      controller(tr, lstn)$run(n = 1, silent = TRUE, plot_event = FALSE)
    },
    "happens before milestones")
})
