# Validation of the `arms` argument of eventNumber() and enrollment().
#
# The check is made once, at the entry shared by the C++ fast path and the
# pure-R fallback of the lock-time search, so both behave the same:
#   - a name that has never been an arm of the trial is an error;
#   - an arm removed before the milestone is evaluated is accepted, with a
#     warning unless the trial is silent, and its patients are counted as
#     specified;
#   - arms = NULL counts the arms in the trial at the time of evaluation,
#     without a warning.
# The set of known arms is every arm ever added in the current replicate,
# so an arm added and then removed within the trial is accepted too.

make_arm <- function(name, rate) {
  ep <- endpoint(name = 'pfs', type = 'tte', generator = rexp,
                 rate = log(2) / rate)
  a <- arm(name = name)
  a$add_endpoints(ep)
  a
}

make_trial <- function(seed = 11, n_patients = 400) {
  accrual <- data.frame(end_time = Inf, piecewise_rate = 20)
  trial(name = "t", n_patients = n_patients, seed = seed,
        enroller = StaggeredRecruiter, accrual_rate = accrual,
        dropout = rweibull, shape = 1, scale = 1e6,
        silent = TRUE)
}

with_cpp <- function(value, code) {
  old <- options(trialsimulator.use_cpp = value)
  on.exit(options(old), add = TRUE)
  force(code)
}

## three arms, trt1 removed at calendar time 8, then `final` is triggered by
## `when`. Returns the trial after the run. Messages of a non-silent run are
## suppressed so that only warnings reach the caller.
run_after_removal <- function(when, seed = 11, silent = TRUE) {
  tr <- make_trial(seed = seed)
  tr$mute(silent)
  add_arms(tr, sample_ratio = c(1, 1, 1),
           make_arm("pbo", 9), make_arm("trt1", 9), make_arm("trt2", 12))
  drop <- milestone(name = "drop", when = calendarTime(time = 8),
                    action = function(trial) remove_arms(trial, "trt1"))
  final <- milestone(name = "final", when = when)
  lstn <- listener(silent = TRUE)
  lstn$add_milestones(drop, final)
  suppressMessages(
    controller(tr, lstn)$run(n = 1, silent = silent, plot_event = FALSE)
  )
  tr
}

events_by_arm <- function(tr, milestone_name) {
  d <- tr$get_locked_data(milestone_name)
  tapply(d$pfs_event, d$arm, sum)
}

for (use_cpp in c(TRUE, FALSE)) {

  label <- if (use_cpp) "C++ path" else "R path"

  test_that(paste("eventNumber(arms = NULL) counts arms in the trial only, no warning:", label), {
    with_cpp(use_cpp, {
      tr_null <- NULL
      expect_no_warning(
        tr_null <- run_after_removal(eventNumber("pfs", n = 150))
      )
      tr_explicit <- run_after_removal(eventNumber("pfs", n = 150, arms = c("pbo", "trt2")))
      expect_equal(tr_null$get_milestone_time("final"),
                   tr_explicit$get_milestone_time("final"))
      ev <- events_by_arm(tr_null, "final")
      expect_equal(unname(ev["pbo"] + ev["trt2"]), 150)
    })
  })

  test_that(paste("eventNumber() warns on a removed arm and counts it as specified:", label), {
    with_cpp(use_cpp, {
      tr <- NULL
      expect_warning(
        tr <- run_after_removal(eventNumber("pfs", n = 150, arms = c("pbo", "trt1", "trt2")),
                                silent = FALSE),
        "Arm\\(s\\) <trt1> in arms of the triggering condition <eventNumber\\(\\)> were removed from the trial at time <8>"
      )
      ev <- events_by_arm(tr, "final")
      expect_equal(unname(sum(ev[c("pbo", "trt1", "trt2")])), 150)
      ## trt1 is censored at its removal, so it contributes its frozen count
      expect_true(ev["trt1"] > 0)
      d <- tr$get_locked_data("final")
      trt1 <- d[d$arm == "trt1", ]
      expect_true(all(trt1$enroll_time + trt1$pfs <= 8 + 1e-8 | trt1$pfs_event == 0))
    })
  })

  test_that(paste("the removed-arm warning is muted in a silent run:", label), {
    with_cpp(use_cpp, {
      tr <- NULL
      expect_no_warning(
        tr <- run_after_removal(eventNumber("pfs", n = 150, arms = c("pbo", "trt1", "trt2")),
                                silent = TRUE)
      )
      ev <- events_by_arm(tr, "final")
      expect_equal(unname(sum(ev[c("pbo", "trt1", "trt2")])), 150)
    })
  })

  test_that(paste("eventNumber() rejects an arm never in the trial:", label), {
    with_cpp(use_cpp, {
      expect_error(
        run_after_removal(eventNumber("pfs", n = 150, arms = c("pbo", "trt9"))),
        "Arm\\(s\\) <trt9> in arms of the triggering condition have never been in the trial"
      )
    })
  })

  test_that(paste("enrollment() warns on a removed arm and rejects an unknown one:", label), {
    with_cpp(use_cpp, {
      tr <- NULL
      expect_warning(
        tr <- run_after_removal(enrollment(n = 250, arms = c("pbo", "trt1", "trt2")),
                                silent = FALSE),
        "Arm\\(s\\) <trt1> in arms of the triggering condition <enrollment\\(\\)> were removed from the trial at time <8>"
      )
      d <- tr$get_locked_data("final")
      expect_equal(nrow(d), 250)
      expect_error(
        run_after_removal(enrollment(n = 250, arms = c("pbo", "trt9"))),
        "Arm\\(s\\) <trt9> in arms of the triggering condition have never been in the trial"
      )
    })
  })

  test_that(paste("an arm added and removed within the trial is accepted, with a warning:", label), {
    with_cpp(use_cpp, {
      tr <- make_trial()
      add_arms(tr, sample_ratio = c(1, 1), make_arm("pbo", 9), make_arm("trt1", 12))
      add <- milestone(name = "add", when = calendarTime(time = 5),
                       action = function(trial) add_arms(trial, sample_ratio = 1, make_arm("trt3", 10)))
      drop <- milestone(name = "drop", when = calendarTime(time = 12),
                        action = function(trial) remove_arms(trial, "trt3"))
      final <- milestone(name = "final",
                         when = eventNumber("pfs", n = 120, arms = c("pbo", "trt3")))
      lstn <- listener(silent = TRUE)
      lstn$add_milestones(add, drop, final)
      expect_warning(
        suppressMessages(
          controller(tr, lstn)$run(n = 1, silent = FALSE, plot_event = FALSE)
        ),
        "Arm\\(s\\) <trt3> in arms of the triggering condition <eventNumber\\(\\)> were removed from the trial at time <12>"
      )
      ev <- events_by_arm(tr, "final")
      expect_equal(unname(ev["pbo"] + ev["trt3"]), 120)
    })
  })
}
