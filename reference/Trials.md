# Class of Trial

Create a class of trial.

Public methods in this R6 class are used in developing this package.
Thus, we have to export the whole R6 class which exposures all public
methods. However, only the public methods in the sections below are
useful to end users, and users are encouraged to restrict themselves to
them. The remaining public methods are internal machinery of the package
and should not be called directly (see the last section).

**Adaptation methods.** The following methods adapt an ongoing trial and
should be called within action functions of milestones. Each of them has
a user-friendly wrapper of the same name, e.g.,
`set_duration(trial, ...)` for `trial$set_duration(...)`.

- `$set_duration()` set duration of a trial. This function can be used
  to extend duration under adaptive designs.

- `$resize()` set maximum sample size of a trial. This function can be
  used to increase sample size under adaptive designs (e.g., sample size
  reassessment).

- `$remove_arms()` drop arms from a trial. This function can be used in
  adaptive designs, e.g., dose selection, enrichment design, etc.

- `$update_sample_ratio()` change sample ratio of arm. This function can
  be used under adaptive designs, e.g., response-adaptive design, etc.

- `$update_generator()` change endpoint generator of arm. This function
  can be used in enrichment design.

- `$add_arms()` add arms to a trial. This function is used to add arms
  to a newly defined trial, or add arms under adaptive design, e.g.,
  dose-ranging, etc.

- `$crossover()` apply a milestone-triggered crossover to eligible
  patients in the trial. Called inside a milestone action; only alters
  patients' post-switch endpoint values and leaves already-observed data
  intact.

- `$stop_followup()` stop follow-up of a subset of enrolled patients at
  or after a milestone. Their data are censored (time-to-event
  endpoints) or set to missing (non-time-to-event endpoints)
  accordingly. This function can be used in adaptive designs, e.g., to
  simulate treatment discontinuation, early termination of follow-up for
  a sub-population, or enrichment design where follow-up of a
  de-selected sub-population is stopped after an interim analysis. It
  can also stop follow-up of an earlier cohort at a pre-specified,
  event-driven milestone (e.g., the last patient of the first cohort),
  optionally with a fixed additional follow-up beyond it, making
  statistics of the cohorts independent to facilitate, e.g., combination
  tests.

- `$update_accrual_rate()` update the accrual rate of the recruitment
  curve at a milestone, e.g., to revise or pause recruitment after dose
  selection or enrichment. `end_time` of the new accrual rate is
  measured from the milestone; patients not yet enrolled are re-planned
  and re-randomized under the new schedule.

- `$update_milestone()` update the trigger condition and/or the action
  of a not-yet-triggered milestone. The update takes effect right after
  the current action function returns.

**Methods callable within action functions.** In addition to the
adaptation methods above, users can call the following methods within
action functions to access and manipulate data, to query the current
status of a trial, and to carry out statistical testing.

Data access and manipulation:

- `$get_locked_data()` request for data snapshot at a milestone. Calling
  this function is recommended as the first action in any action
  function as long as trial data is needed in statistical analysis or
  decision making.

- `$save()` save intermediate result for simulation summary. Results
  across multiple replicates of simulation are saved, which can be
  retrieved by calling `get_output()` anytime.

- `$bind()` row bind and save intermediate results across milestones if
  those results are data frames of similar formats. The life cycle of
  the save results is within a single replicate of simulation and is
  reset to NULL in next simulated trial. Saved results can be retrieved
  by calling [`get()`](https://rdrr.io/r/base/get.html) anytime.

- `$save_custom_data()` save intermediate results of any format. The
  life cycle of the saved result is within a single replicate of
  simulation and is reset to NULL in next simulated trial. Saved results
  can be retrieved by calling [`get()`](https://rdrr.io/r/base/get.html)
  anytime.

- `$get()` retrieve intermediate results saved by calling functions
  `save_custom_data()` or `bind()`.

- `$get_output()` retrieve intermediate results saved by calling
  function [`save()`](https://rdrr.io/r/base/save.html).

Trial status queries:

- `$get_current_time()` return the triggering time of the milestone that
  the calling action function is attached to.

- `$get_milestone_time()` return milestone time when triggering a given
  milestone.

- `$get_sample_ratio()` return current sample ratios of arms.

- `$get_arms_name()` return names of the arms in the trial at the time
  of calling, i.e., arms that have been added and not yet removed by
  `$remove_arms()`. Note that this can differ from the arms present in
  locked data, where data of removed arms remain available (censored at
  the time of removal).

Statistical testing:

- `$dunnettTest()` perform Dunnett's test.

- `$closedTest()` perform combination test based on Dunnett's test.

- `$conditionalPower()` compute conditional power at a triggered interim
  milestone for each treatment-vs-placebo comparison of a time-to-event
  endpoint, under a design with one interim and one final analysis and a
  constant allocation ratio.

**Trial setup.**

- `$add_regimen()` register a `regimen` object to a trial. Must be
  called before `$add_arms()`. Applied at enrollment. Unlike the
  adaptation methods above, it belongs to the setup stage of a trial and
  must not be called within action functions.

**Internal machinery.** The remaining public methods (`$lock_data()`,
`$get_data_lock_time_by_calendar_time()`,
`$get_data_lock_time_by_event_number()`,
`$get_data_lock_time_by_enrollment()`, `$has_arm()`, `$event_plot()`,
`$mute()`, `$reset()`, `$make_arms_snapshot()` and
`$pop_milestone_updates()`) are public only because they are invoked on
a trial object by other components of the package (milestones,
listeners, controllers and triggering conditions), which R6 cannot grant
through private members. Users should not call them directly. Note that
`$save()` and `$get_output()` are invoked by those components too, but
they are part of the user-facing API above at the same time.

## Value

an `R6Class` generator object; use
[`trial()`](https://zhangh12.github.io/TrialSimulator/reference/trial.md)
to create a trial.

## Methods

### Public methods

- [`Trials$new()`](#method-Trials-new)

- [`Trials$set_duration()`](#method-Trials-set_duration)

- [`Trials$resize()`](#method-Trials-resize)

- [`Trials$remove_arms()`](#method-Trials-remove_arms)

- [`Trials$update_sample_ratio()`](#method-Trials-update_sample_ratio)

- [`Trials$update_generator()`](#method-Trials-update_generator)

- [`Trials$add_arms()`](#method-Trials-add_arms)

- [`Trials$crossover()`](#method-Trials-crossover)

- [`Trials$stop_followup()`](#method-Trials-stop_followup)

- [`Trials$update_accrual_rate()`](#method-Trials-update_accrual_rate)

- [`Trials$update_milestone()`](#method-Trials-update_milestone)

- [`Trials$get_locked_data()`](#method-Trials-get_locked_data)

- [`Trials$save()`](#method-Trials-save)

- [`Trials$get_output()`](#method-Trials-get_output)

- [`Trials$bind()`](#method-Trials-bind)

- [`Trials$save_custom_data()`](#method-Trials-save_custom_data)

- [`Trials$get_custom_data()`](#method-Trials-get_custom_data)

- [`Trials$get()`](#method-Trials-get)

- [`Trials$get_current_time()`](#method-Trials-get_current_time)

- [`Trials$get_milestone_time()`](#method-Trials-get_milestone_time)

- [`Trials$get_sample_ratio()`](#method-Trials-get_sample_ratio)

- [`Trials$get_arms_name()`](#method-Trials-get_arms_name)

- [`Trials$dunnettTest()`](#method-Trials-dunnettTest)

- [`Trials$closedTest()`](#method-Trials-closedTest)

- [`Trials$conditionalPower()`](#method-Trials-conditionalPower)

- [`Trials$add_regimen()`](#method-Trials-add_regimen)

- [`Trials$lock_data()`](#method-Trials-lock_data)

- [`Trials$get_data_lock_time_by_calendar_time()`](#method-Trials-get_data_lock_time_by_calendar_time)

- [`Trials$get_data_lock_time_by_event_number()`](#method-Trials-get_data_lock_time_by_event_number)

- [`Trials$get_data_lock_time_by_enrollment()`](#method-Trials-get_data_lock_time_by_enrollment)

- [`Trials$has_arm()`](#method-Trials-has_arm)

- [`Trials$event_plot()`](#method-Trials-event_plot)

- [`Trials$mute()`](#method-Trials-mute)

- [`Trials$reset()`](#method-Trials-reset)

- [`Trials$make_arms_snapshot()`](#method-Trials-make_arms_snapshot)

- [`Trials$pop_milestone_updates()`](#method-Trials-pop_milestone_updates)

- [`Trials$print()`](#method-Trials-print)

- [`Trials$clone()`](#method-Trials-clone)

------------------------------------------------------------------------

### Method `new()`

initialize a trial

#### Usage

    Trials$new(
      name,
      n_patients,
      duration,
      description = name,
      seed = NULL,
      enroller = StaggeredRecruiter,
      dropout = NULL,
      stratification_factors = NULL,
      silent = FALSE,
      ...
    )

#### Arguments

- `name`:

  character. Name of trial. Usually, hmm..., useless.

- `n_patients`:

  integer. Maximum (and initial) number of patients could be enrolled
  when planning the trial. It can be altered adaptively during a trial.

- `duration`:

  Numeric. Trial duration. It can be altered adaptively during a trial.

- `description`:

  character. Optional for description of the trial. By default it is set
  to be trial's `name`. Usually useless.

- `seed`:

  random seed. If `NULL`, seed is set for each simulated trial
  automatically and saved in output. It can be retrieved in the `seed`
  column in `$get_output()`. Setting it to be `NULL` is recommended. For
  debugging, set it to a specific integer.

- `enroller`:

  enrollment-time generator. Must be `StaggeredRecruiter` (the default);
  any other value is rejected. Supply its `accrual_rate` via `...`. See
  [`?TrialSimulator::StaggeredRecruiter`](https://zhangh12.github.io/TrialSimulator/reference/StaggeredRecruiter.md).
  Kept (rather than dropped) for backward compatibility, so existing
  code that passes `enroller = StaggeredRecruiter` explicitly keeps
  working unchanged.

- `dropout`:

  a function returning a vector of dropout time for patients. It can be
  any random number generator with first argument `n`, the number of
  enrolled patients. Usually `rexp` if dropout rate is set at a single
  time point, or `rweibull` if dropout rates are set at two time points.
  See
  [`?TrialSimulator::weibullDropout`](https://zhangh12.github.io/TrialSimulator/reference/weibullDropout.md).

- `stratification_factors`:

  character. Names of baseline characteristics to define stratums in
  stratified permuted block randomization. Stratification factors must
  be defined in
  [`endpoint()`](https://zhangh12.github.io/TrialSimulator/reference/endpoint.md)
  with `readout = 0`. As a natural assumption for randomized trial,
  `TrialSimulator` assumes that the baseline characteristics share the
  same distribution across arms, but endpoints can have same or
  different distributions given baseline characteristics. `NULL` by
  default, i.e., unstratified permuted block randomization is executed.

- `silent`:

  logical. `TRUE` to mute messages. However, warning message is still
  displayed.

- `...`:

  (optional) arguments of `enroller` and `dropout`.

------------------------------------------------------------------------

### Method [`set_duration()`](https://zhangh12.github.io/TrialSimulator/reference/set_duration.md)

set trial duration in an adaptive designed trial. All patients enrolled
before resetting the duration are truncated (non-tte endpoints) or
censored (tte endpoints) at the original duration. Remaining patients
are re-randomized. New duration must be longer than the old one.

#### Usage

    Trials$set_duration(duration)

#### Arguments

- `duration`:

  new duration of a trial. It must be greater than the current duration.

------------------------------------------------------------------------

### Method [`resize()`](https://zhangh12.github.io/TrialSimulator/reference/resize.md)

resize a trial with a greater sample size. This function is used to
update the maximum sample size adaptively after sample size
reassessment. Note that this function should be called within action
functions. It is users' responsibility to ensure it and `TrialSimulator`
has no way to track this.

#### Usage

    Trials$resize(n_patients)

#### Arguments

- `n_patients`:

  integer. Number of maximum sample size of a trial.

------------------------------------------------------------------------

### Method [`remove_arms()`](https://zhangh12.github.io/TrialSimulator/reference/remove_arms.md)

remove arms from a trial. `enroll_patients()` will be called at the end
of this function to enroll all remaining patients after
`Trials$get_current_time()`, i.e. no more unenrolled patients could be
randomized to removed arms. This function may be used with futility
analysis, dose selection, enrichment analysis (sub-population) or
interim analysis (early stop for efficacy).

Note that this function should only be called within action functions.
It is users' responsibility to ensure it and `TrialSimulator` has no way
to track this. In addition, data of the removed arms are censored or
truncated by the time of arm removal.

#### Usage

    Trials$remove_arms(arms_name)

#### Arguments

- `arms_name`:

  character vector. Name of arms to be removed.

------------------------------------------------------------------------

### Method [`update_sample_ratio()`](https://zhangh12.github.io/TrialSimulator/reference/update_sample_ratio.md)

update sample ratios of arms. This could happen after an arm is added or
removed. Note that we may update sample ratios of unaffected arms as
well. Once sample ratio is updated, trial data should be rolled back
with updated randomization queue. Data of unenrolled patients are
re-sampled as well.

#### Usage

    Trials$update_sample_ratio(arm_names, sample_ratios)

#### Arguments

- `arm_names`:

  character vector. Name of arms.

- `sample_ratios`:

  numeric vector. New sample ratios of arms. If sample ratio is a whole
  number, the permuted block randomization is adopted; otherwise,
  [`sample()`](https://rdrr.io/r/base/sample.html) will be used instead,
  which can cause imbalance between arms by chance. However, this is
  fine for simulation.

------------------------------------------------------------------------

### Method [`update_generator()`](https://zhangh12.github.io/TrialSimulator/reference/update_generator.md)

update endpoint generator in an arm

#### Usage

    Trials$update_generator(arm_name, endpoint_name, generator, ...)

#### Arguments

- `arm_name`:

  character. Name of an arm.

- `endpoint_name`:

  character. A vector of endpoint names whose generator is updated.

- `generator`:

  a random number generation (RNG) function. See `generator` of
  [`endpoint()`](https://zhangh12.github.io/TrialSimulator/reference/endpoint.md).

- `...`:

  optional arguments for `generator`.

------------------------------------------------------------------------

### Method [`add_arms()`](https://zhangh12.github.io/TrialSimulator/reference/add_arms.md)

add one or more arms to the trial. `enroll_patients()` will be called at
the end to enroll all remaining patients in
`private$randomization_queue`. This function can be used in two
scenarios: (1) add arms right after a trial is created (i.e.,
`Trials$new(...)`). `sample_ratio` and arms added through `...` should
be of same length; (2) add arms to a trial already with arm(s).

Note that this function should only be called within action functions.
It is users' responsibility to ensure it and `TrialSimulator` has no way
to track this.

#### Usage

    Trials$add_arms(sample_ratio, ...)

#### Arguments

- `sample_ratio`:

  integer vector. Sample ratio for permuted block randomization. It will
  be appended to existing sample ratio in the trial.

- `...`:

  one or more objects returned from
  [`arm()`](https://zhangh12.github.io/TrialSimulator/reference/arm.md).
  Randomization is carried out with updated sample ratio of newly added
  arm. It rolls back all patients after `Trials$get_current_time()`,
  i.e. redo randomization for those patients. This can be useful to add
  arms one by one when creating a trial. Note that we can run
  `Trials$add_arm(sample_ratio1, arm1)` followed by
  `Trials$add_arm(sample_ratio2, arm2)`. We would expected similar
  result with
  `Trials$add_arms(c(sample_ratio1, sample_ratio2), arm1, arm2)`. Note
  that these two method won't return exactly the same trial because
  randomization_queue were generated twice in the first approach but
  only once in the second approach. But statistically, they are
  equivalent and of the same distribution.

------------------------------------------------------------------------

### Method [`crossover()`](https://zhangh12.github.io/TrialSimulator/reference/crossover.md)

Apply a milestone-triggered crossover to eligible patients in the trial.

Unlike a regimen registered via `add_regimen()` (applied at enrollment),
[`crossover()`](https://zhangh12.github.io/TrialSimulator/reference/crossover.md)
is meant to be called inside a milestone's action function. At the
earliest crossover (calendar) time `T = get_current_time() + delay`,
eligible patients may switch to a new treatment, and only their
*post-switch* endpoint values are altered. The triplet is stacked onto
the trial's regimen (so it is also re-applied to patients enrolled
later), and applied immediately, in place, to all currently-eligible
patients.

Eligibility (the pool passed to `what()`) = patients with at least one
endpoint still "open" (unobserved, dropout-/duration-aware) at `T`;
fully-observed patients are excluded. `when()` must return a switch time
with `enroll_time + switch_time >= T` (a crossover cannot predate its
opening), otherwise an error is raised. `how()` may only change
post-switch outcomes; returning a changed value for a pre-switch/locked
cell raises an error.

Two helper columns are injected into `patient_data` for the triplet
functions: `earliest_crossover_calendar_time` (= `T`) and
`earliest_crossover_time_from_enrollment` (= `max(T - enroll_time, 0)`).

#### Usage

    Trials$crossover(what, how, when = NULL, delay = 0, ...)

#### Arguments

- `what`:

  a function selecting which eligible patients crossover and to what
  `new_treatment` (`NA` = no crossover). See
  [`regimen()`](https://zhangh12.github.io/TrialSimulator/reference/regimen.md).

- `how`:

  a function returning the modified post-switch endpoint values.

- `when`:

  (optional) a function returning `switch_time` from enrollment. If
  `NULL` (default), patients switch at `T`
  (`switch_time = earliest_crossover_time_from_enrollment`).

- `delay`:

  numeric. Time after the milestone before crossover opens;
  `T = get_current_time() + delay`. Default `0`.

- `...`:

  (optional) named arguments routed to `what`, `when`, and/or `how`.

------------------------------------------------------------------------

### Method [`stop_followup()`](https://zhangh12.github.io/TrialSimulator/reference/stop_followup.md)

stop follow-up of a subset of patients at a specified time at or after
the current milestone. Data of affected patients are censored
(time-to-event endpoints) or set to missing (non-time-to-event endpoints
with readout after the stopping time), as if those patients were no
longer followed since then. This function can be used in adaptive
designs, e.g., to simulate treatment discontinuation, early termination
of follow-up for a sub-population, or enrichment design where follow-up
of a de-selected sub-population is stopped after an interim analysis. It
can also be called at a pre-specified milestone that splits a trial into
cohorts, e.g., a milestone marking the last patient of the first cohort
and the first patient of the second cohort. Such a milestone is usually
event driven, so its time is unknown until the trial is simulated.
Stopping follow-up of the earlier cohort at that milestone, or after a
pre-specified, fixed `additional_followup` beyond it, makes statistics
computed from the two cohorts independent, which facilitates tests
requiring independence, e.g., combination tests.

Only patients who are enrolled by the time this function is called and
satisfy the subset conditions in `...` (if any) are affected. Patients
enrolled afterwards are followed as usual.

Note that this function should only be called within action functions.
It is users' responsibility to ensure it and `TrialSimulator` has no way
to track this. Calling it before any milestone has been triggered is an
error.

#### Usage

    Trials$stop_followup(..., additional_followup = 0)

#### Arguments

- `...`:

  subset conditions compatible with
  [`dplyr::filter`](https://dplyr.tidyverse.org/reference/filter.html).
  Follow-up is stopped for selected patients only. If no condition is
  provided, follow-up is stopped for all patients enrolled by the time
  this function is called.

- `additional_followup`:

  numeric. Extra follow-up time granted to the selected patients after
  the current milestone. If 0 (default), follow-up stops at the
  milestone itself.

------------------------------------------------------------------------

### Method [`update_accrual_rate()`](https://zhangh12.github.io/TrialSimulator/reference/update_accrual_rate.md)

update the accrual rate of the recruitment curve at a milestone. The
enroller of a trial is always `StaggeredRecruiter`; this function
replaces its `accrual_rate` for patients not yet enrolled, while
enrolled patients are left unchanged. It can be used in adaptive
designs, e.g., to revise recruitment after dose selection or enrichment,
or to pause recruitment for a period after an interim decision.

`end_time` in `accrual_rate` is measured from the time this function is
called (i.e., the current milestone), not from the start of the trial. A
milestone is usually event driven, so its calendar time is unknown until
the trial is simulated, and a schedule on the calendar time scale could
not be pre-specified. Measuring `end_time` from the milestone also lets
users state the new plan simply as "from now on": e.g.,
`data.frame(end_time = c(3, Inf), piecewise_rate = c(20, 35))` means 20
patients per month for the 3 months following the milestone and 35 per
month thereafter, whenever the milestone occurs. Following the
convention of `StaggeredRecruiter`, the first re-planned patient is
enrolled `1 / piecewise_rate` after the milestone; a leading window with
`piecewise_rate = 0` defers enrollment further. As with other
adaptations, patients not yet enrolled are re-randomized and their data
are regenerated under the new schedule.

Note that this function should only be called within action functions.
Calling it before any milestone has been triggered is an error.

#### Usage

    Trials$update_accrual_rate(accrual_rate)

#### Arguments

- `accrual_rate`:

  a data frame of columns `end_time` and `piecewise_rate` as in
  `StaggeredRecruiter`, with `end_time` measured from the current
  milestone. The last `end_time` must be `Inf` with a positive rate.

------------------------------------------------------------------------

### Method [`update_milestone()`](https://zhangh12.github.io/TrialSimulator/reference/update_milestone.md)

update the trigger condition and/or the action of a not-yet-triggered
milestone at a milestone. The milestone to be updated is identified by
its name, which cannot be changed. This function can be used in adaptive
designs, e.g., when conditional power at an interim analysis is lower
than expected, the final analysis can be postponed by increasing the
target number of events in its triggering condition, or its triggering
condition can be switched from a calendar time to an event count
entirely.

The update is not applied immediately: it is queued and takes effect
right after the current action function returns, before the next
milestone is evaluated. The new trigger condition and action replace the
old ones as a whole. Between simulation replicates the milestone is
restored to its as-designed trigger condition and action, so every
replicate starts from the original design. A milestone that has already
been triggered cannot be updated.

Note that this function should only be called within action functions.
Calling it before any milestone has been triggered is an error. Also
note that milestones must trigger in their registration order: an
updated triggering condition that makes a later-registered milestone
fire before an earlier one stops the simulation with an error.

#### Usage

    Trials$update_milestone(name, when = NULL, action = NULL, ...)

#### Arguments

- `name`:

  character. Name of the milestone to be updated. It must be registered
  with the listener and not yet triggered.

- `when`:

  (optional) new triggering condition, an object returned by
  [`calendarTime()`](https://zhangh12.github.io/TrialSimulator/reference/calendarTime.md),
  [`enrollment()`](https://zhangh12.github.io/TrialSimulator/reference/enrollment.md),
  [`eventNumber()`](https://zhangh12.github.io/TrialSimulator/reference/eventNumber.md)
  or their combinations using `&` and `|`. If `NULL`, the triggering
  condition is left unchanged.

- `action`:

  (optional) new action function. See `action` of
  [`milestone()`](https://zhangh12.github.io/TrialSimulator/reference/milestone.md).
  If `NULL`, the action is left unchanged.

- `...`:

  (optional) named arguments of the new `action`. Only allowed when
  `action` is provided. The new action is executed with exactly the
  arguments supplied here: fixed arguments of the previous action are
  never carried over.

------------------------------------------------------------------------

### Method `get_locked_data()`

return locked data, i.e. snapshot at a milestone. TTE data is censored
and non-TTE data is truncated accounting for readout time and dropout
time simultaneously by the triggering time of milestone.

#### Usage

    Trials$get_locked_data(milestone_name)

#### Arguments

- `milestone_name`:

  character. Milestone name of which the locked data to be extracted.

------------------------------------------------------------------------

### Method [`save()`](https://rdrr.io/r/base/save.html)

save a single value or a one-row data frame to trial's output for
further analysis/summary later. Results saved by calling this function
have a life cycle of the whole simulation. This means that all results
are accumulated across multiple simulated trial and can be used for
summary later.

#### Usage

    Trials$save(value, name = "", overwrite = FALSE)

#### Arguments

- `value`:

  value to be saved. It can be a scalar (vector of length 1) or a data
  frame (of one row).

- `name`:

  character to name the saved object. It will be used to name a column
  in trial's output if `value` is a scalar. If `value` is a data frame,
  `name` will be the prefix pasted with the column name of `value` in
  trial's output. If user want to use `value`'s column name as is in
  trial's output, set `name` to be `''` as default. Otherwise, column
  name would be, e.g., `"{<name>}_<{colnames(value)}>"`.

- `overwrite`:

  logic. `TRUE` if overwriting existing entries with warning, otherwise,
  throwing an error and stop.

------------------------------------------------------------------------

### Method `get_output()`

return a data frame of all current outputs saved by calling
`Trials$save()`. Usually this function is call at the end of simulation
for summary.

#### Usage

    Trials$get_output(cols = NULL, simplify = TRUE, tidy = FALSE)

#### Arguments

- `cols`:

  columns to be returned from `Trial$output`. If `NULL`, all columns are
  returned.

- `simplify`:

  logical. Return value rather than a data frame of one column when
  `length(col) == 1` and `simplify == TRUE`.

- `tidy`:

  logical. `TrialSimulator` automatically records a set of standard
  outputs at milestones, even when `doNothing` is used as action
  functions. These includes time of triggering milestones, number of
  observed events for time-to-event endpoints, and number of non-missing
  readouts for non-TTE endpoints (see
  [`vignette('actionFunctions')`](https://zhangh12.github.io/TrialSimulator/articles/actionFunctions.md)).
  This usually mean a large number of columns in outputs. If users have
  no intent to summarize a trial on these columns, setting `tidy = TRUE`
  can eliminate these columns from `get_output()`. Note that currently
  we use regex `"^n_events_<.*?>_<.*?>$"` and `"^milestone_time_<.*?>$"`
  to match columns to be eliminated. If users plan to use `tidy = TRUE`,
  caution is needed when naming custom outputs in
  [`save()`](https://rdrr.io/r/base/save.html). Default `FALSE`.

------------------------------------------------------------------------

### Method `bind()`

row bind a data frame to existing data frame. If a data frame `name` is
not existing in a trial, then it is equivalent to calling
`Trials$save_custom_data()`. Extra columns in `value` are ignored.
Columns in `Trials$custom_data[[name]]` but not in `value` are filled
with `NA`.

This function can be used to save results across multiple milestones.
For example, p-values and effect estimates of endpoints may be computed
at multiple milestones. Users may want to bind them into a data frame
for combination test or graphical test. In this case, this function can
be called repeatedly in milestones. Once the data frame is fully
conducted, statistical test can be performed on its final version
retrieved by calling `Trials$get()`.

Note that data saved by calling this function has a short life cycle
within a single simulated trial. It will be reset to `NULL` before
simulated another trial. Thus, it cannot be used to save results that
are used for summarizing the simulation.

#### Usage

    Trials$bind(value, name)

#### Arguments

- `value`:

  a data frame to be saved. It can consist of one or multiple rows.

- `name`:

  character. Name of object to be saved.

------------------------------------------------------------------------

### Method `save_custom_data()`

save arbitrary (number of) objects into a trial so that users can use
those to control the workflow. Most common use case is to store
simulation parameters to be used in action functions.

#### Usage

    Trials$save_custom_data(value, name, overwrite = FALSE)

#### Arguments

- `value`:

  value to be saved. Any type.

- `name`:

  character. Name of the value to be accessed later.

- `overwrite`:

  logic. `TRUE` if overwriting existing entries with warning, otherwise,
  throwing an error and stop.

------------------------------------------------------------------------

### Method `get_custom_data()`

return custom data saved by calling `Trials$save_custom_data()` or
`Trials$bind()` with its name.

#### Usage

    Trials$get_custom_data(name)

#### Arguments

- `name`:

  character. Name of custom data to be accessed.

------------------------------------------------------------------------

### Method [`get()`](https://rdrr.io/r/base/get.html)

alias of function `get_custom_data` to make it short and cool.

#### Usage

    Trials$get(name)

#### Arguments

- `name`:

  character. Name of custom data to be accessed.

------------------------------------------------------------------------

### Method `get_current_time()`

return current time of a trial

#### Usage

    Trials$get_current_time()

------------------------------------------------------------------------

### Method `get_milestone_time()`

return milestone time when triggering a given milestone

#### Usage

    Trials$get_milestone_time(milestone_name = NULL)

#### Arguments

- `milestone_name`:

  character. Name of milestone. If `NULL`, time of all triggered
  milestones are returned.

------------------------------------------------------------------------

### Method `get_sample_ratio()`

return current sample ratio of the trial. The ratio can probably change
during the trial (e.g., arm is removed or added)

#### Usage

    Trials$get_sample_ratio(arm_names = NULL)

#### Arguments

- `arm_names`:

  character vector of arms.

------------------------------------------------------------------------

### Method `get_arms_name()`

return arms' name of trial

#### Usage

    Trials$get_arms_name()

------------------------------------------------------------------------

### Method `dunnettTest()`

carry out closed test based on Dunnett method under group sequential
design.

#### Usage

    Trials$dunnettTest(
      formula,
      placebo,
      treatments,
      milestones,
      alternative,
      planned_info,
      ...
    )

#### Arguments

- `formula`:

  An object of class `formula` that can be used with
  [`survival::coxph`](https://rdrr.io/pkg/survival/man/coxph.html). Must
  consist `arm` and endpoint in `data`. No covariate is allowed.
  Stratification variables are supported and can be added using
  `strata(...)`.

- `placebo`:

  character. Name of placebo arm.

- `treatments`:

  character vector. Name of treatment arms to be used in comparison.

- `milestones`:

  character vector. Names of triggered milestones at which either
  adaptation is applied or statistical testing for endpoint is
  performed. Milestones in `milestones` does not need to be sorted by
  their triggering time.

- `alternative`:

  a character string specifying the alternative hypothesis, must be one
  of `"greater"` or `"less"`. No default value. `"greater"` means
  superiority of treatment over placebo is established by an hazard
  ratio greater than 1 when a log-rank test is used.

- `planned_info`:

  a data frame of planned number of events of time-to-event endpoint in
  each stage and each arm. Milestone names, i.e., `milestones` are row
  names of `planned_info`, and arm names, i.e., `c(placebo, treatments)`
  are column names. Note that it is not the accumulative but stage-wise
  event numbers. It is usually not easy to determine these numbers in
  practice, simulation may be used to get estimates. Note:
  `planned_info` can also be a character `"default"` so that
  `planned_info` are set to be number of newly randomized patients in
  the control arm in each of the stages. This assumes that event rate do
  not change over time and/or sample ratio between placebo and a
  treatment arm does not change as well, which may not be true. It is
  for the purpose of debugging or rapid implementation only. Using
  simulation to pick `planned_info` is recommended in formal simulation
  study. Another issue with `planned_info` set to be `"default"` is that
  it is possible patient recruitment is done before a specific stage, as
  a result, `planned_info` is zero which can crash the program.

- `...`:

  subset condition that is compatible with
  [`dplyr::filter`](https://dplyr.tidyverse.org/reference/filter.html).
  `survdiff` will be fitted on this subset only to compute one-sided
  logrank statistics. It could be useful when comparison is made on a
  subset of treatment arms. By default it is not specified, all data
  (placebo plus one treatment arm at a time) in the locked data are used
  to fit the model.

#### Details

This function computes stage-wise p-values for each of the intersection
hypotheses based on Dunnett test. If only one treatment arm is present,
it is equivalent to compute the stage-wise p-values of elemental
hypotheses. This function also computes inverse normal combination test
statistics at each of the stages. The choice of `planned_info` can
affect the calculation of stage-wise p-values. Specifically, it is used
to compute the columns `observed_info` and `p_inverse_normal` in
returned data frame, which will be used in `Trial$closedTest()`. The
choice of `planned_info` can affect the result of `Trial$closedTest()`
so user should chose it with caution.

Note that in `Trial$closedTest()`, `observed_info`, which is derived
from `planned_info`, will lead to the same closed testing results up to
a constant. This is because the closed test uses information fraction
`observed_info/sum(observed_info)`. As a result, setting `planned_info`
to, e.g., `10 * planned_info` should give same closed test results.

Based on numerical study, setting `planned_info = "default"` leads to a
much higher power (roughly 10%) than setting `planned_info` to median of
event numbers at stages, which can be determined by simulation. I am not
sure if regulator would support such practice. For example, if a
milestone (e.g., interim analysis) is triggered at a pre-specified
calendar time, the number of randomized patients is random and is
unknown when planning the trial. If I understand it correctly, regulator
may want the information fraction in closed test (combined with Dunnett
test) to be pre-fixed. In addition, this choice for `planned_info`
assumes that the event rates does not change over time which is
obviously not true. It is recommended to always use pre-fixed
`planned_info` for restrict control of family-wise error rate. It should
be pointed out that the choice of pre-fixed `planned_info` can affect
statistical power significantly so fine-tuning may be required.

#### Returns

a list with element names like `arm_name`, `arm1_name|arm2_name`,
`arm1_name|arm2_name|arm3_name`, etc., i.e., all possible combination of
treatment arms in comparison. Each element is a data frame, with its
column names self-explained. Specifically, the columns
`p_inverse_normal`, `observed_info`, `is_final` can be used with
`GroupSequentialTest` to perform significance test.

#### Examples

    \dontrun{
    trial$dunnettTest(Surv(pfs, pfs_event) ~ arm, 'pbo', c('high dose', 'low dose'),
                      listener$get_milestone_names(), 'default')
    }

------------------------------------------------------------------------

### Method `closedTest()`

perform closed test based on Dunnett test

#### Usage

    Trials$closedTest(
      dunnett_test,
      treatments,
      milestones,
      alpha,
      alpha_spending = c("asP", "asOF")
    )

#### Arguments

- `dunnett_test`:

  object returned by `Trial$dunnettTest()`.

- `treatments`:

  character vector. Name of treatment arms to be used in comparison.

- `milestones`:

  character vector. Names of triggered milestones at which significance
  testing for endpoint is performed in closed test. Milestones in
  `milestones` does not need to be sorted by their triggering time.

- `alpha`:

  numeric. Allocated alpha.

- `alpha_spending`:

  alpha spending function. It can be `"asP"` or `"asOF"`. Note that
  theoretically it can be `"asUser"`, but it is not tested. It may be
  supported in the future.

#### Returns

a data frame of columns `arm`, `decision` (final decision on a
hypothesis at the end of trial, `"accept"` or `"reject"`),
`milestone_at_reject`, and `reject_time`. If a hypothesis is accepted at
then end of a trial, `milestone_at_reject` is `NA`, and `reject_time` is
`Inf`.

Note that if a hypothesis is tested at multiple milestones, the final
`decision` will be `"accept"` if it is accepted at at least one
milestone. The `decision` is `"reject"` only if the hypothesis is
rejected at all milestones.

#### Examples

    \dontrun{
    dt <- trial$dunnettTest(
      Surv(pfs, pfs_event) ~ arm,
      placebo = 'pbo',
      treatments = c('high dose', 'low dose'),
      milestones = c('dose selection', 'interim', 'final'),
      data.frame(pbo = c(100, 160, 80),
                 low = c(100, 160, 80),
                 high = c(100, 160, 80),
                 row.names = c('dose selection', 'interim', 'final'))

    trial$closedTest(dt, treatments = c('high dose', 'low dose'),
                     milestones = c('interim', 'final'),
                     alpha = 0.025, alpha_spending = 'asOF')
    }

------------------------------------------------------------------------

### Method `conditionalPower()`

compute conditional power at a triggered interim milestone for each
treatment-vs-placebo comparison of a time-to-event endpoint, under a
group sequential design with one interim and one final analysis. Locked
data of the milestone is pulled automatically;
[`fitLogrank()`](https://zhangh12.github.io/TrialSimulator/reference/fitLogrank.md)
is called internally to obtain, for every treatment arm vs `placebo`,
the observed z statistic and the observed number of events `d` on the
two arms of that comparison (after applying subset conditions in `...`,
if any). Conditional power is then \$\$CP =
\Phi\left(\frac{\Phi^{-1}(\alpha) - \sqrt{d/D}\\z - \theta\sqrt{\omega
D}\\(1 - d/D)}{\sqrt{1 - d/D}}\right)\$\$ under `alternative = 'less'`
(mirrored for `'greater'`), where \\\theta\\ is the log hazard ratio at
which conditional power is evaluated and \\\omega = r/(1+r)^2\\ with
\\r\\ the allocation ratio of the pair recorded when the milestone's
data was locked. Like \\z\\ and \\d\\, \\r\\ is an interim quantity: the
result depends on the requested milestone only, not on adaptations
applied after it.

The calculation assumes the trial continues as designed: the allocation
ratio of the compared arms is constant from the start of enrollment
through the final analysis, and the final analysis tests the planned
statistic at the planned boundary. A data-dependent design change (e.g.,
updating the sample ratio based on interim results) alters both the
final test statistic and its boundary; such adaptations require a
combination-test analysis instead (see `$dunnettTest()` and
`$closedTest()`). It is users' responsibility to call this function only
when the calculation is legitimate – in particular, the allocation ratio
of the compared arms has not been updated before the milestone, the
compared arms are enrolled concurrently with placebo, and subset
conditions in `...` are independent of randomization – `TrialSimulator`
has no way to track this.

Conditional power can be requested for an arm that has been removed from
the trial: its z and `d` are well-defined historical quantities,
although no further event will accrue on it. With a numeric `effect`,
however, an error is raised for an arm removed before the milestone, as
no allocation ratio of the pair is recorded at the milestone.

#### Usage

    Trials$conditionalPower(
      milestone,
      formula,
      placebo,
      alternative,
      alpha,
      D,
      effect,
      ...
    )

#### Arguments

- `milestone`:

  character. Name of a triggered milestone at which the interim results
  are observed.

- `formula`:

  an object of class `formula` as in
  [`fitLogrank()`](https://zhangh12.github.io/TrialSimulator/reference/fitLogrank.md),
  e.g., `Surv(pfs, pfs_event) ~ arm`. Stratification via `strata(...)`
  is supported; no covariate is allowed.

- `placebo`:

  character. Name of the placebo arm.

- `alternative`:

  a character string specifying the alternative hypothesis, must be one
  of `"greater"` or `"less"`. No default value. `"greater"` means
  superiority of treatment over placebo is established by a hazard ratio
  greater than 1. See
  [`fitLogrank()`](https://zhangh12.github.io/TrialSimulator/reference/fitLogrank.md).

- `alpha`:

  numeric. The one-sided nominal significance level(s) corresponding to
  the planned final critical boundary, in (0, 1): under
  `alternative = 'less'` the final z statistic is compared with
  `qnorm(alpha)`. Under a group sequential design it is implied by the
  alpha spending function, e.g., `1 - pnorm(c)` for a final critical
  value `c` on the upper scale; in general it differs from both the
  total design alpha and the alpha spent, cumulatively or incrementally,
  at the final look. If a single treatment arm is compared with placebo,
  an unnamed scalar is accepted; otherwise `alpha` must be a named
  vector using treatment arm names, matching the names of `D`. Entries
  are matched to `D` by name, so the order of components does not
  matter.

- `D`:

  numeric. Planned number of events at the final analysis for each
  comparison, counted on the two arms of that comparison (placebo plus
  one treatment arm). If a single treatment arm is compared with
  placebo, an unnamed scalar is accepted; otherwise `D` must be a named
  vector using treatment arm names. A subset of the treatment arms can
  be specified, in which case conditional power is computed for that
  subset of comparisons only. `D` and `alpha` must be of the same length
  and, when named, use the identical set of arm names. An error is
  raised if the observed number of events `d` of a comparison already
  reaches `D`.

- `effect`:

  the treatment effect at which conditional power is evaluated. No
  default value. `'trend'` extrapolates the effect observed at the
  interim; `'null'` assumes no effect for the remaining events
  (conditional type I error); a single positive numeric value is
  interpreted as a hazard ratio (e.g., `effect = 0.75`), which is
  converted internally using the allocation ratio of each pair recorded
  at the milestone.

- `...`:

  subset conditions compatible with
  [`dplyr::filter`](https://dplyr.tidyverse.org/reference/filter.html),
  passed to
  [`fitLogrank()`](https://zhangh12.github.io/TrialSimulator/reference/fitLogrank.md).

#### Returns

a data frame with one row per treatment-vs-placebo comparison, with
columns `arm`, `placebo`, `z`, `d`, `D`, `info_fraction`, `alpha`,
`effect` and `cp`.

------------------------------------------------------------------------

### Method `add_regimen()`

register regimen to a trial. The regimen consists of three functions to
determine the patients who may switch to other treatment during a a
trial, to determine the switching time and how to update patients'
endpoint data accordingly.

#### Usage

    Trials$add_regimen(regimen)

#### Arguments

- `regimen`:

  an object created by
  [`regimen()`](https://zhangh12.github.io/TrialSimulator/reference/regimen.md).

------------------------------------------------------------------------

### Method `lock_data()`

**INTERNAL MACHINERY: DO NOT CALL THIS METHOD DIRECTLY.**

lock data at specific calendar time. For time-to-event endpoints, their
event indicator `*_event` should be updated accordingly. Locked data
should be stored separately. DO NOT OVERWRITE/UPDATE
private\$trial_data! which can lose actual time-to-event information.
For example, a patient may be censored at the first data lock. However,
he may have event being observed in a later data lock.

#### Usage

    Trials$lock_data(at_calendar_time, milestone_name)

#### Arguments

- `at_calendar_time`:

  time point to lock trial data

- `milestone_name`:

  assign milestone name as the name of locked data for future reference.

------------------------------------------------------------------------

### Method `get_data_lock_time_by_calendar_time()`

**INTERNAL MACHINERY: DO NOT CALL THIS METHOD DIRECTLY.**

given the calendar time to lock the data, return it with event counts of
each of the endpoints.

#### Usage

    Trials$get_data_lock_time_by_calendar_time(calendar_time)

#### Arguments

- `calendar_time`:

  numeric. Calendar time to lock the data

#### Returns

data lock time

------------------------------------------------------------------------

### Method `get_data_lock_time_by_event_number()`

**INTERNAL MACHINERY: DO NOT CALL THIS METHOD DIRECTLY.**

given a set of endpoints and target number of events, determine the data
lock time for a milestone (futility, interim, final, etc.). This
function does not change trial object (e.g. rolling back not yet
randomized patients after the found data lock time).

#### Usage

    Trials$get_data_lock_time_by_event_number(
      endpoints,
      arms,
      target_n_events,
      type = c("all", "any"),
      ...
    )

#### Arguments

- `endpoints`:

  character vector. Data lock time is determined by a set of endpoints.

- `arms`:

  a vector of arms' name on which number of events will be counted.

- `target_n_events`:

  target number of events for each of the `endpoints`.

- `type`:

  `all` if all target number of events are reached. `any` if the any
  target number of events is reached.

- `...`:

  subset conditions compatible with
  [`dplyr::filter`](https://dplyr.tidyverse.org/reference/filter.html).
  Number Time of milestone is based on event counts on the subset of
  trial data.

#### Returns

data lock time

------------------------------------------------------------------------

### Method `get_data_lock_time_by_enrollment()`

**INTERNAL MACHINERY: DO NOT CALL THIS METHOD DIRECTLY.**

given a target number of enrolled patients, determine the data lock time
for a milestone (futility, interim, final, etc.). This function does not
change trial object (e.g. rolling back not yet randomized patients after
the found data lock time). It is similar to
get_data_lock_time_by_event_number but only focus on patient_id.

#### Usage

    Trials$get_data_lock_time_by_enrollment(
      arms,
      target_n_patients,
      min_treatment_duration,
      ...
    )

#### Arguments

- `arms`:

  a vector of arms' name on which number of events will be counted.

- `target_n_patients`:

  target number of enrolled patients.

- `min_treatment_duration`:

  numeric. Zero or positive value. minimum treatment duration of
  enrolled patients. If 0, it looks for triggering time based on number
  of enrolled patients in population specified by `...` and `arms`. If
  positive, it means that milestone is triggered when a specific number
  of enrolled patients have received treatment for at least
  `min_treatment_duration` duration. It is users' responsibility to
  assure that the unit of `min_treatment_duration` are consistent with
  readout of non-tte endpoints, dropout time, and trial duration.

- `...`:

  subset conditions compatible with
  [`dplyr::filter`](https://dplyr.tidyverse.org/reference/filter.html).
  Number Time of milestone is based on event counts on the subset of
  trial data.

#### Returns

data lock time

------------------------------------------------------------------------

### Method `has_arm()`

**INTERNAL MACHINERY: DO NOT CALL THIS METHOD DIRECTLY.**

check if the trial has any arm. Return `TRUE` or `FALSE`.

#### Usage

    Trials$has_arm()

------------------------------------------------------------------------

### Method `event_plot()`

**INTERNAL MACHINERY: DO NOT CALL THIS METHOD DIRECTLY.**

plot of cumulative number of events/samples over calendar time.

#### Usage

    Trials$event_plot()

------------------------------------------------------------------------

### Method `mute()`

**INTERNAL MACHINERY: DO NOT CALL THIS METHOD DIRECTLY.**

mute all messages (not including warnings)

#### Usage

    Trials$mute(silent)

#### Arguments

- `silent`:

  logical.

------------------------------------------------------------------------

### Method `reset()`

**INTERNAL MACHINERY: DO NOT CALL THIS METHOD DIRECTLY.**

reset a trial to its snapshot taken before it was executed. Seed will be
reassigned with a new one. Enrollment time are re-generated. If the
trial already have arms when this function is called, they are added
back to recruit patients again.

#### Usage

    Trials$reset()

------------------------------------------------------------------------

### Method `make_arms_snapshot()`

**INTERNAL MACHINERY: DO NOT CALL THIS METHOD DIRECTLY.**

make a snapshot of arms

#### Usage

    Trials$make_arms_snapshot()

------------------------------------------------------------------------

### Method `pop_milestone_updates()`

**INTERNAL MACHINERY: DO NOT CALL THIS METHOD DIRECTLY.**

return and clear the queue of milestone update requests scheduled by
[`update_milestone()`](https://zhangh12.github.io/TrialSimulator/reference/update_milestone.md)
within the current action function. It is called by the listener right
after each action function returns, to apply the requested updates to
its registered milestones.

#### Usage

    Trials$pop_milestone_updates()

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

print a trial

#### Usage

    Trials$print()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Trials$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples
