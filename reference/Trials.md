# Class of Trial

Create a class of trial.

Public methods in this R6 class are used in developing this package.
Thus, we have to export the whole R6 class which exposures all public
methods. However, only the public methods in the list below are useful
to end users.

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

- `$dunnettTest()` perform Dunnett's test.

- `$closedTest()` perform combination test based on Dunnett's test.

## Methods

### Public methods

- [`Trials$new()`](#method-Trials-new)

- [`Trials$get_trial_data()`](#method-Trials-get_trial_data)

- [`Trials$get_duration()`](#method-Trials-get_duration)

- [`Trials$set_duration()`](#method-Trials-set_duration)

- [`Trials$set_enroller()`](#method-Trials-set_enroller)

- [`Trials$get_enroller()`](#method-Trials-get_enroller)

- [`Trials$set_dropout()`](#method-Trials-set_dropout)

- [`Trials$get_dropout()`](#method-Trials-get_dropout)

- [`Trials$roll_back()`](#method-Trials-roll_back)

- [`Trials$remove_arms()`](#method-Trials-remove_arms)

- [`Trials$update_sample_ratio()`](#method-Trials-update_sample_ratio)

- [`Trials$update_generator()`](#method-Trials-update_generator)

- [`Trials$resize()`](#method-Trials-resize)

- [`Trials$add_arms()`](#method-Trials-add_arms)

- [`Trials$get_name()`](#method-Trials-get_name)

- [`Trials$get_description()`](#method-Trials-get_description)

- [`Trials$get_arms()`](#method-Trials-get_arms)

- [`Trials$get_arms_name()`](#method-Trials-get_arms_name)

- [`Trials$get_number_arms()`](#method-Trials-get_number_arms)

- [`Trials$has_arm()`](#method-Trials-has_arm)

- [`Trials$get_an_arm()`](#method-Trials-get_an_arm)

- [`Trials$get_sample_ratio()`](#method-Trials-get_sample_ratio)

- [`Trials$get_number_patients()`](#method-Trials-get_number_patients)

- [`Trials$get_number_enrolled_patients()`](#method-Trials-get_number_enrolled_patients)

- [`Trials$get_number_unenrolled_patients()`](#method-Trials-get_number_unenrolled_patients)

- [`Trials$get_randomization_queue()`](#method-Trials-get_randomization_queue)

- [`Trials$get_enroll_time()`](#method-Trials-get_enroll_time)

- [`Trials$enroll_patients()`](#method-Trials-enroll_patients)

- [`Trials$set_current_time()`](#method-Trials-set_current_time)

- [`Trials$get_current_time()`](#method-Trials-get_current_time)

- [`Trials$get_event_tables()`](#method-Trials-get_event_tables)

- [`Trials$get_data_lock_time_by_event_number()`](#method-Trials-get_data_lock_time_by_event_number)

- [`Trials$get_data_lock_time_by_calendar_time()`](#method-Trials-get_data_lock_time_by_calendar_time)

- [`Trials$get_locked_data()`](#method-Trials-get_locked_data)

- [`Trials$get_locked_data_name()`](#method-Trials-get_locked_data_name)

- [`Trials$get_event_number()`](#method-Trials-get_event_number)

- [`Trials$save_milestone_time()`](#method-Trials-save_milestone_time)

- [`Trials$get_milestone_time()`](#method-Trials-get_milestone_time)

- [`Trials$lock_data()`](#method-Trials-lock_data)

- [`Trials$event_plot()`](#method-Trials-event_plot)

- [`Trials$censor_trial_data()`](#method-Trials-censor_trial_data)

- [`Trials$save()`](#method-Trials-save)

- [`Trials$bind()`](#method-Trials-bind)

- [`Trials$save_custom_data()`](#method-Trials-save_custom_data)

- [`Trials$get_custom_data()`](#method-Trials-get_custom_data)

- [`Trials$get()`](#method-Trials-get)

- [`Trials$get_output()`](#method-Trials-get_output)

- [`Trials$mute()`](#method-Trials-mute)

- [`Trials$independentIncrement()`](#method-Trials-independentIncrement)

- [`Trials$dunnettTest()`](#method-Trials-dunnettTest)

- [`Trials$closedTest()`](#method-Trials-closedTest)

- [`Trials$get_seed()`](#method-Trials-get_seed)

- [`Trials$print()`](#method-Trials-print)

- [`Trials$get_snapshot_copy()`](#method-Trials-get_snapshot_copy)

- [`Trials$make_snapshot()`](#method-Trials-make_snapshot)

- [`Trials$make_arms_snapshot()`](#method-Trials-make_arms_snapshot)

- [`Trials$reset()`](#method-Trials-reset)

- [`Trials$set_arm_added_time()`](#method-Trials-set_arm_added_time)

- [`Trials$get_arm_added_time()`](#method-Trials-get_arm_added_time)

- [`Trials$set_arm_removal_time()`](#method-Trials-set_arm_removal_time)

- [`Trials$get_arm_removal_time()`](#method-Trials-get_arm_removal_time)

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
      enroller,
      dropout = NULL,
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

  a function returning a vector enrollment time for patients. Its first
  argument `n` is the number of enrolled patients. Set it to
  `StaggeredRecruiter` can handle most of the use cases. See
  [`?TrialSimulator::StaggeredRecruiter`](https://zhangh12.github.io/TrialSimulator/reference/StaggeredRecruiter.md)
  for more information.

- `dropout`:

  a function returning a vector of dropout time for patients. It can be
  any random number generator with first argument `n`, the number of
  enrolled patients. Usually `rexp` if dropout rate is set at a single
  time point, or `rweibull` if dropout rates are set at two time points.
  See
  [`?TrialSimulator::weibullDropout`](https://zhangh12.github.io/TrialSimulator/reference/weibullDropout.md).

- `silent`:

  logical. `TRUE` to mute messages. However, warning message is still
  displayed.

- `...`:

  (optional) arguments of `enroller` and `dropout`.

------------------------------------------------------------------------

### Method `get_trial_data()`

return trial data of enrolled patients at the time of this function is
called

#### Usage

    Trials$get_trial_data()

------------------------------------------------------------------------

### Method `get_duration()`

return maximum duration of a trial

#### Usage

    Trials$get_duration()

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

### Method `set_enroller()`

set recruitment curve when initialize a trial.

#### Usage

    Trials$set_enroller(func, ...)

#### Arguments

- `func`:

  function to generate enrollment time. It can be built-in function like
  \`rexp\` or customized functions like \`StaggeredRecruiter\`.

- `...`:

  (optional) arguments for `func`.

------------------------------------------------------------------------

### Method `get_enroller()`

get function of recruitment curve

#### Usage

    Trials$get_enroller()

------------------------------------------------------------------------

### Method `set_dropout()`

set distribution of drop out time. This can be done when initialize a
trial, or when updating a trial in adaptive design.

#### Usage

    Trials$set_dropout(func, ...)

#### Arguments

- `func`:

  function to generate dropout time. It can be built-in function like
  \`rexp\` or customized functions.

- `...`:

  (optional) arguments for `func`.

------------------------------------------------------------------------

### Method `get_dropout()`

get generator of dropout time

#### Usage

    Trials$get_dropout()

------------------------------------------------------------------------

### Method `roll_back()`

roll back data to current time of trial. By doing so, `Trial$trial_data`
will be cut at current time, and data after then are deleted. However,
`Trial$enroll_time` after current time are kept unchanged because that
is planned enrollment curve.

#### Usage

    Trials$roll_back()

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

### Method `get_name()`

return name of trial

#### Usage

    Trials$get_name()

------------------------------------------------------------------------

### Method `get_description()`

return description of trial

#### Usage

    Trials$get_description()

------------------------------------------------------------------------

### Method `get_arms()`

return a list of arms in the trial

#### Usage

    Trials$get_arms()

------------------------------------------------------------------------

### Method `get_arms_name()`

return arms' name of trial

#### Usage

    Trials$get_arms_name()

------------------------------------------------------------------------

### Method `get_number_arms()`

get number of arms in the trial

#### Usage

    Trials$get_number_arms()

------------------------------------------------------------------------

### Method `has_arm()`

check if the trial has any arm. Return `TRUE` or `FALSE`.

#### Usage

    Trials$has_arm()

------------------------------------------------------------------------

### Method `get_an_arm()`

return an arm

#### Usage

    Trials$get_an_arm(arm_name)

#### Arguments

- `arm_name`:

  character, name of arm to be extracted

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

### Method `get_number_patients()`

return number of patients when planning the trial

#### Usage

    Trials$get_number_patients()

------------------------------------------------------------------------

### Method `get_number_enrolled_patients()`

return number of enrolled (randomized) patients

#### Usage

    Trials$get_number_enrolled_patients()

------------------------------------------------------------------------

### Method `get_number_unenrolled_patients()`

return number of unenrolled patients

#### Usage

    Trials$get_number_unenrolled_patients()

------------------------------------------------------------------------

### Method `get_randomization_queue()`

return randomization queue of planned but not yet enrolled patients.
This function does not update randomization_queue, just return its value
for debugging purpose.

#### Usage

    Trials$get_randomization_queue(index = NULL)

#### Arguments

- `index`:

  index to be extracted. Return all queue if `NULL`.

------------------------------------------------------------------------

### Method `get_enroll_time()`

return enrollment time of planned but not yet enrolled patients. This
function does not update enroll_time, just return its value for
debugging purpose.

#### Usage

    Trials$get_enroll_time(index = NULL)

#### Arguments

- `index`:

  index to extract. Return all enroll time if `NULL`.

------------------------------------------------------------------------

### Method `enroll_patients()`

assign new patients to pre-planned randomization queue at pre-specified
enrollment time.

#### Usage

    Trials$enroll_patients(n_patients = NULL)

#### Arguments

- `n_patients`:

  number of new patients to be enrolled. If `NULL`, all remaining
  patients in plan are enrolled. Error may be triggered if n_patients is
  greater than remaining patients as planned.

------------------------------------------------------------------------

### Method `set_current_time()`

set current time of a trial. Any data collected before could not be
changed. private\$now should be set after a milestone is triggered
(through Milestones class, futility, interim, etc), an arm is added or
removed at a milestone

#### Usage

    Trials$set_current_time(time)

#### Arguments

- `time`:

  current calendar time of a trial.

------------------------------------------------------------------------

### Method `get_current_time()`

return current time of a trial

#### Usage

    Trials$get_current_time()

------------------------------------------------------------------------

### Method `get_event_tables()`

count accumulative number of events (for TTE) or non-missing samples
(otherwise) over calendar time (enroll time + tte for TTE, or enroll
time + readout otherwise)

#### Usage

    Trials$get_event_tables(arms = NULL, ...)

#### Arguments

- `arms`:

  a vector of arms' name on which the event tables are created. if
  `NULL`, all arms in the trial will be used.

- `...`:

  subset conditions compatible with
  [`dplyr::filter`](https://dplyr.tidyverse.org/reference/filter.html).
  Event tables will be counted on subset of trial data only.

------------------------------------------------------------------------

### Method `get_data_lock_time_by_event_number()`

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

### Method `get_data_lock_time_by_calendar_time()`

given the calendar time to lock the data, return it with event counts of
each of the endpoints.

#### Usage

    Trials$get_data_lock_time_by_calendar_time(calendar_time, arms)

#### Arguments

- `calendar_time`:

  numeric. Calendar time to lock the data

- `arms`:

  a vector of arms' name on which number of events will be counted.

#### Returns

data lock time

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

### Method `get_locked_data_name()`

return names of locked data

#### Usage

    Trials$get_locked_data_name()

------------------------------------------------------------------------

### Method `get_event_number()`

return number of events at lock time of milestones

#### Usage

    Trials$get_event_number(milestone_name = NULL)

#### Arguments

- `milestone_name`:

  names of triggered milestones. Use all triggered milestones if `NULL`.

------------------------------------------------------------------------

### Method `save_milestone_time()`

save time of a new milestone.

#### Usage

    Trials$save_milestone_time(milestone_time, milestone_name)

#### Arguments

- `milestone_time`:

  numeric. Time of new milestone.

- `milestone_name`:

  character. Name of new milestone.

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

### Method `lock_data()`

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

### Method `event_plot()`

plot of cumulative number of events/samples over calendar time.

#### Usage

    Trials$event_plot()

------------------------------------------------------------------------

### Method `censor_trial_data()`

censor trial data at calendar time

#### Usage

    Trials$censor_trial_data(
      censor_at = NULL,
      selected_arms = NULL,
      enrolled_before = Inf
    )

#### Arguments

- `censor_at`:

  time of censoring. It is set to trial duration if `NULL`.

- `selected_arms`:

  censoring is applied to selected arms (e.g., removed arms) only. If
  `NULL`, it will be set to all available arms in trial data. Otherwise,
  censoring is applied to user-specified arms only. This is necessary
  because number of events/sample size in removed arms should be fixed
  unchanged since corresponding milestone is triggered. In that case,
  one can update trial data by something like
  `censor_trial_data(censor_at = milestone_time, selected_arms = removed_arms)`.

- `enrolled_before`:

  censoring is applied to patients enrolled before specific time. This
  argument would be used when trial duration is updated by
  `set_duration`. Adaptation happens when `set_duration` is called so we
  fix duration for patients enrolled before adaptation to maintain
  independent increment. This should work when trial duration is updated
  for multiple times.

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

### Method `mute()`

mute all messages (not including warnings)

#### Usage

    Trials$mute(silent)

#### Arguments

- `silent`:

  logical.

------------------------------------------------------------------------

### Method `independentIncrement()`

calculate independent increments from a given set of milestones

#### Usage

    Trials$independentIncrement(
      formula,
      placebo,
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

  character. String of placebo in trial's locked data.

- `milestones`:

  a character vector of milestone names in the trial, e.g.,
  `listener$get_milestone_names()`.

- `alternative`:

  a character string specifying the alternative hypothesis, must be one
  of `"greater"` or `"less"`. No default value. `"greater"` means
  superiority of treatment over placebo is established by an hazard
  ratio greater than 1 when a log-rank test is used.

- `planned_info`:

  a vector of planned accumulative number of event of time-to-event
  endpoint. It is named by milestone names. Note: `planned_info` can
  also be a character `"oracle"` so that planned number of events are
  set to be observed number of events, in that case inverse normal z
  statistics equal to one-sided logrank statistics. This is for the
  purpose of debugging only. In formal simulation, `"oracle"` should not
  be used if adaptation is present. Pre-fixed `planned_info` should be
  used to create weights in combination test that controls the
  family-wise error rate in the strong sense.

- `...`:

  subset condition that is compatible with
  [`dplyr::filter`](https://dplyr.tidyverse.org/reference/filter.html).
  `survdiff` will be fitted on this subset only to compute one-sided
  logrank statistics. It could be useful when a trial consists of more
  than two arms. By default it is not specified, all data will be used
  to fit the model.

#### Returns

This function returns a data frame with columns:

- `p_inverse_normal`:

  one-sided p-value for inverse normal test based on logrank test
  (alternative hypothesis: risk is higher in placebo arm). Accumulative
  data is used.

- `z_inverse_normal`:

  z statistics of `p_inverse_normal`. Accumulative data is used.

- `p_lr`:

  one-sided p-value for logrank test (alternative hypothesis: risk is
  higher in placebo arm). Accumulative data is used.

- `z_lr`:

  z statistics of `p_lr`. Accumulative data is used.

- `info`:

  observed accumulative event number.

- `planned_info`:

  planned accumulative event number.

- `info_pbo`:

  observed accumulative event number in placebo.

- `info_trt`:

  observed accumulative event number in treatment arm.

- `wt`:

  weights in `z_inverse_normal`.

#### Examples

    \dontrun{
    trial$independentIncrement(Surv(pfs, pfs_event) ~ arm, 'pbo',
                               listener$get_milestone_names(),
                               'less', 'oracle')
    }

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

### Method `get_seed()`

return random seed

#### Usage

    Trials$get_seed()

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

print a trial

#### Usage

    Trials$print()

------------------------------------------------------------------------

### Method `get_snapshot_copy()`

return a snapshot of a trial before it is executed.

#### Usage

    Trials$get_snapshot_copy()

------------------------------------------------------------------------

### Method `make_snapshot()`

make a snapshot before running a trial. This can be useful when
resetting a trial. This is only called when initializing a \`Trial\`
object, when arms have not been added yet.

#### Usage

    Trials$make_snapshot()

------------------------------------------------------------------------

### Method `make_arms_snapshot()`

make a snapshot of arms

#### Usage

    Trials$make_arms_snapshot()

------------------------------------------------------------------------

### Method `reset()`

reset a trial to its snapshot taken before it was executed. Seed will be
reassigned with a new one. Enrollment time are re-generated. If the
trial already have arms when this function is called, they are added
back to recruit patients again.

#### Usage

    Trials$reset()

------------------------------------------------------------------------

### Method `set_arm_added_time()`

save time when an arm is added to the trial

#### Usage

    Trials$set_arm_added_time(arm, time)

#### Arguments

- `arm`:

  name of added arm.

- `time`:

  time when an arm is added.

------------------------------------------------------------------------

### Method `get_arm_added_time()`

get time when an arm is added to the trial

#### Usage

    Trials$get_arm_added_time(arm)

#### Arguments

- `arm`:

  arm name.

------------------------------------------------------------------------

### Method `set_arm_removal_time()`

save time when an arm is removed to the trial

#### Usage

    Trials$set_arm_removal_time(arm, time)

#### Arguments

- `arm`:

  name of removed arm.

- `time`:

  time when an arm is removed.

------------------------------------------------------------------------

### Method `get_arm_removal_time()`

get time when an arm is removed from the trial

#### Usage

    Trials$get_arm_removal_time(arm)

#### Arguments

- `arm`:

  arm name.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Trials$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples
