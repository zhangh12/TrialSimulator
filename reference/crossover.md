# Crossover at a Milestone

Apply a milestone-triggered crossover to patients who are still in the
trial.

Unlike a regimen registered via `add_regimen()` (which is applied once,
at enrollment), `crossover()` is meant to be called *inside* a
milestone's action function. At the earliest crossover (calendar) time
`T = trial$get_current_time() + delay`, eligible patients may switch to
a new treatment, and only their *post-switch* endpoint values are
altered; already-observed data is left intact. The crossover triplet is
stacked onto the trial's regimen, so it is also re-applied to patients
enrolled later.

Eligibility (the pool passed to `what()`) consists of patients with at
least one endpoint still "open" (unobserved, dropout-/duration-aware) at
`T`; fully-observed patients are excluded. `when()` must return a switch
time with `enroll_time + switch_time >= T` (a crossover cannot predate
its opening) or an error is raised. `how()` may only change post-switch
outcomes; returning a changed value for a pre-switch/locked cell raises
an error.

Two helper columns are injected into `patient_data` for the triplet
functions: `earliest_crossover_calendar_time` (`= T`) and
`earliest_crossover_time_from_enrollment` (`= max(T - enroll_time, 0)`),
so `when()` can write e.g.
`switch_time = pmax(pfs, earliest_crossover_time_from_enrollment)`.

Note that this function should only be called within action functions of
milestones. It is users' responsibility to ensure that and
`TrialSimulator` has no way to track it.

This is a user-friendly wrapper of the member function of trial, i.e.,
`Trials$crossover()`. Users who are not familiar with the concept of
classes may consider using this wrapper directly.

## Usage

``` r
crossover(trial, what, how, when = NULL, delay = 0, ...)
```

## Arguments

- trial:

  a trial object returned by
  [`trial()`](https://zhangh12.github.io/TrialSimulator/reference/trial.md).

- what:

  a function selecting which eligible patients crossover, returning one
  row per crossing-over patient with their `new_treatment`. See
  [`regimen()`](https://zhangh12.github.io/TrialSimulator/reference/regimen.md).

- how:

  a function returning the modified post-switch endpoint values for
  crossing-over patients. See
  [`regimen()`](https://zhangh12.github.io/TrialSimulator/reference/regimen.md).

- when:

  (optional) a function returning `switch_time` from enrollment. If
  `NULL` (default), patients switch at `T`, i.e.
  `switch_time = earliest_crossover_time_from_enrollment`.

- delay:

  numeric. Time after the milestone before the crossover opens, so
  `T = trial$get_current_time() + delay`. Default `0` (opens at the
  milestone).

- ...:

  (optional) named arguments routed to `what`, `when`, and/or `how`.

## Value

no return value, called for its side effect of updating `trial`.
