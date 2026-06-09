# Crossover at a Milestone

The [dynamic treatment
switching](https://zhangh12.github.io/TrialSimulator/articles/dynamicTreatmentSwitching.html)
vignette describes crossover that is decided *at enrollment*: every
patient’s switching rule is fixed when they enter the trial, off their
own (fully simulated) outcome trajectory. That covers crossover-at-
progression, rescue medication, and similar patient-level designs.

This vignette covers a different situation: crossover that becomes
available only **at, or after, a milestone**, and is therefore a
decision the trial makes once it reaches that milestone. Typical
examples include:

- at an interim analysis, control patients who are still in the trial
  are offered the experimental treatment;
- after a futile dose is dropped, patients on that dose are switched to
  the retained dose;
- a protocol amendment opens crossover from a fixed calendar time
  onward.

The defining feature is that switching is anchored to a **calendar
time** that is not known until the trial runs (it may be event-driven),
and it applies only to patients who are still in the trial when that
time is reached. Already-observed outcomes must not change; only the
future may. `TrialSimulator` supports this through the member function
`trial$crossover()`.

[`crossover()`](https://zhangh12.github.io/TrialSimulator/reference/crossover.md)
reuses the same `what()` / `when()` / `how()` interface as
[`regimen()`](https://zhangh12.github.io/TrialSimulator/reference/regimen.md),
so this vignette assumes familiarity with the three-function design
described in the [dynamic treatment
switching](https://zhangh12.github.io/TrialSimulator/articles/dynamicTreatmentSwitching.html)
vignette. Here we focus on what is specific to a milestone crossover:
the opening time, how the eligible population is determined, and how the
package validates the triplet you supply.

## Where `crossover()` is called

Unlike `add_regimen()`, which must be registered *before*
[`add_arms()`](https://zhangh12.github.io/TrialSimulator/reference/add_arms.md)
and is applied at enrollment,
[`crossover()`](https://zhangh12.github.io/TrialSimulator/reference/crossover.md)
is meant to be called **inside the action function of a milestone**:

``` r

action <- function(trial){
  locked_data <- trial$get_locked_data('interim')
  ## ... interim analysis / decision making ...
  trial$crossover(what = what_fn, how = how_fn)
}

interim <- milestone(name = 'interim',
                     when = eventNumber(endpoint = 'pfs', n = 300),
                     action = action)
```

By the time an action function runs, `TrialSimulator` has already locked
the data at the milestone, so `trial$get_current_time()` returns the
milestone (calendar) time.
[`crossover()`](https://zhangh12.github.io/TrialSimulator/reference/crossover.md)
uses this to decide who is still in the trial and to anchor the switch.
Because of this, calling
[`crossover()`](https://zhangh12.github.io/TrialSimulator/reference/crossover.md)
outside an action function (when the current time is still `0`) is
rejected with an informative error.

The user-friendly wrapper `crossover(trial, ...)` is also available for
those who prefer not to call member functions directly:

``` r

crossover(trial, what = what_fn, how = how_fn)
```

## The earliest crossover time: `delay` and the opening time

The single milestone-specific argument is `delay`. The **earliest
crossover time** (the calendar time from which switching may take
effect) is

    T = trial$get_current_time() + delay

- `delay = 0` (default): crossover opens *at* the milestone. This is the
  common “let eligible patients switch now” case.
- `delay > 0`: crossover opens a fixed amount of time *after* the
  milestone. This is convenient for an administrative ramp-up or a
  wash-out window before switching becomes effective.

`T` is the reference time for everything that follows: it determines who
is eligible, it is the floor for the switching time, and it is the
boundary that separates already-observed outcomes (which are protected)
from future outcomes (which `how()` may modify).

## How the eligible population is determined

A milestone crossover only makes sense for patients who still have
something left to change. Before calling `what()`, the package therefore
restricts the candidate pool to patients who are **still in the trial at
`T`**, i.e. patients with at least one endpoint whose value has not yet
been settled by `T`. Concretely, for each patient the package checks
whether any endpoint is still *open*, using the reference time
`ref = max(T, enroll_time)` (so that not-yet-enrolled patients are
handled correctly too):

- a time-to-event endpoint is open when
  `enroll_time + min(event_time, dropout_time) > ref`, i.e. neither the
  event nor dropout has occurred by `ref`;
- a non-time-to-event endpoint is open when
  `enroll_time + readout > ref` **and** the reading will actually be
  taken, that is `readout <= dropout_time` and the reading falls within
  the trial duration. A baseline endpoint (`readout = 0`) is measured at
  enrollment and is therefore never open.

A patient enters the pool passed to `what()` if **any** of their
endpoints is open. Patients who have died, dropped out, or completed all
of their readouts by `T` are excluded automatically – they have no
post-`T` outcome that a crossover could alter. This mirrors the way
[`regimen()`](https://zhangh12.github.io/TrialSimulator/reference/regimen.md)
“figures out patients who are eligible for the three functions”; the
difference is only the reference time `T`.

The three functions then receive progressively smaller data frames,
which keeps the logic clean and avoids unnecessary computation:

- `what()` receives the **eligible** patients;
- `when()` receives the patients that `what()` actually selected for
  switching;
- `how()` receives those same switchers, with their assigned
  `switch_time`.

To make timing rules easy to express, the package injects two helper
columns into `patient_data` for the triplet:

- `earliest_crossover_calendar_time`, equal to `T`;
- `earliest_crossover_time_from_enrollment`, equal to
  `max(T - enroll_time, 0)`, i.e. the earliest admissible `switch_time`
  measured from enrollment.

These let `when()` express, for example, “switch at progression, but not
before crossover opens”:

``` r

time_selector <- function(patient_data){
  data.frame(
    patient_id  = patient_data$patient_id,
    switch_time = pmax(patient_data$pfs,
                       patient_data$earliest_crossover_time_from_enrollment)
  )
}
```

If `when()` is not supplied, patients switch at the opening time `T` by
default (`switch_time = earliest_crossover_time_from_enrollment`).

## How the package monitors the triplet’s output

Because the triplet is user-supplied, `TrialSimulator` validates what
each function returns and stops with an informative error when an input
would corrupt the simulation. This lets you develop the three functions
incrementally and catch mistakes early (a
[`browser()`](https://rdrr.io/r/base/browser.html) inside any of them is
a convenient way to debug).

**`what()`** must return a data frame with columns `patient_id` and
`new_treatment`. `NA` in `new_treatment` means “do not switch”, so the
returned set of switchers may be smaller than the input. The package
checks that:

- the result is a data frame containing the required columns;
- no `patient_id` is duplicated;
- every selected patient belongs to the eligible pool. Because `what()`
  is only ever shown eligible patients, selecting a patient outside that
  pool indicates a logic error and is rejected.

`new_treatment` is a free-form label; it does not need to be an existing
arm. The randomized `arm` column is never changed – the label is
recorded only in the patient’s switching history (see below).

**`when()`** must return a data frame with columns `patient_id` and
`switch_time`, measured from enrollment. The package checks that:

- the result is a data frame containing the required columns;
- no `patient_id` is duplicated;
- a non-`NA` `switch_time` is provided for *every* patient selected by
  `what()`;
- the switch does not predate the opening time, i.e.
  `enroll_time + switch_time >= T` for every switcher. A milestone
  crossover cannot take effect before it opens, so an earlier time is
  rejected.

**`how()`** must return a data frame with `patient_id` and the endpoint
columns it modifies. You only return the endpoints you change, and
within an endpoint you may leave cells unchanged by setting them to `NA`
(or to their original value). The package checks that:

- the result is a data frame with a `patient_id` column, no duplicated
  ids, and only columns that exist in the trial data;
- protected columns (`arm`, `enroll_time`, `dropout_time`, and the
  internal switching-history column) are not modified;
- **only post-switch outcomes are altered.** Returning a value that
  differs from the original for a cell whose readout or event falls at
  or before the patient’s `switch_time` – a pre-switch or
  already-observed outcome – is rejected. This guarantees that the
  crossover never rewrites history: outcomes observed before the switch
  are preserved exactly, and the data locked at the milestone is left
  intact.

In practice this means a `how()` for a milestone crossover guards the
post-switch region explicitly, for example:

``` r

data_modifier <- function(patient_data){
  data.frame(
    patient_id = patient_data$patient_id,
    ## extend only the residual (post-switch) survival; leave os unchanged
    ## for patients whose event is at or before the switch
    os = ifelse(patient_data$os > patient_data$switch_time,
                patient_data$switch_time +
                  1.2 * (patient_data$os - patient_data$switch_time),
                patient_data$os)
  )
}
```

As with
[`regimen()`](https://zhangh12.github.io/TrialSimulator/reference/regimen.md),
never apply dropout or censoring manually in `how()`; `TrialSimulator`
re-applies dropout and calendar censoring automatically after the
crossover and when locking data at later milestones.

## A complete action function

Putting the pieces together, the following action lets control patients
who are still in the trial at an interim cross over to the experimental
treatment, extending only their post-switch survival:

``` r

crossover_action <- function(trial){

  ## decision making on the locked interim data can go here
  locked_data <- trial$get_locked_data('interim')

  what <- function(patient_data){
    data.frame(
      patient_id    = patient_data$patient_id,
      new_treatment = ifelse(patient_data$arm == 'control',
                             'crossover', NA_character_)
    )
  }

  how <- function(patient_data){
    data.frame(
      patient_id = patient_data$patient_id,
      os = ifelse(patient_data$os > patient_data$switch_time,
                  patient_data$switch_time +
                    1.2 * (patient_data$os - patient_data$switch_time),
                  patient_data$os)
    )
  }

  ## when() is omitted, so switching takes effect at the interim time
  trial$crossover(what = what, how = how)
}

interim <- milestone(name = 'interim',
                     when = eventNumber(endpoint = 'os', n = 300),
                     action = crossover_action)
```

## Scenarios

**Delayed opening (wash-out).** Supplying `delay` opens crossover after
the milestone, which is useful when switching cannot be implemented
immediately:

``` r

trial$crossover(what = what, how = how, delay = 2) # opens 2 time units later
```

**Custom timing.** Provide `when()` to switch at a clinically meaningful
time rather than at the opening, using the injected helper column to
respect the opening floor (see the `time_selector` example above).

**Patients enrolled after the milestone.** A milestone crossover also
applies to patients who enroll later: they switch according to the same
triplet once they are in the trial. There is nothing extra to do.

**Several crossovers.**
[`crossover()`](https://zhangh12.github.io/TrialSimulator/reference/crossover.md)
can be called in more than one milestone. Each call stacks an additional
switching opportunity, applied in chronological order, so a patient may
cross over more than once.

## Relationship to `add_regimen()`

A milestone crossover and an enrollment-time
[`regimen()`](https://zhangh12.github.io/TrialSimulator/reference/regimen.md)
are the same machinery under the hood: both are sequences of `what()` /
`when()` / `how()` triplets, each with an earliest crossover time. The
enrollment regimen is simply the special case where that time is `0`
(switching is open from the first enrollment);
[`crossover()`](https://zhangh12.github.io/TrialSimulator/reference/crossover.md)
appends a triplet whose earliest time is the milestone time plus
`delay`. The eligibility filter, the timing floor, and the post-switch
protection therefore behave consistently across both entry points.

## Inspecting the switches

Every switch is recorded in the `regimen_trajectory` column of the
locked data as a compact `"arm@0;new_treatment@switch_time"` string. Use
[`expandRegimen()`](https://zhangh12.github.io/TrialSimulator/reference/expandRegimen.md)
to expand it into one row per regimen segment per patient for summaries
and checks:

``` r

locked <- trial$get_locked_data('final')
expandRegimen(locked)
```
