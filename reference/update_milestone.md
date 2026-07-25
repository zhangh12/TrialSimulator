# Updating a Not-Yet-Triggered Milestone of a Trial

update the trigger condition and/or the action of a not-yet-triggered
milestone. The milestone to be updated is identified by its name, which
cannot be changed. This function can be used in adaptive designs, e.g.,
when conditional power at an interim analysis is lower than expected,
the final analysis can be postponed by increasing the target number of
events in its triggering condition, or its triggering condition can be
switched from a calendar time to an event count entirely.

The update is not applied immediately: it is queued and takes effect
right after the current action function returns, before the next
milestone is evaluated. The new trigger condition and action replace the
old ones as a whole. Between simulation replicates the milestone is
restored to its as-designed trigger condition and action, so every
replicate starts from the original design. A milestone that has already
been triggered cannot be updated.

Note that this function should only be called within action functions of
milestones. It is users' responsibility to ensure that and
`TrialSimulator` has no way to track it. Calling it before any milestone
has been triggered is an error. Also note that milestones must trigger
in their registration order: an updated triggering condition that makes
a later-registered milestone fire before an earlier one stops the
simulation with an error.

This is a user-friendly wrapper of the member function of trial, i.e.,
`Trials$update_milestone()`, which is used in vignettes. Users who are
not familiar with the concept of classes may consider using this wrapper
directly.

## Usage

``` r
update_milestone(trial, name, when = NULL, action = NULL, ...)
```

## Arguments

- trial:

  a trial object returned by
  [`trial()`](https://zhangh12.github.io/TrialSimulator/reference/trial.md).

- name:

  character. Name of the milestone to be updated. It must be registered
  with the listener and not yet triggered.

- when:

  (optional) new triggering condition, an object returned by
  [`calendarTime()`](https://zhangh12.github.io/TrialSimulator/reference/calendarTime.md),
  [`enrollment()`](https://zhangh12.github.io/TrialSimulator/reference/enrollment.md),
  [`eventNumber()`](https://zhangh12.github.io/TrialSimulator/reference/eventNumber.md)
  or their combinations using `&` and `|`. If `NULL`, the triggering
  condition is left unchanged.

- action:

  (optional) new action function. See `action` of
  [`milestone()`](https://zhangh12.github.io/TrialSimulator/reference/milestone.md).
  If `NULL`, the action is left unchanged.

- ...:

  (optional) named arguments of the new `action`. Only allowed when
  `action` is provided. The new action is executed with exactly the
  arguments supplied here: fixed arguments of the previous action are
  never carried over.

## Value

no return value, called for its side effect of updating `trial`.
