# Removing One or More Arms From a Trial

remove arms from a trial. The application of this function includes, but
is not limited to, dose selection, enrichment analysis (select
sub-population).

Note that this function should only be called within action functions of
milestones. It is users' responsibility to ensure that and
`TrialSimulator` has no way to track it.

Removing an arm has three effects. No patient is randomized to it
afterwards; unenrolled patients are randomized again among the remaining
arms. It leaves the set of arms in the trial, e.g., `arms = NULL` in
[`eventNumber()`](https://zhangh12.github.io/TrialSimulator/reference/eventNumber.md)
and
[`enrollment()`](https://zhangh12.github.io/TrialSimulator/reference/enrollment.md)
no longer counts it, and `dunnettTest()` stops testing it at later
milestones. Its data are censored at the time of removal, or
`additional_followup` later, but stay in the trial data, so they are
present in the locked data of later milestones.

`additional_followup` has no default, so that the decision is visible in
the action function. With 0, follow-up of the patients in a removed arm
stops at the milestone where the arm is removed, e.g., a dose dropped
for futility. A positive value keeps following them for a fixed time
beyond the milestone, and `Inf` for the rest of the trial, e.g., to
collect overall survival or safety data of a dose that stops enrolling.
The same value applies to all arms removed in one call; to grant
different times to different arms, call this function once per arm.
Events of a removed arm within its follow-up are counted by a later
milestone only if the arm is listed in `arms` of its triggering
condition; see the next section.

## Usage

``` r
remove_arms(trial, arms_name, additional_followup)
```

## Arguments

- trial:

  a trial object returned by
  [`trial()`](https://zhangh12.github.io/TrialSimulator/reference/trial.md).

- arms_name:

  character vector. Name of arms to be removed.

- additional_followup:

  numeric. Extra follow-up time granted to the patients of the removed
  arms after the current milestone, shared by all arms in `arms_name`.
  No default: 0 stops their follow-up at the milestone itself. `Inf`
  keeps following them for the rest of the trial.

## Value

no return value, called for its side effect of updating `trial`.

## Counting on removed arms at later milestones

Which arms are removed, and how many, are usually decided from the data
and thus unknown when milestones are defined. Three patterns cover the
triggering conditions of later milestones:

- Count on the arms still in the trial: leave `arms = NULL` in
  [`eventNumber()`](https://zhangh12.github.io/TrialSimulator/reference/eventNumber.md)
  or
  [`enrollment()`](https://zhangh12.github.io/TrialSimulator/reference/enrollment.md).
  It is resolved when the milestone is evaluated, so any arm removed by
  then is excluded.

- Count on all arms of the design, including removed ones: list every
  arm of the design in `arms` of
  [`eventNumber()`](https://zhangh12.github.io/TrialSimulator/reference/eventNumber.md)
  or
  [`enrollment()`](https://zhangh12.github.io/TrialSimulator/reference/enrollment.md).
  All names are known when the design is written; a removed arm passes
  the check because it was once in the trial, and a warning is raised
  unless the trial is silent. This is how the total sample size of a
  trial is counted after a dose is dropped.

- Count on a subset that depends on which arm was removed, e.g., the
  control arm and the removed arm only: the subset is unknown when the
  later milestone is defined. Call
  [`update_milestone()`](https://zhangh12.github.io/TrialSimulator/reference/update_milestone.md)
  in the same action function as `remove_arms()` to replace the
  triggering condition of the later milestone, with `arms` built from
  the arm just removed.

See the examples.

This is a user-friendly wrapper of the member function of trial, i.e.,
`Trials$remove_arms()`, which is used in vignettes. Users who are not
familiar with the concept of classes may consider using this wrapper
directly.

## Examples

``` r
if (FALSE) { # \dontrun{
## Within an action function: drop a dose and stop following its patients
## at this milestone. additional_followup has no default.
remove_arms(trial, 'low dose', additional_followup = 0)

## Drop a dose but follow its patients for another 12 months, e.g., for
## overall survival.
remove_arms(trial, 'low dose', additional_followup = 12)

## Drop a dose and follow its patients until the end of the trial.
remove_arms(trial, 'low dose', additional_followup = Inf)

## Different follow-up for different arms: one call per arm.
remove_arms(trial, 'low dose', additional_followup = 12)
remove_arms(trial, 'mid dose', additional_followup = Inf)

## A design with placebo and two doses. At dose selection, one dose may be
## dropped; which one, if any, is decided from the data.

## Pattern 1: the final analysis counts events in the arms still in the
## trial. arms = NULL is resolved when `final` is evaluated, so the dropped
## dose is excluded automatically.
final <- milestone(name = 'final',
                   when = eventNumber(endpoint = 'os', n = 300),
                   action = action_at_final)
# final is then registered with the listener

## Pattern 2: the final analysis counts events in all arms of the design,
## including a dropped dose (events up to its removal). List every arm;
## a dropped arm is accepted, with a warning unless the trial is silent.
## In this example, final analysis is triggered when in total 1000 patients
## are enrolled, including those from the removed arm.
final <- milestone(name = 'final',
                   when = enrollment(n = 1000,
                                     arms = c('placebo', 'low dose', 'high dose')),
                   action = action_at_final)
# final is then registered with the listener

## Pattern 3: the final analysis counts events in placebo and the dropped
## dose only. The pair is unknown when `final` is defined, so `final` is
## registered with a placeholder condition, and the action of dose
## selection replaces it after the removal. The placeholder is never
## evaluated: milestones are triggered in registration order, and the
## update takes effect before `final` is checked.
final <- milestone(name = 'final',
                   when = eventNumber(endpoint = 'os', n = 90), # placeholder
                   action = action_at_final)

action_at_dose_selection <- function(trial){
  locked_data <- trial$get_locked_data('dose selection')
  dropped <- ...  # the dose to drop, selected from locked_data, or NULL
  if(!is.null(dropped)){
    trial$remove_arms(dropped, additional_followup = 0)
    trial$update_milestone(name = 'final',
                           when = eventNumber(endpoint = 'os', n = 300,
                           arms = c('placebo', dropped)))
  }
}

dose_selection <- milestone(name = 'dose selection',
                            when = eventNumber(endpoint = 'pfs', n = 150),
                            action = action_at_dose_selection)
# dose_selection and final are then registered with the listener, in this
# order, so that the update in dose_selection is in effect before final
# is checked
listener <- listener()
listener$add_milestones(dose_selection, final)
} # }
```
