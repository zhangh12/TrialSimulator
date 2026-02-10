# An Action Function that Does Nothing

This is an action function that does nothing when the corresponding
milestone is triggered. When the listener is monitoring a trial and
determining the time to trigger a milestone, data is automatically
locked with other necessary data manipulations (censoring, truncation,
etc.) are executed. If the users have no intent to modify the trial
adaptively at the milestone, e.g., adding
([`add_arms()`](https://zhangh12.github.io/TrialSimulator/reference/add_arms.md))
or removing
([`remove_arms()`](https://zhangh12.github.io/TrialSimulator/reference/remove_arms.md))
arm(s), changing sampling ratio(s)
([`update_sample_ratio()`](https://zhangh12.github.io/TrialSimulator/reference/update_sample_ratio.md)),
modifying trial duration
([`set_duration()`](https://zhangh12.github.io/TrialSimulator/reference/set_duration.md)),
carrying out statistical testing, or saving intermediate results
([`save()`](https://rdrr.io/r/base/save.html), etc.), then this function
can be used to set the argument `action` when creating a new milestone.
Note that the triggering time and number of observations/events of
endpoints at a milestone with `action = doNothing` is still recorded in
output automatically.

## Usage

``` r
doNothing(trial, ...)
```

## Arguments

- trial:

  an object returned from
  [`trial()`](https://zhangh12.github.io/TrialSimulator/reference/trial.md).

- ...:

  (optional) arguments. This is for capturing redundant arguments in
  [`milestone()`](https://zhangh12.github.io/TrialSimulator/reference/milestone.md)
  only.

## Value

This function returns `NULL`. Actually, nothing is done in this
function.
