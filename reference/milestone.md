# Define a Milestone

Define a milestone of a trial. This is a user-friendly wrapper for the
class constructor `Milestones$new()`. Users who are not familiar with
the concept of classes may consider using this wrapper directly.

A milestone means the time point to take an action, e.g., carrying out
(futility, interim, final) analysis for adding/removing arms, or
stopping a trial early. It can also be any more general time point where
trial data is used in decision making or adaptation. For example, one
can define a milestone for changing randomization scheme, sample size
re-assessment, trial duration extension etc.

Refer to the
[vignette](https://zhangh12.github.io/TrialSimulator/articles/conditionSystem.html)
to learn how to define milestones when performing simulation using
`TrialSimulator`.

## Usage

``` r
milestone(name, when, action = doNothing, ...)
```

## Arguments

- name:

  character. Name of milestone.

- when:

  condition to check if this milestone should be triggered. It taks
  value returned from functions
  [`calendarTime()`](https://zhangh12.github.io/TrialSimulator/reference/calendarTime.md),
  [`enrollment()`](https://zhangh12.github.io/TrialSimulator/reference/enrollment.md),
  [`eventNumber()`](https://zhangh12.github.io/TrialSimulator/reference/eventNumber.md)
  or their logic combinations.

- action:

  function to execute when the milestone triggers. If no action to be
  executed but simply need to record triggering time and number of
  events/non-missing observations of endpoints at a milestone, `action`
  can be its default value, a built-in function `doNothing`.

- ...:

  (optional) arguments of `action`.

## Examples

``` r
## See vignette('conditionSystem')

```
