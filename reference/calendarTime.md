# Triggering Condition by Calendar Time

Define a condition to trigger trial milestone by calendar time. The
milestone will be triggered when a trial has been running for at least
the specified duration since the first patient is enrolled. It can be
used combined with conditions specified by
[enrollment](https://zhangh12.github.io/TrialSimulator/reference/enrollment.md)
and
[eventNumber](https://zhangh12.github.io/TrialSimulator/reference/eventNumber.md).

Refer to the
[vignette](https://zhangh12.github.io/TrialSimulator/articles/conditionSystem.html)
to learn how to define milestones when performing simulation using
`TrialSimulator`.

## Usage

``` r
calendarTime(time)
```

## Arguments

- time:

  numeric. Calendar time to trigger a milestone of a trial.

## Value

an object of class \`Condition\`

## Examples

``` r
milestone(name = 'end of trial', when = calendarTime(time = 12))
#> <Milestones>
#>   Public:
#>     clone: function (deep = FALSE) 
#>     execute_action: function (trial) 
#>     get_action: function () 
#>     get_name: function () 
#>     get_trigger_condition: function () 
#>     get_trigger_status: function () 
#>     get_type: function () 
#>     initialize: function (name, type = name, trigger_condition, action = doNothing, 
#>     mute: function (silent) 
#>     reset: function () 
#>     set_dry_run: function (dry_run) 
#>     trigger_milestone: function (trial) 
#>   Private:
#>     action: function (trial, ...) 
#>     action_args: list
#>     is_dry_run: FALSE
#>     name: end of trial
#>     silent: FALSE
#>     trigger_condition: CalendarTimeCondition, Condition, R6
#>     triggered: FALSE
#>     type: end of trial
```
