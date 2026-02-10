# Triggering Condition by Number of Events or Non-missing Observations of an Endpoint

Define a condition to trigger trial milestone by the number of events of
a time-to-event endpoint or the number of non-missing observations of a
non-time-to-event endpoint. The milestone will be triggered when a trial
has observed at least the specified number of endpoint events (or
non-missing observations). It can be used combined with conditions
specified by
[calendarTime](https://zhangh12.github.io/TrialSimulator/reference/calendarTime.md)
and
[enrollment](https://zhangh12.github.io/TrialSimulator/reference/enrollment.md).

Number of events for a time-to-event endpoint can vary at different
milestones as more patients are randomized into a trial, or more events
onset over time.

Number of non-missing observations for a non-time-to-event endpoint can
vary at different milestones as more patients are randomized into a
trial, or more patients have been treated until their readout time
(thus, `NA` turns to a value).

Both numbers are affected by dropout.

Refer to the
[vignette](https://zhangh12.github.io/TrialSimulator/articles/conditionSystem.html)
to learn how to define milestones when performing simulation using
`TrialSimulator`.

## Usage

``` r
eventNumber(endpoint, n, ..., arms = NULL)
```

## Arguments

- endpoint:

  character. Name of an endpoint. It should be something that is
  specified in the argument `name` in
  [`endpoint()`](https://zhangh12.github.io/TrialSimulator/reference/endpoint.md).

- n:

  integer. Targeted number of events or non-missing obervations,
  depending on the type of endpoint.

- ...:

  subset conditions compatible with
  [`dplyr::filter`](https://dplyr.tidyverse.org/reference/filter.html).
  Number of events/observations will be counted on subset of trial data
  only.

- arms:

  vector of character. Name of arms on which the number of
  events/observations is counted. If `NULL`, use all arms that are not
  yet removed from the trial (using
  [`remove_arms()`](https://zhangh12.github.io/TrialSimulator/reference/remove_arms.md))
  by the time of calculation.

## Value

an object of class \`Condition\`
