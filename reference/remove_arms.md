# Removing One or More Arms From a Trial

remove arms from a trial. The application of this function includes, but
is not limited to, dose selection, enrichment analysis (select
sub-population).

Note that this function should only be called within action functions of
milestones. It is users' responsibility to ensure that and
`TrialSimulator` has no way to track it. In addition, data of the
removed arms are censored or truncated by the time of arm removal.

This is a user-friendly wrapper of the member function of trial, i.e.,
`Trials$remove_arms()`, which is used in vignettes. Users who are not
familiar with the concept of classes may consider using this wrapper
directly.

## Usage

``` r
remove_arms(trial, arms_name)
```

## Arguments

- trial:

  a trial object returned by
  [`trial()`](https://zhangh12.github.io/TrialSimulator/reference/trial.md).

- arms_name:

  character vector. Name of arms to be removed.
