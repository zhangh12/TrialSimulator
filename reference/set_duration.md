# Extending Duration of a Trial

set trial duration in an adaptive designed trial. New duration must be
longer than the old one. All patients enrolled before resetting the
duration are truncated (non-TTE endpoints) or censored (TTE endpoints)
at the original duration. This helps maintain proper type I error or
family-wise error rate and control multiplicity when conducting testing
statistics. For more details of why this is necessary, please refer to
[Jorgens et al.
2019](https://onlinelibrary.wiley.com/doi/10.1002/pst.1926).

Note that this function should only be called within action functions of
milestones. It is users' responsibility to ensure that and
`TrialSimulator` has no way to track it.

This is a user-friendly wrapper of the member function of trial, i.e.,
`Trials$set_duration()`, which is used in vignettes. Users who are not
familiar with the concept of classes may consider using this wrapper
directly.

## Usage

``` r
set_duration(trial, duration)
```

## Arguments

- trial:

  a trial object returned by
  [`trial()`](https://zhangh12.github.io/TrialSimulator/reference/trial.md).

- duration:

  new duration of a trial. It must be greater than the current duration.
