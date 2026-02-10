# Resizing a Trial

resize a trial with a greater sample size. This function is used to
update the maximum sample size adaptively after sample size
reassessment. Note that this function should be called within action
functions. It is users' responsibility to ensure it and `TrialSimulator`
has no way to track this.

This is a user-friendly wrapper of the member function of trial, i.e.,
`Trials$resize()`, which is used in vignettes. Users who are not
familiar with the concept of classes may consider using this wrapper
directly.

## Usage

``` r
resize(trial, n_patients)
```

## Arguments

- trial:

  a trial object returned by
  [`trial()`](https://zhangh12.github.io/TrialSimulator/reference/trial.md).

- n_patients:

  integer. Number of maximum sample size of a trial.
