# Updating Sampling Ratios of Existing Arms in a Trial

update sample ratios of arms. This could be called after an arm is added
or removed. Sample ratios can be updated for any existing arms.

This is a user-friendly wrapper of the member function of trial, i.e.,
`Trials$update_sample_ratio()`, which is used in vignettes. Users who
are not familiar with the concept of classes may consider using this
wrapper directly.

## Usage

``` r
update_sample_ratio(trial, arm_names, sample_ratios)
```

## Arguments

- trial:

  a trial object returned by
  [`trial()`](https://zhangh12.github.io/TrialSimulator/reference/trial.md).

- arm_names:

  character vector. Name of arms.

- sample_ratios:

  numeric vector. New sample ratios of arms. If sample ratio is a whole
  number, the permuted block randomization is adopted; otherwise,
  [`sample()`](https://rdrr.io/r/base/sample.html) will be used instead,
  which can cause imbalance between arms by chance. However, this is
  usually fine for simulation.
