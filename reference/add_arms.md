# Adding One or More Arms to a Trial

add one or more arms to a trial. This function can be used in two
scenarios: (1) adding arms right after a trial is created (i.e.,
`trial(...)`). `sample_ratio` and arms added through `...` must be of
same length; (2) adding arms to an existing trial in action functions of
milestones.

This is a user-friendly wrapper of the member function of trial, i.e.,
`Trials$add_arms()`, which is used in vignettes. Users who are not
familiar with the concept of classes may consider using this wrapper
directly.

## Usage

``` r
add_arms(trial, sample_ratio, ...)
```

## Arguments

- trial:

  a trial object returned by
  [`trial()`](https://zhangh12.github.io/TrialSimulator/reference/trial.md).

- sample_ratio:

  integer vector. Sample ratio for permuted block randomization. It will
  be appended to existing sample ratio in the trial.

- ...:

  one or more objects returned from
  [`arm()`](https://zhangh12.github.io/TrialSimulator/reference/arm.md).
  Randomization is carried out with updated sample ratio of newly added
  arm. Note that we can run `Trials$add_arm(sample_ratio1, arm1)`
  followed by `add_arms(trial, sample_ratio2, arm2)`. We would expected
  similar result with
  `add_arms(trial, c(sample_ratio1, sample_ratio2), arm1, arm2)`. Note
  that these two methods won't return exactly the same trial because
  randomization queue are generated twice in the first approach but only
  once in the second approach. But statistically, they are equivalent
  and of the same distribution.
