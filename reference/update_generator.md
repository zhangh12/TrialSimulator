# Updating Data Generators of One or More Endpoints in an Arm

update endpoint generator in an arm. This function can be useful in,
e.g., enrichment design where generator is updated so that patients are
enrolled from sub-population afterwards. This function can also be used
when data model changes over time, i.e., generating data in a new way
after a milestone. This function can be called multiple times to update
generators of endpoints one by one.

Note that this function should only be called within action functions of
milestones. It is users' responsibility to ensure that and
`TrialSimulator` has no way to track it.

This is a user-friendly wrapper of the member function of trial, i.e.,
`Trials$update_generator()`, which is used in vignettes. Users who are
not familiar with the concept of classes may consider using this wrapper
directly.

## Usage

``` r
update_generator(trial, arm_name, endpoint_name, generator, ...)
```

## Arguments

- trial:

  a trial object returned by
  [`trial()`](https://zhangh12.github.io/TrialSimulator/reference/trial.md).

- arm_name:

  character. Name of an arm.

- endpoint_name:

  character. A vector of endpoint names whose generator is updated.

- generator:

  a random number generation (RNG) function. See generator of
  [`endpoint()`](https://zhangh12.github.io/TrialSimulator/reference/endpoint.md).

- ...:

  optional arguments for generator.
