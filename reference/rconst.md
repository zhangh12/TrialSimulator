# Generate Constant Variable

A random number generator returning only a constant. This can be used to
set dropout time. Currently it is the default value of dropout time,
with `value = Inf`.

This function can also be used as a generator of
[`endpoint()`](https://zhangh12.github.io/TrialSimulator/reference/endpoint.md)
if a constant endpoint is needed.

## Usage

``` r
rconst(n, value)
```

## Arguments

- n:

  integer. Number of observations.

- value:

  scalar. Value of constant observations.
