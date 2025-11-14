# Generate Time-to-Event Endpoint from Piecewise Constant Exponential Distribution

This function can be used as generator to define endpoint.
Implementation is based on [this
algorithm](https://www.demogr.mpg.de/papers/technicalreports/tr-2010-003.pdf).
This distribution can be used to simulate delayed treatment effect.

## Usage

``` r
PiecewiseConstantExponentialRNG(n, risk, endpoint_name)
```

## Arguments

- n:

  integer. Number of random numbers

- risk:

  a data frame of columns

  `end_time`

  :   End time for a constant risk in a time window. The start time of
      the first time window is 0.

  `piecewise_risk`

  :   A constant risk in a time window, which is absolute risk \*
      relative risk, or (h0 \* g) in the link.

  `hazard_ratio`

  :   An optional column for simulating an active arm. If absent, a
      column of 1s will be added. Equivalently, user can multiply
      `piecewise_risk` by `hazard_ratio` manually and ignore this
      column.

- endpoint_name:

  character. Name of endpoint. This should be the same as the `name`
  argument when calling function
  [`endpoint()`](https://zhangh12.github.io/TrialSimulator/reference/endpoint.md).

## Value

a data frame of `n` rows and two columns

- `<endpoint_name>`:

  name of endpoint specified by users in `endpoint_name`.

- \<endpoint_name\>\_event:

  event indicator with 0/1 as censoring and event, respectively. Note
  that due to the nature of the algorithm to generate data from this
  distribution, it is possible to have the endpoint being censoring at
  the last `end_time` unless it is set to `Inf`.

## Examples

``` r
# example code
# In this example, absolute risk in [0, 1) and [26, 52] are 0.0181 and
# 0.0027, respectively.
risk <- data.frame(
  end_time = c(1, 4.33, 26.0, 52.0),
  piecewise_risk = c(1, 1.01, 0.381, 0.150) * exp(-4.01)
)
PiecewiseConstantExponentialRNG(10, risk, 'PFS')
#>    PFS PFS_event
#> 1   52         0
#> 2   52         0
#> 3   52         0
#> 4   52         0
#> 5   52         0
#> 6   52         0
#> 7   52         0
#> 8   52         0
#> 9   52         0
#> 10  52         0
```
