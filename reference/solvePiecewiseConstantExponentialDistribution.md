# Compute Constant Rates of Piecewise Exponential Distribution

This function computes the rate parameters `lambda` in each of the time
windows. `lambda` are natural parameters of piecewise exponential
distribution, but in practice, users may define the distribution by
specifying the survival probabilities at time points where event rates
change.

This function returns a data frame, which can be used as input of the
argument `risk` of the data generator `PiecewiseConstantExponentialRNG`.

## Usage

``` r
solvePiecewiseConstantExponentialDistribution(surv_prob, times)
```

## Arguments

- surv_prob:

  numeric. A vector of survival probabilities at `times`.

- times:

  numeric. A vector of time points where event rates change. `times` and
  `surv_prob` must be of equal length.

## Value

a data frame of two columns

- `end_time`:

  End time for a constant event rate. The start time of the first time
  window is 0.

- piecewise_risk:

  A constant event rate in the time window ending with `end_time` on the
  same row.

## Examples

``` r
solvePiecewiseConstantExponentialDistribution(
  surv_prob = c(.9, .75, .64, .42, .28),
  times = c(.4, 1.2, 4, 5.5, 9)
)
#>   end_time piecewise_risk
#> 1      0.4     0.26340129
#> 2      1.2     0.22790195
#> 3      4.0     0.05664465
#> 4      5.5     0.28080898
#> 5      9.0     0.11584717
```
