# Generate Enrollment Time from Piecewise Constant Uniform Distribution

It assumes a uniform enrollment with constant rate in each of the time
windows. This function can be used as the `enroller` when calling
[`trial()`](https://zhangh12.github.io/TrialSimulator/reference/trial.md)
to define a trial.

## Usage

``` r
StaggeredRecruiter(n, accrual_rate)
```

## Arguments

- n:

  integer. Number of random numbers.

- accrual_rate:

  a data frame of columns

  `end_time`

  :   End time for a constant rate in a time window. The start time of
      the first time window is 0.

  `piecewise_rate`

  :   A constant rate in a time window. So the number of patients being
      recruited in that window is window length x `piecewise_rate`.

## Examples

``` r
accrual_rate <- data.frame(
  end_time = c(12, 13:17, Inf),
  piecewise_rate = c(15, 15 + 6 * (1:5), 45)
)

StaggeredRecruiter(30, accrual_rate)
#>  [1] 0.00000000 0.06666667 0.13333333 0.20000000 0.26666667 0.33333333
#>  [7] 0.40000000 0.46666667 0.53333333 0.60000000 0.66666667 0.73333333
#> [13] 0.80000000 0.86666667 0.93333333 1.00000000 1.06666667 1.13333333
#> [19] 1.20000000 1.26666667 1.33333333 1.40000000 1.46666667 1.53333333
#> [25] 1.60000000 1.66666667 1.73333333 1.80000000 1.86666667 1.93333333

accrual_rate <- data.frame(
  end_time = c(3, 4, 5, 8, Inf),
  piecewise_rate = c(1, 2, 2, 3, 4)
)

StaggeredRecruiter(30, accrual_rate)
#>  [1]  0.000000  1.000000  2.000000  3.000000  3.500000  4.000000  4.500000
#>  [8]  5.000000  5.333333  5.666667  6.000000  6.333333  6.666667  7.000000
#> [15]  7.333333  7.666667  8.000000  8.250000  8.500000  8.750000  9.000000
#> [22]  9.250000  9.500000  9.750000 10.000000 10.250000 10.500000 10.750000
#> [29] 11.000000 11.250000
```
