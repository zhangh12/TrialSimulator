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

  integer. Number of enrollment times to generate.

- accrual_rate:

  a data frame of columns

  `end_time`

  :   End time for a constant rate in a time window. The start time of
      the first time window is 0. Values must be positive and strictly
      increasing; the last one must be `Inf`.

  `piecewise_rate`

  :   A constant rate in a time window. So the number of patients being
      recruited in that window is window length x `piecewise_rate`. A
      rate of 0 pauses enrollment for that window. Rates must be
      non-negative and finite; the last must be positive.

## Details

Enrollment times are the deterministic inverse of the cumulative accrual
intensity: patient `k` (counting from 0) enrolls when the expected
cumulative enrollment reaches `k`. Consequently the cumulative accrual
capacity increases by window length x `piecewise_rate` across a window
(so a window whose capacity is an integer holds exactly that many
patients), and within a window with a positive rate consecutive patients
are spaced by `1 / piecewise_rate`. When the first window has a positive
rate, the first patient enrolls at time 0.

A window may have `piecewise_rate = 0` to model a recruitment pause (a
hold for safety review, a site not yet activated, a seasonal gap, etc.).
No patient is enrolled during a pause window, but calendar time still
advances across it, so enrollment resumes at the window's `end_time`.
Pauses may occur in the first window or span several consecutive
windows. A leading pause therefore defers the first enrollment to the
end of the pause rather than time 0.

The last `end_time` must be `Inf` with a positive rate, so that the
schedule can supply any number of patients. (`TrialSimulator` may
internally request several times the planned sample size for adaptive
resizing; an open-ended final window keeps that from failing.)

A positive rate too low for its window – one expecting fewer than a
single patient (window length x `piecewise_rate` \< 1) – is almost
always a misspecification (e.g. a tiny rate meant as a pause) and raises
an error. A window expecting exactly one patient (product equal to 1) is
allowed; the check uses a small floating-point tolerance so that
`rate = 1 / width` is not rejected when the product rounds just below 1.
Use `piecewise_rate = 0` for a true pause, or a rate of at least 1 /
width.

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

## recruitment pause: 30/mo for 12 months, paused for months 12-18, then 30/mo
accrual_rate <- data.frame(
  end_time = c(12, 18, Inf),
  piecewise_rate = c(30, 0, 30)
)

StaggeredRecruiter(30, accrual_rate)
#>  [1] 0.00000000 0.03333333 0.06666667 0.10000000 0.13333333 0.16666667
#>  [7] 0.20000000 0.23333333 0.26666667 0.30000000 0.33333333 0.36666667
#> [13] 0.40000000 0.43333333 0.46666667 0.50000000 0.53333333 0.56666667
#> [19] 0.60000000 0.63333333 0.66666667 0.70000000 0.73333333 0.76666667
#> [25] 0.80000000 0.83333333 0.86666667 0.90000000 0.93333333 0.96666667
```
