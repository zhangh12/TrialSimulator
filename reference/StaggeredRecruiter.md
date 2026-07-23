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

`StaggeredRecruiter` is the only enroller accepted by
[`trial()`](https://zhangh12.github.io/TrialSimulator/reference/trial.md):
a piecewise constant accrual rate is flexible enough to approximate
realistic recruitment in practice, e.g., site ramp-up, steady accrual,
and temporary pauses.

The returned enrollment times are deterministic, not random. Within a
window of positive rate, patients enroll one by one with spacing
`1/piecewise_rate`; under a constant rate `r`, the `k`-th patient
enrolls exactly at `k / r`. In particular, the first patient enrolls at
`1/piecewise_rate` rather than at time 0, and a milestone triggered by
`enrollment(n = n)` occurs exactly at the time the planned cumulative
accrual reaches `n`.

A window with `piecewise_rate = 0` models a recruitment pause (a hold
for safety review, a site not yet activated, a seasonal gap, etc.): no
patient is enrolled in that window, and enrollment resumes after its
`end_time`. Pauses may occur in the first window or span several
consecutive windows; a leading pause defers the first enrollment
accordingly.

A valid `accrual_rate` must satisfy all of the following:

- it is a data frame with columns `end_time` and `piecewise_rate`;

- `end_time` is positive and strictly increasing, and the last entry is
  `Inf` with a positive rate, so that the schedule can supply any number
  of patients (`TrialSimulator` may internally request more than the
  planned sample size, e.g., for adaptive resizing via
  [`resize()`](https://zhangh12.github.io/TrialSimulator/reference/resize.md));

- rates are non-negative and finite;

- a finite window with a positive rate must expect at least one patient,
  i.e., window length x `piecewise_rate` \>= 1. A tiny positive rate
  meant as a pause is rejected with an error; use `piecewise_rate = 0`
  for a true pause.

## Examples

``` r
## constant accrual of 25 patients/month: patient k enrolls at k / 25
accrual_rate <- data.frame(end_time = Inf, piecewise_rate = 25)

StaggeredRecruiter(30, accrual_rate)
#>  [1] 0.04 0.08 0.12 0.16 0.20 0.24 0.28 0.32 0.36 0.40 0.44 0.48 0.52 0.56 0.60
#> [16] 0.64 0.68 0.72 0.76 0.80 0.84 0.88 0.92 0.96 1.00 1.04 1.08 1.12 1.16 1.20

## recruitment pause: 30/mo through month 12, paused during months 12-18,
## then 30/mo again. Monthly counts show months 13-17 are empty and
## enrollment resumes at the end of the pause (month 18).
accrual_rate <- data.frame(
  end_time = c(12, 18, Inf),
  piecewise_rate = c(30, 0, 30)
)

enroll_time <- StaggeredRecruiter(400, accrual_rate)
table(ceiling(enroll_time))
#> 
#>  1  2  3  4  5  6  7  8  9 10 11 12 18 19 20 
#> 30 30 30 30 30 30 30 30 30 30 30 29  1 30 10 

## leading pause (first rate is 0): enrollment opens 3 months after study
## start, e.g., the first site is activated with a delay, then 30/mo
accrual_rate <- data.frame(
  end_time = c(3, Inf),
  piecewise_rate = c(0, 30)
)

StaggeredRecruiter(30, accrual_rate)
#>  [1] 3.033333 3.066667 3.100000 3.133333 3.166667 3.200000 3.233333 3.266667
#>  [9] 3.300000 3.333333 3.366667 3.400000 3.433333 3.466667 3.500000 3.533333
#> [17] 3.566667 3.600000 3.633333 3.666667 3.700000 3.733333 3.766667 3.800000
#> [25] 3.833333 3.866667 3.900000 3.933333 3.966667 4.000000

## approximate a linear ramp-up by monthly steps: accrual grows by 5/mo
## each month, from 5/mo up to 30/mo, then stays steady at 30/mo
accrual_rate <- data.frame(
  end_time = c(1:6, Inf),
  piecewise_rate = c(seq(5, 30, by = 5), 30)
)

enroll_time <- StaggeredRecruiter(200, accrual_rate)

## monthly enrolled counts show the ramp (5, 10, ..., 30) and the plateau (30)
table(ceiling(enroll_time))
#> 
#>  1  2  3  4  5  6  7  8  9 10 
#>  5 10 15 20 25 30 30 30 30  5 
```
