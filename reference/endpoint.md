# Define Endpoints

Define one or multiple endpoints. This is a user-friendly wrapper for
the class constructor `Endpoint$new`. Users who are not familiar with
the concept of classes may consider using this wrapper directly.

Note that it is users' responsibility to assure that the units of
readout of non-tte endpoints, dropout time, and trial duration are
consistent.

## Usage

``` r
endpoint(name, type = c("tte", "non-tte"), readout = NULL, generator, ...)
```

## Arguments

- name:

  character vector. Name(s) of endpoint(s)

- type:

  character vector. Type(s) of endpoint(s) in `name`. It supports
  `"tte"` for time-to-event endpoints, and `"non-tte"` for all other
  types of endpoints (e.g., continuous, binary, categorical, or repeated
  measurement. `TrialSimulator` will do some verification if an endpoint
  is of type `"tte"`. However, no special manipulation is done for
  non-tte endpoints.

- readout:

  numeric vector named by non-tte endpoint(s). `readout` should be
  specified for every non-tte endpoint. For example,
  `c(endpoint1 = 6, endpoint2 = 3)`, which means that it takes 6 and 3
  unit time to get readouts of `endpoint1` and `endpoint2` of a patient
  since being randomized. For readouts of a longitudinal endpoint being
  collected at baseline (`baseline`) and 2 (`ep1`), 4 (`ep2`) unit time,
  its `readout` can be set as `c(baseline = 0, ep1 = 2, ep2 = 4)`. Error
  message will be prompted if `readout` is not named or is not specified
  for all non-tte endpoint, or it is specified for any tte endpoints. If
  all endpoints are tte, `readout` should be its default value `NULL`.

- generator:

  a RNG function. Its first argument must be `n`, number of patients. It
  must return a data frame of `n` rows. It supports all univariate
  random number generators, like those in `stats`, e.g.,
  [`stats::rnorm`](https://rdrr.io/r/stats/Normal.html),
  [`stats::rexp`](https://rdrr.io/r/stats/Exponential.html), etc. that
  with `n` as the first argument for number of observations. `generator`
  could be any custom functions as long as (1) its first argument is
  `n`; and (2) it returns a vector of length `n` or a data frame of `n`
  rows. Custom random number generator can return data of more than one
  endpoint. This is useful when users need to simulate correlated
  endpoints (e.g., longitudinal endpoints, or PFS/OS). The column names
  of returned data frame should match to the argument `name` exactly,
  but order does not matter. If an endpoint is of type `"tte"`, the
  custom `generator` should also return a column as event indicator. The
  column name of event indicator is `<endpoint name>_event`. For
  example, if `"pfs"` is `"tte"`, then custom `generator` should return
  at least two columns `"pfs"` and `"pfs_event"`. Usually `pfs_event`
  can be all 1s if no censoring. For other generators, e.g.,
  [`TrialSimulator::PiecewiseConstantExponentialRNG`](https://zhangh12.github.io/TrialSimulator/reference/PiecewiseConstantExponentialRNG.md)
  and
  [`TrialSimulator::CorrelatedPfsAndOs4`](https://zhangh12.github.io/TrialSimulator/reference/CorrelatedPfsAndOs4.md),
  the event indicators could take values 0/1 due to the nature of their
  algorithms. Censoring can also be specified later in
  [`trial()`](https://zhangh12.github.io/TrialSimulator/reference/trial.md)
  through its argument `dropout`. See
  [`?Trials`](https://zhangh12.github.io/TrialSimulator/reference/Trials.md).
  Note that if covariates, e.g., biomarker, subgroup, are needed in
  generating and analyzing trial data, they can and should be defined as
  endpoints as well.

- ...:

  (optional) arguments of `generator`.

## Examples

``` r
set.seed(12345)
## Example 1. Generate a time-to-event endpoint.
## Two columns are returned, one for time, one for event (1/0, 0 for
## A built-in RNG function is used to handle piecewise constant exponential
## distribution
risk <- data.frame(
  end_time = c(1, 10, 26.0, 52.0),
  piecewise_risk = c(1, 1.01, 0.381, 0.150) * exp(-3.01)
)

pfs <- endpoint(name = 'pfs', type='tte',
                generator = PiecewiseConstantExponentialRNG,
                risk = risk, endpoint_name = 'pfs')

# run it in R console to display a summary report
# event indicator takes values 0/1
pfs
#> Summary generated.

## Example 2. Generate continuous and binary endpoints using R's built-in
## RNG functions, e.g. rnorm, rexp, rbinom, etc.
ep1 <- endpoint(
         name = 'cd4', type = 'non-tte', generator = rnorm, readout = c(cd4=1),
         mean = 1.2)
ep2 <- endpoint(
         name = 'resp_time', type = 'non-tte', generator = rexp, readout = c(resp_time=0),
         rate = 4.5)
ep3 <- endpoint(
         name = 'orr', type = 'non-tte', readout = c(orr=3), generator = rbinom,
         size = 1, prob = .4)

ep1 # run it in R console. Mean and sd should be comparable to (1.2, 1.0)
#> Summary generated.

ep2 # run it in R console. Median should be comparable to log(2)/4.5 = 0.154
#> Summary generated.

ep3 # run it in R console. Mean and sd should be comparable to 0.4 and 0.49
#> Summary generated.


## Example3: delayed effect
## Use piecewise constant exponential random number generator
## Baseline hazards are piecewise constant
## Hazard ratios are piecewise constant, resulting a delayed effect.
## Note that this example is for explaining the concept of "endpoint".
## Generating endpoint data manually is not the recommended way to use this package.

run <- TRUE

if (!requireNamespace("survminer", quietly = TRUE)) {
  run <- FALSE
  message("Please install 'survminer' to run this example.")
}

if (!requireNamespace("survival", quietly = TRUE)) {
  run <- FALSE
  message("Please install 'survival' to run this example.")
}

if(run){
risk1 <- risk
ep1 <- endpoint(
  name = 'pfs', type='tte',
  generator = PiecewiseConstantExponentialRNG,
  risk=risk1, endpoint_name = 'pfs')

risk2 <- risk1
risk2$hazard_ratio <- c(1, 1, .6, .4)
ep2 <- endpoint(
  name = 'pfs', type='tte',
  generator = PiecewiseConstantExponentialRNG,
  risk=risk2, endpoint_name = 'pfs')

n <- 1000
tte <- rbind(ep1$get_generator()(n), ep2$get_generator()(n))
arm <- rep(0:1, each = n)
dat <- data.frame(tte, arm)
sfit <- survival::survfit(
  survival::Surv(time = pfs, event = pfs_event) ~ arm, dat)

survminer::ggsurvplot(sfit,
           data = dat,
           pval = TRUE,  # Show p-value
           conf.int = TRUE,  # Show confidence intervals
           risk.table = TRUE,  # Add risk table
           palette = c("blue", "red"))


## print summary reports for endpoint objects in console
ep1
ep2

}
#> Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
#> ℹ Please use `linewidth` instead.
#> ℹ The deprecated feature was likely used in the ggpubr package.
#>   Please report the issue at <https://github.com/kassambara/ggpubr/issues>.
#> Summary generated.

## Example 4: generate correlated pfs and os
## See vignette('simulatePfsAndOs')
```
