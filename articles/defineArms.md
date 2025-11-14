# Define and Summarize Arms in Clinical Trials

In `TrialSimulator`, a trial arm is defined as a collection of endpoints
(and potentially other covariates or biomarkers) with a data generation
process. This vignette demonstrates how to use the following key
functions to define and summarize arms in a simulated clinical trial
setting.

- `endpoint`: Creates one or more endpoints
- `add_endpoints`: Add one or more endpoints to an arm
- `generate_data`: Generates a dataset from an `Arms` object (for
  exploratory purpose)
- `print`: Method that displays a summary report of an `Arms` object
  summarizing all endpoints in the arm

## Define Arm with Mulitple Sets of Endpoints

The function `endpoint` can be used to define one or multiple endpoints
simultaneously. These endpoints can be independent or correlated,
depending on the `generator` provided. In the following hypothetical
example, we construct a custom generator that simulates PFS, OS, PSA
levels at baseline and year 1. A pre-specified correlation matrix
ensures the endpoints are appropriately correlated. We also ensure that
PFS is always less than or equal to OS.

``` r
rng <- function(n, pfs_rate, os_rate, psa_mean, psa_sd, corr_matrix){
  
  dist <- list()
  dist[['PFS']] <- function(x) qexp(x, rate = pfs_rate)
  dist[['OS']] <- function(x) qexp(x, rate = os_rate)
  dist[['PSA_baseline']] <- function(x) qnorm(x, mean = psa_mean, sd = psa_sd)
  dist[['PSA_year1']] <- function(x) qnorm(x, mean = psa_mean - 12, sd = psa_sd)
  dsgn = simdata::simdesign_norta(cor_target_final = corr_matrix, 
                                dist = dist, 
                                transform_initial = data.frame,
                                names_final = names(dist), 
                                seed_initial = 1)
  
  simdata::simulate_data(dsgn, n_obs = n) %>% 
    mutate(PFS = pmin(PFS, OS)) %>% 
    mutate(PFS_event = 1, OS_event = 1)
  
}
```

In this generator,

- Baseline PSA follows a normal distribution with mean 20 and SD 4.0.
- After one year of treatment, the average PSA level decreases to 8.
- PFS and OS are exponentially distributed with median times of 2.5 and
  4.5 years, respectively.
- A constraint ensures that PFS does not exceed OS. To define correlated
  PFS and OS that satisfy this constraint in a more natural way, refer
  to the vignette of [multi-state
  model](https://zhangh12.github.io/TrialSimulator/articles/simulatePfsAndOs.md)
- PSA readout times are 0 (at baseline) and 1 (at year 1).

The following code defines the endpoints and uses the `print` method to
generate a summary report based on 10,000 samples from the generator
`rng`.

``` r
ep1 <- endpoint(name = c('PSA_baseline', 'PSA_year1', 'OS', 'PFS'), 
                type = c('non-tte', 'non-tte', 'tte', 'tte'), 
                readout = c(PSA_baseline = 0, PSA_year1 = 1), 
                generator = rng, 
                pfs_rate = log(2)/2.5, os_rate = log(2)/4.5, 
                psa_mean = 20, psa_sd = 4, 
                corr_matrix = matrix(c(1, .6, -.5, -.4, 
                                       .6, 1, -.4, -.3, 
                                       -.5, -.4, 1, .7, 
                                       -.4, -.3, .7, 1), nrow = 4))

ep1
```

We can define another set of endpoints using a separate call to
[`endpoint()`](https://zhangh12.github.io/TrialSimulator/reference/endpoint.md).
However, keep in mind that any endpoints defined separately are assumed
to be independent of those in prior calls (i.e. `PSA_baseline`,
`PSA_year1`, `PFS` and `OS`).

In the following example, we define a biomarker, even though it is
actually not an endpoint. In practice, the function `endpoint` is useful
in introducing any variables, including covariates, biomarkers,
sub-group indicators, etc. Ideally, a biomarker should be integrated
into the generator `rng` to capture meaningful correlation with other
endpoints.

``` r
ep2 <- endpoint(name = 'biomarker', 
                type = 'non-tte', 
                readout = c(biomarker = 0), 
                generator = rbinom, 
                size = 1, prob = .3)
ep2
```

We now create a treatment arm by combining `ep1` and `ep2`. The `print`
method automatically summarizes the marginal distributions of all
endpoints. As seen, the summary report of the arm simply concatenates
the two reports of `ep1` and `ep2`.

``` r
trt <- arm(name = 'treated')
trt$add_endpoints(ep1, ep2)
trt
```

## Add Inclusion Criteria for the Arm

We can define inclusion criteria for the arm by passing logical filter
expressions via the `...` argument in
[`arm()`](https://zhangh12.github.io/TrialSimulator/reference/arm.md).
These filters are applied to the generated trial data. For example, the
following code restricts enrollment to patients with

- Baseline PSA \> 10
- Positive PSA values at year 1

The summary report will reflect the effect of these inclusion criteria
on the simulated population.

``` r
trt <- arm(name = 'treated', PSA_baseline > 10 & PSA_year1 > 0)
trt$add_endpoints(ep1, ep2)
trt
```

## Simulate Data Explicitly (Not Recommended)

Although `TrialSimulator` allows direct data generation using the
`generate_data()` method, it is generally discouraged. One of the core
principles of `TrialSimulator` is to separate trial simulation logic
from data generation, allowing the framework to manage data generation
and truncation (and/or censoring) dynamically based on trial milestones.

Nevertheless, for inspection or debugging, one can call

``` r
## not recommended
tmp <- trt$generate_data(100)
head(tmp)
#>         PFS         OS PSA_baseline PSA_year1 PFS_event OS_event
#> 1 1.2410022  2.9010900     17.05084 7.4786703         1        1
#> 2 1.5231352  4.0614705     14.86082 6.7132135         1        1
#> 3 0.8116851  3.0751050     24.34986 9.0405846         1        1
#> 4 3.5868602 10.1470295     13.49077 0.2577257         1        1
#> 5 1.2952825  1.2952825     21.89733 8.6974747         1        1
#> 6 0.3332482  0.3332482     20.30860 7.4350724         1        1
#>   PSA_baseline_readout PSA_year1_readout biomarker biomarker_readout
#> 1                    0                 1         0                 0
#> 2                    0                 1         0                 0
#> 3                    0                 1         1                 0
#> 4                    0                 1         1                 0
#> 5                    0                 1         0                 0
#> 6                    0                 1         0                 0
```

This gives a preview of the patient-level data generated by the
treatment arm configuration (generator, inclusion filters). However,
enrollment schedule and dropout are not taken into account, which is
another reason why we strongly discourage this way to use
`TrialSimulator`.
