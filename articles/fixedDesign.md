# An Example of Fixed Design with Two Correlated Endpoints

In this vignette, we illustrate how to use `TrialSimulator` to simulate
a trial of two correlated endpoints. Under a fixed design, only one
milestone is specified for final analysis.

## Simulation Settings

- Trial consists of two active arms of high and low dose, and a
  standard-of-care arm.

- Patients are randomized into the trial with ratio `1:1:1`.

- 30 patients are randomized per month for the first 10 months, and 50
  patients per month after then until 1000 patients.

- Dropout rate is 10% at month 18, which is modeled by an exponential
  distribution, with rate parameter `-log(1 - 0.1)/18`.

- Modeled by a three-state ill-death model, two endpoints `PFS` and `OS`
  are simulated.

  - Primary endpoint `OS` has a medians of 15, 18.5, 20 months in
    standard-of-care, low dose and high dose arms, respectively.
  - Key secondary endpoint `PFS` has a median of 7, 9, 10 months in the
    three arms.
  - Pearson’s correlation between `OS` and `PFS` are 0.68, 0.65, and
    0.60 in the three arms.

- To ensures sufficient powers for both endpoints, final analysis is set
  when we have at least a total of 800 `PFS` events in the
  standard-of-care and high dose arm, meanwhile a total of 550 `OS`
  events are observed in three arms

  - `OS` is tested using one-sided logrank test.
  - `PFS` is tested using one-sided p-value from the Cox proportional
    hazard model as the PH assumption is assumed.
  - The Bonferroni correction is adopted to split overall
    $\alpha = 5\%$, i.e., claiming significant effect for an endpoint
    when p-value is lower than $0.05/4$.

## Transition Hazards of `PFS` and `OS`

We adopt the ill-death model to simulate the two endpoints. This ensures
`PFS` $\leq$`OS` with probability one, and makes no assumption on latent
variables or copula parameters. `TrialSimulator` offers a function
[`solveThreeStateModel()`](https://zhangh12.github.io/TrialSimulator/reference/solveThreeStateModel.md)
to convert endpoints’ medians and correlation to the transition hazards,
which are required by the built-in generator
[`CorrelatedPfsAndOs3()`](https://zhangh12.github.io/TrialSimulator/reference/CorrelatedPfsAndOs3.md).
Refer to the vignette [Simulate Correlated Progression-Free Survival and
Overall Survival as Endpoints in Clinical
Trials](https://zhangh12.github.io/TrialSimulator/articles/simulatePfsAndOs.md)
for more details.

``` r
pars_soc <- solveThreeStateModel(median_pfs = 7, median_os = 15, corr = .68, 
                                 h12 = seq(.07, .10, length.out = 50))
pars_soc
#>   corr        h01       h02        h12        error
#> 1 0.68 0.07498342 0.0240376 0.08959184 0.0001363832
```

``` r
pars_low <- solveThreeStateModel(median_pfs = 9, median_os = 18.5, corr = .65, 
                                 h12 = seq(.04, .07, length.out = 50))
pars_low
#>   corr        h01        h02        h12       error
#> 1 0.65 0.05059501 0.02642134 0.06204082 0.001646964
```

``` r
pars_high <- solveThreeStateModel(median_pfs = 10, median_os = 20, corr = .60, 
                                  h12 = seq(.02, .06, length.out = 50))
pars_high
#>   corr        h01        h02        h12      error
#> 1  0.6 0.03964373 0.02967099 0.04693878 0.00208503
```

| arm  |   h01 |   h02 |   h12 |
|:-----|------:|------:|------:|
| soc  | 0.075 | 0.024 | 0.090 |
| low  | 0.051 | 0.026 | 0.062 |
| high | 0.040 | 0.030 | 0.047 |

Transition hazards in three treatment arms

## Define Treatment Arms

We use generator
[`CorrelatedPfsAndOs3()`](https://zhangh12.github.io/TrialSimulator/reference/CorrelatedPfsAndOs3.md)
with
[`endpoint()`](https://zhangh12.github.io/TrialSimulator/reference/endpoint.md)
and
[`arm()`](https://zhangh12.github.io/TrialSimulator/reference/arm.md) to
define the three treatment arms.

``` r
#' define SoC
pfs_os_in_soc <- endpoint(name = c('pfs', 'os'), 
                          type = c('tte', 'tte'), 
                          generator = CorrelatedPfsAndOs3, 
                          h01 = 0.075, h02 = 0.024, h12 = 0.090)

soc <- arm(name = 'soc')
soc$add_endpoints(pfs_os_in_soc)

#' define low dose arm
pfs_os_in_low <- endpoint(name = c('pfs', 'os'), 
                          type = c('tte', 'tte'), 
                          generator = CorrelatedPfsAndOs3, 
                          h01 = 0.051, h02 = 0.026, h12 = 0.062)

low <- arm(name = 'low')
low$add_endpoints(pfs_os_in_low)

#' define high dose arm
pfs_os_in_high <- endpoint(name = c('pfs', 'os'), 
                           type = c('tte', 'tte'), 
                           generator = CorrelatedPfsAndOs3, 
                          h01 = 0.040, h02 = 0.030, h12 = 0.047)

high <- arm(name = 'high')
high$add_endpoints(pfs_os_in_high)
```

We can request for a summary report of, e.g., the high dose arm, by
printing the arm object in R console. The medians of `PFS` and `OS`
matches to the settings very well.

``` r
high
```

## Define a Trial

With three arms, we can define a trial using the function
[`trial()`](https://zhangh12.github.io/TrialSimulator/reference/trial.md).
Recruitment curve are specified through `enroller` with a built-in
function `StaggeredRecruiter` of piecewise constant rate. We set
`duration` to be an arbitrary large number (500) but controlling the end
of trial through a pre-defined milestone later. Note that if
`seed = NULL`, `TrialSimulator` will pick a seed for the purpose of
reproducibility.

``` r
accrual_rate <- data.frame(end_time = c(10, Inf),
                           piecewise_rate = c(30, 50))
trial <- trial(
  name = 'Trial-3415', n_patients = 1000,
  seed = 1727811904, duration = 500,
  enroller = StaggeredRecruiter, accrual_rate = accrual_rate,
  dropout = rexp, rate = -log(1 - 0.1)/18, ## 10% by month 18
  silent = TRUE
)

trial$add_arms(sample_ratio = c(1, 1, 1), soc, low, high) ## 1:1:1
trial
#>  ⚕⚕ Trial Name:  Trial-3415  
#>  ⚕⚕ Description:  Trial-3415  
#>  ⚕⚕ Number of Arms:  3  
#>  ⚕⚕ Registered Arms:  soc, low, high  
#>  ⚕⚕ Sample Ratio:  1, 1, 1  
#>  ⚕⚕ Number of Patients:  1000  
#>  ⚕⚕ Planned Duration:  500  
#>  ⚕⚕ Random Seed:  1727811904
```

## Define Milestone and Action for Final Analysis

To ensure sufficient powers of testing `PFS` and `OS`, final analysis is
performed when we have at least 700 events for `OS` and 800 events for
`PFS`. In the action function, we compute one-sided p-value of `PFS`
using the proportional hazard Cox model, and one-sided p-value of `OS`
using the logrank test. This is consistent with the assumption of
ill-death model implemented in data generator
[`CorrelatedPfsAndOs3()`](https://zhangh12.github.io/TrialSimulator/reference/CorrelatedPfsAndOs3.md).
Note that five columns are available in locked data: `arm`, `pfs`, ‘os’,
‘pfs_event’, and ‘os_event’, which are used to construct model formula.
Estimates of hazard ratio are also computed. Built-in functions
`fitCoxph` and `fitLogrank` return data frames. Refer to their help
documants for more details.

``` r
action <- function(trial){
  
  locked_data <- trial$get_locked_data('final')
  
  pfs <- fitCoxph((Surv(pfs, pfs_event) ~ arm), placebo = 'soc', 
                  data = locked_data, alternative = 'less', 
                  scale = 'hazard ratio')
  
  os <- fitLogrank((Surv(os, os_event) ~ arm), placebo = 'soc', 
                   data = locked_data, alternative = 'less')
  
  ## Bonferroni test is applied to four hypotheses: 
  ## PFS_low, PFS_high, OS_low, and OS_high
  pfs$decision <- ifelse(pfs$p < .05/4, 'reject', 'accept')
  os$decision <- ifelse(os$p < .05/4, 'reject', 'accept')
  
  trial$save(
    value = pfs %>% filter(arm == 'low') %>% select(estimate, decision, info), 
    name = 'pfs_low')
  
  trial$save(
    value = pfs %>% filter(arm == 'high') %>% select(estimate, decision, info), 
    name = 'pfs_high')
  
  trial$save(
    value = os %>% filter(arm == 'low') %>% select(decision, info), 
    name = 'os_low')
  
  trial$save(
    value = os %>% filter(arm == 'high') %>% select(decision, info), 
    name = 'os_high')
  
}
```

Now we can define and register the milestone to a listener, which
monitors the trial for us through a controller

``` r
final <- milestone(name = 'final', action = action, 
                   when = eventNumber(endpoint = 'pfs', n = 450, 
                                      arms = c('soc', 'high')) & 
                     eventNumber(endpoint = 'os', n = 550)
                   )

listener <- listener()
listener$add_milestones(final)
#> A milestone <final> is registered.

controller <- controller(trial, listener)
```

We can run a massive number of replicates in simulation to study
operating characteristics of a trial design by specifying `n` in
`Controller$run()`. The simulation results can be accessed by calling
the member function `get_output()` of the controller.

``` r
controller$run(n = 1000, plot_event = FALSE, silent = TRUE)
output <- controller$get_output()
```

``` r
output %>% 
  head(5) %>% 
  kable(escape = FALSE) %>% 
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE,
                position = "left") %>%
  scroll_box(width = "100%")
```

| trial      |       seed | milestone_time\_\<final\> | n_events\_\<final\>\_\<patient_id\> | n_events\_\<final\>\_\<pfs\> | n_events\_\<final\>\_\<os\> | n_events\_\<final\>\_\<arms\> | pfs_low\_\<estimate\> | pfs_low\_\<decision\> | pfs_low\_\<info\> | pfs_high\_\<estimate\> | pfs_high\_\<decision\> | pfs_high\_\<info\> | os_low\_\<decision\> | os_low\_\<info\> | os_high\_\<decision\> | os_high\_\<info\> | error_message |
|:-----------|-----------:|--------------------------:|------------------------------------:|-----------------------------:|----------------------------:|:------------------------------|----------------------:|:----------------------|------------------:|-----------------------:|:-----------------------|-------------------:|:---------------------|-----------------:|:----------------------|------------------:|:--------------|
| Trial-3415 | 1727811904 |                  39.79807 |                                1000 |                          726 |                         550 | c(334, 2….                    |             0.8373861 | accept                |               490 |              0.8470156 | accept                 |                485 | reject               |              372 | accept                |               378 |               |
| Trial-3415 | 1580839845 |                  36.97289 |                                1000 |                          731 |                         550 | c(334, 2….                    |             0.8293869 | accept                |               504 |              0.7566906 | reject                 |                491 | reject               |              383 | accept                |               372 |               |
| Trial-3415 | 1644140445 |                  35.81142 |                                1000 |                          738 |                         550 | c(334, 2….                    |             0.7382446 | reject                |               498 |              0.7208539 | reject                 |                504 | accept               |              368 | accept                |               372 |               |
| Trial-3415 |  157892763 |                  36.98769 |                                1000 |                          732 |                         550 | c(334, 2….                    |             0.6802710 | reject                |               502 |              0.6518396 | reject                 |                490 | reject               |              389 | reject                |               380 |               |
| Trial-3415 | 1672612707 |                  37.67231 |                                1000 |                          745 |                         550 | c(334, 2….                    |             0.8062252 | reject                |               513 |              0.6809993 | reject                 |                493 | reject               |              381 | reject                |               374 |               |

For example, we can compute the powers and summarize the estimates of
hazard ratio for `PFS`.

``` r
output %>% 
  summarise(
    across(matches('_<decision>$'), ~ mean(. == 'reject') * 100, .names = 'Power_{.col}'), 
    across(matches('_<estimate>$'), ~ mean(.x), .names = 'HR_{.col}')
  ) %>%
  rename_with(~ sub('_<decision>$', '', .), starts_with('Power_')) %>%
  rename_with(~ sub('_<estimate>$', '', .), starts_with('HR_')) %>% 
  kable(col.name = NULL, digits = 3, align = 'r') %>% 
  add_header_above(c('Low', 'High', 'Low', 'High', 'Low', 'High'), align = 'r') %>% 
  add_header_above(c('PFS' = 2, 'OS' = 2, 'PFS' = 2)) %>% 
  add_header_above(c('Power (%)' = 4, 'Hazard Ratio' = 2)) %>% 
  kable_styling(full_width = TRUE)
```

[TABLE]

Note that `OS` does not satisfy the proportional hazard assumption, and
a composite condition on event numbers is used to trigger the final
analysis. Thus, the powers in the table above would not match to the
output from power calculation packages, which is as expected.
