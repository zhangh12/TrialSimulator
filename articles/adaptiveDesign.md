# An Example of Simulating a Trial with Adaptive Design

In this vignette, we illustrate how to use `TrialSimulator` to simulate
a trial with seamless adaptive design of multiple endpoints, where dose
selection, futility analysis, interim analysis are triggered by
user-defined milestones. Family-wise error rate are controlled by closed
test with Dunnett’s test, with $\alpha$ split for the endpoints. Please
refer to another vignette where graphical testing procedure is adopted
instead.

## Simulation Settings

- Trial consists of two active arms of high or low dose, and a placebo
  arm.
- Patients are randomized into the trial with ratio `1:1:1`.
- 30 patients are randomized per month for the first 10 months, and 50
  patients per month after then until 1000 patients.
- Dropout rate is 8% at month 12, and 18% at month 18, which is modeled
  by a Weibull distribution. The scale and shape parameters of the
  Weibull distribution can be computed using helper function

``` r
weibullDropout(time = c(12, 18), dropout_rate = c(.08, .18))
#>     shape     scale 
#>  2.138567 38.343517
```

- Two time-to-event endpoints (`PFS` and `OS`) and a binary `surrogate`
  are under consideration, where a higher rate of `surrogate` is better.
- `surrogate` has a readout time of 5 weeks.
- `surrogate` is used for dose selection using a hypothetical rule. Let
  $z_{h}$ (or $z_{l}$) be the z statistics of `surrogate` between high
  (or low) dose and placebo.
  - low dose arm is selected if $z_{l} > 1.28$;
  - high dose arm is selected if $z_{h} > 1.28 > z_{l}$;
  - keep both arms otherwise.
- Rates of `surrogate` are
  - 0.05 in placebo
  - 0.12 in low dose arm
  - 0.13 in high dose arm
- Medians of PFS are
  - 5 months in placebo
  - 6.7 months in low dose arm (hazard ratio 0.75)
  - 7.1 months in high dose arm (hazard ratio 0.70) Exponential
    distribution is assumed.
- Medians of OS are
  - 14 months in placebo
  - 17.5 months in low dose arm (hazard ratio 0.80)
  - 18.2 months high dose arms (hazard ratio 0.77)
- Dose selection is done when 300 patients are randomized with readout
  of `surrogate` are ready for analysis.
- An interim is set to test `PFS` when 300 `PFS` events are observed. A
  non-binding futility analysis is also done with a boundary 0.5.
- Final analysis is set for `PFS` and `OS` when all planned 1000
  patients are randomized and at least 300 `OS` events are observed.
  Meanwhile, the trial has reached at least 28 months or at least 520
  events are observed for `PFS`.
- To control the family-wise error rate accounting for `PFS` and `OS`,
  as well as adaptation of dose-selection, we split $\alpha$ between
  `PFS` (0.5%) and `OS` (2%), and adopt closed test with Dunnett’s test
  for intersection hypotheses.

## Define Three Arms

In this example, we call
[`endpoint()`](https://zhangh12.github.io/TrialSimulator/reference/endpoint.md)
twice for each arm to define `PFS` and `OS`. This implicitly assumes
independence between the two time-to-event endpoints. `TrialSimulator`
offers a built-in generator
[`CorrelatedPfsAndOs3()`](https://zhangh12.github.io/TrialSimulator/reference/CorrelatedPfsAndOs3.md)
to generate `PFS` and `OS` with correlation based on the ill-death
model, which enjoys several nice properties. For selection of its
arguments (i.e.  transition hazards), refer to vignette [Simulate
Correlated Progression-Free Survival and Overall Survival as Endpoints
in Clinical
Trials](https://zhangh12.github.io/TrialSimulator/articles/simulatePfsAndOs.md).

``` r
#' define three arms
pbo <- arm(name = 'placebo')
low <- arm(name = 'low dose')
high <- arm(name = 'high dose')

#' define endpoints in placebo
pfs <- endpoint(name = 'pfs', type = 'tte',
                generator = rexp, rate = log(2) / 5)

os <- endpoint(name = 'os', type = 'tte',
               generator = rexp, rate = log(2) / 14)

five_weeks <- 5 / 52 * 12 ## convert it in months
surrogate <- endpoint(name = 'surrogate', type = 'non-tte',
                      readout = c(surrogate = five_weeks),
                      generator = rbinom, size = 1, prob = .05)
pbo$add_endpoints(pfs, os, surrogate)

#' define endpoints in low dose arm
pfs <- endpoint(name = 'pfs', type = 'tte',
                generator = rexp, rate = log(2) / 6.7)

os <- endpoint(name = 'os', type = 'tte',
               generator = rexp, rate = log(2) / 17.5)

surrogate <- endpoint(name = 'surrogate', type = 'non-tte',
                      readout = c(surrogate = five_weeks),
                      generator = rbinom, size = 1, prob = .12)
low$add_endpoints(pfs, os, surrogate)

#' define endpoints in high dose arm
pfs <- endpoint(name = 'pfs', type = 'tte',
                generator = rexp, rate = log(2) / 7.1)

os <- endpoint(name = 'os', type = 'tte',
               generator = rexp, rate = log(2) / 18.2)

surrogate <- endpoint(name = 'surrogate', type = 'non-tte',
                      readout = c(surrogate = five_weeks),
                      generator = rbinom, size = 1, prob = .13)
high$add_endpoints(pfs, os, surrogate)
```

## Define a Trial

With three arms, we can define a trial of class `Trial`. Recruitment
curve are specified through `enroller` with a built-in function
`StaggeredRecruiter` of piecewise constant rate. We set `duration` to be
an arbitrary large number (40) but controlling the end of trial through
pre-defined milestones later. Note that if `seed = NULL`,
`TrialSimulator` will pick a seed for the purpose of reproducibility.

``` r
accrual_rate <- data.frame(end_time = c(10, Inf),
                           piecewise_rate = c(30, 50))
trial <- trial(
  name = 'Trial-3415', n_patients = 1000,
  seed = 1727811904, duration = 40,
  enroller = StaggeredRecruiter, accrual_rate = accrual_rate,
  dropout = rweibull, shape = 2.139, scale = 38.343
)

trial$add_arms(sample_ratio = c(1, 1, 1), low, high, pbo)
#> Arm(s) <low dose, high dose, placebo> are added to the trial.
#> Randomization is done for 1000 potential patients.
#> Data of 1000 potential patients are generated for the trial with 3 arm(s) <low dose, high dose, placebo>.
```

## Define Trial Milestones and Action Functions

Next, we define action functions for trial milestones, i.e., dose
selection, interim and final analysis. Note that an action function
should always has argument `trial` returned from
[`trial()`](https://zhangh12.github.io/TrialSimulator/reference/trial.md).
We calculate z statistics of the Farrington-Manning test for binary
endpoint `surrogate` using helper function `fitFarringtonManning`, which
returns a data frame (see
[`?fitFarringtonManning`](https://zhangh12.github.io/TrialSimulator/reference/fitFarringtonManning.md)).

``` r
action1 <- function(trial){
  
  locked_data <- trial$get_locked_data('dose selection')
  
  fit <- fitFarringtonManning(endpoint = 'surrogate', placebo = 'placebo',
                              data = locked_data, alternative = 'greater')
  
  # browser() ## if you want to see what does fit look like
  z_l <- fit$z[fit$arm == 'low dose']
  z_h <- fit$z[fit$arm == 'high dose']
  if(z_l > 1.28){
    trial$remove_arms('high dose')
    trial$save(value = 'low', name = 'kept_arm')
  }else if(z_h > 1.28){
    trial$remove_arms('low dose')
    trial$save(value = 'high', name = 'kept_arm')
  }else{
    trial$save(value = 'both', name = 'kept_arm')
  }

}
```

At the interim, we test `PFS` using the logrank test to carry out a
non-binding futility analysis in the following action function.
Depending on how many arms are carried forward in dose selection, `fit`
may consists of testing results of one or two dose arms. Note that a
formal test for `PFS` will be done at the end of the trial when we have
all p-values. If no futility analysis is planned, we can simply set
`action = doNothing` in `milestone`.

``` r
action2 <- function(trial){
  
  locked_data <- trial$get_locked_data('interim')

  fit <- fitLogrank(Surv(pfs, pfs_event) ~ arm, placebo = 'placebo', 
                    data = locked_data, alternative = 'less')

  ## futility analysis
  if(max(fit$z) < .5){
    trial$save(value = 'negative', name = 'futility')
    
    ## extend duration
    ## trial$set_duration(45)
  }else{
    trial$save(value = 'positive', name = 'futility')
  }

}
```

At final analysis, we conduct a closed test using Dunnett’s test for
intersection hypotheses. The action function is as follows.

``` r
action3 <- function(trial){
  
  locked_data <- trial$get_locked_data('final')

  ## test PFS
  dt_pfs <- trial$dunnettTest(Surv(pfs, pfs_event) ~ arm, placebo = 'placebo',
                              treatments = c('high dose', 'low dose'),
                              milestones = c('dose selection', 'interim', 'final'),
                              alternative = 'less',
                              planned_info = 'default')
  
  ct_pfs <- trial$closedTest(dt_pfs, treatments = c('high dose', 'low dose'),
                             milestones = c('interim', 'final'),
                             alpha = .005, alpha_spending = 'asOF')

  ## test OS
  dt_os <- trial$dunnettTest(Surv(os, os_event) ~ arm, placebo = 'placebo',
                             treatments = c('high dose', 'low dose'),
                             milestones = c('dose selection', 'final'),
                             alternative = 'less', 
                             planned_info = 'default')
  
  ct_os <- trial$closedTest(dt_os, treatments = c('high dose', 'low dose'),
                            milestones = c('final'),
                            alpha = .02, alpha_spending = 'asOF')
  
  ## we only save testing decision here
  ## You can save whatever you want for summarizing things later, e.g. reject time
  trial$save(value = ct_pfs$decision[ct_pfs$arm == 'high dose'], 
             name = 'pfs_high_dose_decision')
  
  trial$save(value = ct_pfs$decision[ct_pfs$arm == 'low dose'], 
             name = 'pfs_low_dose_decision')
  
  trial$save(value = ct_os$decision[ct_os$arm == 'high dose'], 
             name = 'os_high_dose_decision')
  
  trial$save(value = ct_os$decision[ct_os$arm == 'low dose'], 
             name = 'os_low_dose_decision')
}
```

Next, we register three trial milestones to a listener

``` r
dose_selection <- milestone(name = 'dose selection', action = action1,   
                            when = eventNumber(endpoint = 'surrogate', n = 300)
                            )

interim <- milestone(name = 'interim', action = action2, 
                     when = eventNumber(endpoint = 'pfs', n = 300)
                     )

final <- milestone(name = 'final', action = action3, 
                   when = enrollment(n = 1000, arms = c('placebo', 'low dose', 'high dose')) & 
                     eventNumber(endpoint = 'os', n = 300) & (
                       calendarTime(time = 28) | 
                         eventNumber(endpoint = 'pfs', n = 520)
                       )
                   )

listener <- listener()
#' register milestones with listener
listener$add_milestones(
  dose_selection,
  interim,
  final
)
#> A milestone <dose selection> is registered.
#> A milestone <interim> is registered.
#> A milestone <final> is registered.
```

## Execute a Trial

We can run a trial as follows. By default, `TrialSimulator` generates a
plot showing cumulative number of patients or endpoint events over time
for each of the arms. We can set it to `FALSE` if a massive number of
simulation replicates is run to save time.

``` r
controller <- controller(trial, listener)
controller$run(plot_event = TRUE)
#> Condition of milestone <dose selection> is being checked.
#> Data is locked at time = 11.1205128205128 for milestone <dose selection>.
#> Locked data can be accessed in Trial$get_locked_data('dose selection'). 
#> Number of events at lock time:
#>   patient pfs os surrogate         arms
#> 1     357 141 65       300 c(119, 4....
#> 
#> Arm <high dose> is removed.
#> Sample ratio is updated to be <low dose: 1, placebo: 1>.
#> Trial data is rolling back to time = 11.1205128205128. 
#> Randomization will be carried out again for unenrolled patients.
#> Randomization is done for 643 potential patients.
#> Data of 643 potential patients are generated for the trial with 2 arm(s) <low dose, placebo>.
#> Condition of milestone <interim> is being checked.
#> Data is locked at time = 17.9949397642942 for milestone <interim>.
#> Locked data can be accessed in Trial$get_locked_data('interim'). 
#> Number of events at lock time:
#>   patient pfs  os surrogate         arms
#> 1     581 300 138       524 c(290, 1....
#> 
#> Condition of milestone <final> is being checked.
#> Data is locked at time = 25.7475563250256 for milestone <final>.
#> Locked data can be accessed in Trial$get_locked_data('final'). 
#> Number of events at lock time:
#>   patient pfs  os surrogate         arms
#> 1     881 596 300       880 c(441, 2....
#> 
#> Ignoring unknown labels:
#> • colour : ""
```

![](adaptiveDesign_files/figure-html/eiaaf-1.png)

In custom action functions, we can use `Trial$save()` to save
intermediate results for summary purpose, which can be accessed anytime
and anywhere by calling `get_output()`

``` r
controller$get_output() %>% 
  kable(escape = FALSE) %>% 
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE,
                position = "left") %>%
  scroll_box(width = "100%")
```

| trial      |       seed | milestone_time\_\<dose selection\> | n_events\_\<dose selection\>\_\<patient_id\> | n_events\_\<dose selection\>\_\<pfs\> | n_events\_\<dose selection\>\_\<os\> | n_events\_\<dose selection\>\_\<surrogate\> | n_events\_\<dose selection\>\_\<arms\> | kept_arm | milestone_time\_\<interim\> | n_events\_\<interim\>\_\<patient_id\> | n_events\_\<interim\>\_\<pfs\> | n_events\_\<interim\>\_\<os\> | n_events\_\<interim\>\_\<surrogate\> | n_events\_\<interim\>\_\<arms\> | futility | milestone_time\_\<final\> | n_events\_\<final\>\_\<patient_id\> | n_events\_\<final\>\_\<pfs\> | n_events\_\<final\>\_\<os\> | n_events\_\<final\>\_\<surrogate\> | n_events\_\<final\>\_\<arms\> | pfs_high_dose_decision | pfs_low_dose_decision | os_high_dose_decision | os_low_dose_decision | error_message |
|:-----------|-----------:|-----------------------------------:|---------------------------------------------:|--------------------------------------:|-------------------------------------:|--------------------------------------------:|:---------------------------------------|:---------|----------------------------:|--------------------------------------:|-------------------------------:|------------------------------:|-------------------------------------:|:--------------------------------|:---------|--------------------------:|------------------------------------:|-----------------------------:|----------------------------:|-----------------------------------:|:------------------------------|:-----------------------|:----------------------|:----------------------|:---------------------|:--------------|
| Trial-3415 | 1727811904 |                           11.12051 |                                          357 |                                   141 |                                   65 |                                         300 | c(119, 4….                             | low      |                    17.99494 |                                   581 |                            300 |                           138 |                                  524 | c(290, 1….                      | negative |                  25.74756 |                                 881 |                          596 |                         300 |                                880 | c(441, 2….                    | accept                 | reject                | accept                | accept               |               |

Here we dive into the action function (`action3`) for the final
analysis. We can literally execute the function line by line with locked
data loaded. Note that the member functions `Trial$dunnettTest` and
`Trial$closedTest` can make use of locked data at multiple stages
(through the `milestones` argument) automatically, thus, unlike in
`action3`, an explicit call of `Trial$get_locked_data('final')` is
unnecessary.

``` r
## test PFS
dt_pfs <- trial$dunnettTest(Surv(pfs, pfs_event) ~ arm, placebo = 'placebo',
                            treatments = c('high dose', 'low dose'),
                            milestones = c('dose selection', 'interim', 'final'),
                            alternative = 'less', 
                            planned_info = 'default')
ct_pfs <- trial$closedTest(dt_pfs, treatments = c('high dose', 'low dose'),
                           milestones = c('interim', 'final'),
                           alpha = .005, alpha_spending = 'asOF')

## test OS
dt_os <- trial$dunnettTest(Surv(os, os_event) ~ arm, placebo = 'placebo',
                           treatments = c('high dose', 'low dose'),
                           milestones = c('dose selection', 'final'),
                           alternative = 'less', 
                           planned_info = 'default')
ct_os <- trial$closedTest(dt_os, treatments = c('high dose', 'low dose'),
                          milestones = c('final'),
                          alpha = .02, alpha_spending = 'asOF')

print(ct_pfs)
#>         arm decision milestone_at_reject reject_time
#> 1 high dose   accept                <NA>         Inf
#> 2  low dose   reject               final    25.74756
print(ct_os)
#>         arm decision milestone_at_reject reject_time
#> 1 high dose   accept                  NA         Inf
#> 2  low dose   accept                  NA         Inf
```

The two null hypotheses of `PFS` and `OS` are both accepted for the high
dose because it is dropped at dose selection. Thus,
`milestone_at_reject` is `NA`, and the `reject_time` is infinite. In
contrast, `PFS` and `OS` are significant in low does. Note that even if
`PFS` are tested at interim and final, one cannot claim its significance
until the final analysis (`milestone_at_reject`). The `reject_time` ()
can be saved to the output using the member function `Trial$save`.

`TrialSimulator` abstracts the data generation and management to allow
user focus on implementing the statistical analysis. It simulates a
trial on patient level, and provide flexibility in adaptive design.

## Execute Trial Simulation

We can run a massive number of replicates in simulation to study
operating characteristics of a trial design by specifying `n` in
`Controller$run()`. Parallelization is also supported by setting
`n_workers`.

``` r
## reset a controller if $run has been executed before
controller$reset()
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

| trial      |       seed | milestone_time\_\<dose selection\> | n_events\_\<dose selection\>\_\<patient_id\> | n_events\_\<dose selection\>\_\<pfs\> | n_events\_\<dose selection\>\_\<os\> | n_events\_\<dose selection\>\_\<surrogate\> | n_events\_\<dose selection\>\_\<arms\> | kept_arm | milestone_time\_\<interim\> | n_events\_\<interim\>\_\<patient_id\> | n_events\_\<interim\>\_\<pfs\> | n_events\_\<interim\>\_\<os\> | n_events\_\<interim\>\_\<surrogate\> | n_events\_\<interim\>\_\<arms\> | futility | milestone_time\_\<final\> | n_events\_\<final\>\_\<patient_id\> | n_events\_\<final\>\_\<pfs\> | n_events\_\<final\>\_\<os\> | n_events\_\<final\>\_\<surrogate\> | n_events\_\<final\>\_\<arms\> | pfs_high_dose_decision | pfs_low_dose_decision | os_high_dose_decision | os_low_dose_decision | error_message |
|:-----------|-----------:|-----------------------------------:|---------------------------------------------:|--------------------------------------:|-------------------------------------:|--------------------------------------------:|:---------------------------------------|:---------|----------------------------:|--------------------------------------:|-------------------------------:|------------------------------:|-------------------------------------:|:--------------------------------|:---------|--------------------------:|------------------------------------:|-----------------------------:|----------------------------:|-----------------------------------:|:------------------------------|:-----------------------|:----------------------|:----------------------|:---------------------|:--------------|
| Trial-3415 | 1727811904 |                           11.31385 |                                          366 |                                   142 |                                   65 |                                         300 | c(122, 4….                             | low      |                    19.16584 |                                   637 |                            300 |                           146 |                                  534 | c(319, 1….                      | negative |                  29.44763 |                                 878 |                          542 |                         300 |                                744 | c(439, 2….                    | accept                 | reject                | accept                | reject               |               |
| Trial-3415 |  776792356 |                           11.33385 |                                          367 |                                   139 |                                   68 |                                         300 | c(123, 4….                             | high     |                    19.52434 |                                   654 |                            300 |                           145 |                                  544 | c(327, 1….                      | negative |                  29.56296 |                                 877 |                          520 |                         300 |                                748 | c(439, 2….                    | reject                 | accept                | reject                | accept               |               |
| Trial-3415 | 1430198130 |                           11.41385 |                                          371 |                                   138 |                                   64 |                                         300 | c(124, 4….                             | low      |                    19.99458 |                                   677 |                            300 |                           158 |                                  554 | c(339, 1….                      | negative |                  30.26166 |                                 877 |                          546 |                         300 |                                740 | c(439, 2….                    | accept                 | reject                | accept                | reject               |               |
| Trial-3415 | 1400866922 |                           11.29385 |                                          365 |                                   154 |                                   65 |                                         300 | c(122, 4….                             | high     |                    19.48899 |                                   653 |                            300 |                           140 |                                  547 | c(326, 1….                      | negative |                  28.74682 |                                 878 |                          541 |                         300 |                                755 | c(439, 2….                    | reject                 | accept                | reject                | accept               |               |
| Trial-3415 | 1898009641 |                           11.19385 |                                          360 |                                   131 |                                   60 |                                         300 | c(120, 4….                             | low      |                    19.73958 |                                   667 |                            300 |                           151 |                                  550 | c(334, 1….                      | negative |                  28.64718 |                                 880 |                          534 |                         300 |                                743 | c(440, 2….                    | accept                 | reject                | accept                | reject               |               |

``` r
output %>% 
  summarise(
    time_dose_selection = mean(`milestone_time_<dose selection>`), 
    time_interim = mean(`milestone_time_<interim>`), 
    time_final = mean(`milestone_time_<final>`), 
    n_dose_selection = mean(`n_events_<dose selection>_<patient_id>`), 
    n_interim = mean(`n_events_<interim>_<patient_id>`), 
    n_final = mean(`n_events_<final>_<patient_id>`), 
    low = mean(kept_arm == 'low') * 100, 
    high = mean(kept_arm == 'high') * 100, 
    both = mean(kept_arm == 'both') * 100
  ) %>% 
  kable(col.names = NULL, digits = 1, align = 'r', 
        caption = 'Number of Randomized Patients at Stages') %>% 
  add_header_above(c(rep(c('Dose Selection', 'Interim', 'Final'), 2), 
                     'Low Dose', 'High Dose', 'Both'), align = 'r') %>% 
  add_header_above(c('Time' = 3, 'Number of Patients' = 3, 'Selected Dose (%)' = 3)) %>% 
  kable_styling(full_width = TRUE)
```

[TABLE]

Number of Randomized Patients at Stages

``` r
output %>% 
  summarise(
    n_pfs_dose_selection = mean(`n_events_<dose selection>_<pfs>`), 
    n_pfs_interim = mean(`n_events_<interim>_<pfs>`),
    n_pfs_final = mean(`n_events_<final>_<pfs>`),
    n_os_dose_selection = mean(`n_events_<dose selection>_<os>`), 
    n_os_interim = mean(`n_events_<interim>_<os>`),
    n_os_final = mean(`n_events_<final>_<os>`)
  ) %>% 
  kable(col.names = NULL, digits = 1, align = 'r', 
        caption = 'Number of Events of PFS and OS at Stages') %>% 
  add_header_above(rep(c('Dose Selection', 'Interim', 'Final'), 2), align = 'r') %>% 
  add_header_above(c('PFS' = 3, 'OS' = 3)) %>% 
  kable_styling(full_width = TRUE)
```

[TABLE]

Number of Events of PFS and OS at Stages

``` r
output %>% 
  summarise(
    power_pfs_low = mean(pfs_low_dose_decision == 'reject') * 100, 
    power_pfs_high = mean(pfs_high_dose_decision == 'reject') * 100, 
    power_pfs_or = mean(pfs_low_dose_decision == 'reject' | pfs_high_dose_decision == 'reject') * 100, 
    power_pfs_and = mean(pfs_low_dose_decision == 'reject' & pfs_high_dose_decision == 'reject') * 100, 
    power_os_low = mean(os_low_dose_decision == 'reject') * 100, 
    power_os_high = mean(os_high_dose_decision == 'reject') * 100, 
    power_os_or = mean(os_low_dose_decision == 'reject' | os_high_dose_decision == 'reject') * 100,
    power_os_and = mean(os_low_dose_decision == 'reject' & os_high_dose_decision == 'reject') * 100
  ) %>% 
  kable(col.names = NULL, digits = 1, align = 'r', 
        caption = 'Power of Testing PFS and OS') %>% 
  add_header_above(rep(c('Low Dose', 'High Dose', 'Low or High', 'Low and High'), 2), align = 'r') %>% 
  add_header_above(c('PFS (%)' = 4, 'OS (%)' = 4)) %>% 
  kable_styling(full_width = TRUE)
```

[TABLE]

Power of Testing PFS and OS

``` r
output %>% 
  summarise(
    power_pfs_not_os = mean((pfs_low_dose_decision == 'reject' | pfs_high_dose_decision == 'reject') & 
                              os_low_dose_decision == 'accept' & os_high_dose_decision == 'accept') * 100, 
    power_os_not_pfs = mean((os_low_dose_decision == 'reject' | os_high_dose_decision == 'reject') & 
                              pfs_low_dose_decision == 'accept' & pfs_high_dose_decision == 'accept') * 100, 
    power_pfs_and_os = mean((pfs_low_dose_decision == 'reject' | pfs_high_dose_decision == 'reject') & 
                              (os_low_dose_decision == 'reject' | os_high_dose_decision == 'reject')) * 100,
    power_pfs_or_os = mean((pfs_low_dose_decision == 'reject' | pfs_high_dose_decision == 'reject') | 
                              (os_low_dose_decision == 'reject' | os_high_dose_decision == 'reject')) * 100
  ) %>% 
  kable(col.names = NULL, digits = 1, align = 'r', 
        caption = 'Power of Testing PFS and OS (Cont.)') %>% 
  add_header_above(c('Reject PFS and Accept OS', 'Accept PFS and Reject OS', 
                     'Reject PFS and OS', 'Reject PFS or OS'), align = 'r') %>% 
  kable_styling(full_width = TRUE)
```

[TABLE]

Power of Testing PFS and OS (Cont.)
