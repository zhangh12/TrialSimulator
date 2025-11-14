# Wrapper Functions of Common Statistical Methods in TrialSimulator

The `TrialSimulator` package provides a unified set of wrapper functions
that encapsulate statistical methods commonly used in clinical trial
simulations. These functions facilitate model fitting, treatment
comparisons, and covariate adjustments within a standardized interface.

When multiple active treatment arms are present, each wrapper function
automatically performs pairwise comparisons between each active arm and
the designated reference (e.g., placebo, control, or standard-of-care).
All wrapper functions share a consistent syntax and output structure.
Most of them support model specification via an R formula interface, and
covariate adjustment is available where appropriate.

Pairwise average treatment effects (ATEs) are estimated using the
`emmeans` package under the hood. All tests are one-sided, and the
ellipsis (`...`) argument can be used to define data subsets, enabling
flexible analyses such as those needed in enrichment designs.

Below is a summary of the available wrapper functions included in this
vignette, along with their corresponding statistical methods, output
metrics, and support for covariate adjustment.

[TABLE]

## Example

To demonstrate the usage of the wrapper functions, we simulate a
hypothetical three-arm trial with one control (`pbo`) and two active
doses (`low` and `high`). The trial includes a continuous covariate `x`,
three endpoint types (time-to-event, continuous, and binary), and a
binary biomarker used to define subgroups.

The placebo arm is constructed as follows:

``` r
## time-to-event endpoint
pfs <- endpoint(name = 'pfs', type = 'tte', generator = rexp, rate = .07)
## continuous endpoint
cep <- endpoint(name = 'cep', type = 'non-tte', 
                readout = c(cep = 0), generator = rnorm)
## binary endpoint
bep <- endpoint(name = 'bep', type = 'non-tte', 
                readout = c(bep = 0), generator = rbinom, size = 1, prob = .1)

## biomarker
bm <- endpoint(name = 'biomarker', type = 'non-tte', 
               readout = c(biomarker = 0), generator = rbinom, 
               size = 1, prob = .7)

## covariate
covar <- endpoint(name = 'x', type = 'non-tte', 
                  readout = c(x = 0), generator = rnorm)

pbo <- arm(name = 'pbo')
pbo$add_endpoints(pfs, cep, bep, bm, covar)
```

For brevity, the code for the low and high dose arms and the trial
definition are hidden in this vignette. Refer to the source of this
vignette for full code. A single milestone for final analysis is
defined, with an empty action `doNothing.` This is because we will
explicitly request locked data outside of the action function when
demonstrating the wrapper functions.

    #> A milestone <final> is registered.

Now we execute the trial. After simulation, locked data can be retrieved
using the `get_locked_data()` method with the milestone name `"final"`.

``` r
controller$run(n = 1, plot_event = FALSE, silent = TRUE)
locked_data <- trial$get_locked_data('final')
head(locked_data)
#>   patient_id  arm enroll_time dropout_time       pfs pfs_event         cep
#> 1          1  pbo  0.00000000   322.817715  6.819525         1 -0.21028508
#> 2          2 high  0.03333333   408.104162 98.520770         1 -0.08806648
#> 3          3  low  0.06666667    45.651176 21.215875         1  2.12517509
#> 4          4  low  0.10000000   103.734510 27.046261         1  1.53260487
#> 5          5  pbo  0.13333333     8.685884  4.776078         1  0.71222847
#> 6          6 high  0.16666667    12.309588 12.309588         0  1.80510702
#>   cep_readout bep bep_readout biomarker biomarker_readout             x
#> 1           0   0           0         0                 0 -0.6829126592
#> 2           0   0           0         1                 0  2.2913063006
#> 3           0   1           0         1                 0  0.0001582446
#> 4           0   0           0         0                 0 -0.1193032317
#> 5           0   0           0         1                 0  1.5541601999
#> 6           0   1           0         1                 0  0.1176311038
#>   x_readout
#> 1         0
#> 2         0
#> 3         0
#> 4         0
#> 5         0
#> 6         0

table(locked_data$arm)
#> 
#> high  low  pbo 
#>  100  100  100
```

## Analyze Time-to-Event Endpoint

We begin by analyzing the time-to-event endpoint `pfs` using both a Cox
proportional hazards model and a log-rank test. When performing analysis
on a subset defined via the `...` argument, the syntax must be
compatible with that of
[`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html).

``` r
## adjust for covariate x
fitCoxph(Surv(pfs, pfs_event) ~ arm + x, placebo = 'pbo', 
         data = locked_data, alternative = 'less', 
         scale = 'hazard ratio')
#>    arm placebo  estimate            p info         z
#> 1 high     pbo 0.4212846 7.451797e-08  171 -5.253747
#> 2  low     pbo 0.7270550 1.826582e-02  178 -2.090960

fitLogrank(Surv(pfs, pfs_event) ~ arm, placebo = 'pbo', 
           data = locked_data, alternative = 'less')
#>    arm placebo            p info         z
#> 1 high     pbo 4.327270e-08  171 -5.352922
#> 2  low     pbo 1.945612e-02  178 -2.065114

## more details
fitLogrank(Surv(pfs, pfs_event) ~ arm, placebo = 'pbo', 
           data = locked_data, alternative = 'less', tidy = FALSE)
#>    arm placebo            p info         z info_pbo info_trt n_pbo n_trt
#> 1 high     pbo 4.327270e-08  171 -5.352922       88       83   100   100
#> 2  low     pbo 1.945612e-02  178 -2.065114       88       90   100   100

## with strata
fitLogrank(Surv(pfs, pfs_event) ~ arm + strata(biomarker), placebo = 'pbo', 
           data = locked_data, alternative = 'less')
#>    arm placebo            p info         z
#> 1 high     pbo 1.118371e-07  171 -5.178502
#> 2  low     pbo 1.618737e-02  178 -2.139753

## analyze a subset
fitCoxph(Surv(pfs, pfs_event) ~ arm + strata(biomarker), placebo = 'pbo', 
         data = locked_data, alternative = 'less', 
         scale = 'log hazard ratio', 
         x > -2 & x < 3) ## define a subset
#>    arm placebo   estimate            p info         z
#> 1 high     pbo -0.8573491 1.459637e-07  167 -5.128585
#> 2  low     pbo -0.3321455 1.494187e-02  178 -2.171628
```

## Analyze Continuous Endpoint

We analyze the continuous endpoint `cep` using linear models, with and
without covariate adjustment.

``` r
## ATE accounting for covariate x
fitLinear(cep ~ arm * x, placebo = 'pbo', 
          data = locked_data, alternative = 'greater')
#> NOTE: Results may be misleading due to involvement in interactions
#> NOTE: Results may be misleading due to involvement in interactions
#>    arm placebo estimate            p info        z
#> 1 high     pbo 1.145315 1.487699e-14  200 8.206717
#> 2  low     pbo 1.260325 0.000000e+00  200 9.385532

## marginal model
fitLinear(cep ~ arm, placebo = 'pbo', 
          data = locked_data, alternative = 'greater')
#>    arm placebo estimate            p info        z
#> 1 high     pbo 1.141955 1.287859e-14  200 8.223983
#> 2  low     pbo 1.246252 0.000000e+00  200 9.329409

## analyze a sub-group
fitLinear(cep ~ arm, placebo = 'pbo', 
          data = locked_data, alternative = 'greater', 
          biomarker == 1) ## define the subgroup
#>    arm placebo estimate            p info        z
#> 1 high     pbo 1.233081 1.240319e-11  142 7.257133
#> 2  low     pbo 1.253416 1.585532e-11  133 7.253694
```

## Analyze Binary Endpoint

We analyze the binary endpoint `bep` using logistic regression. Multiple
estimands (e.g., odds ratio, risk ratio, risk difference) can be
computed by specifying the `scale` argument.

``` r
## compute regression coefficient of arm
fitLogistic(bep ~ arm * x + biomarker, placebo = 'pbo', 
            data = locked_data, alternative = 'greater', 
            scale = 'coefficient')
#>    arm placebo  estimate            p info         z
#> 1 high     pbo 1.5617130 2.607897e-05  200 4.0457407
#> 2  low     pbo 0.4052616 1.706828e-01  200 0.9514706

## compute odds ratio (ATE)
fitLogistic(bep ~ arm + x*biomarker, placebo = 'pbo', 
            data = locked_data, alternative = 'greater', 
            scale = 'odds ratio')
#>    arm placebo estimate            p info         z
#> 1 high     pbo 4.519236 2.494409e-05  200 4.0561502
#> 2  low     pbo 1.434263 1.924932e-01  200 0.8687454

## compute risk ratio (ATE)
fitLogistic(bep ~ arm + x + biomarker, placebo = 'pbo', 
            data = locked_data, alternative = 'greater', 
            scale = 'risk ratio')
#>    arm placebo estimate            p info         z
#> 1 high     pbo 3.208045 5.888748e-05  200 3.8507119
#> 2  low     pbo 1.371446 1.894037e-01  200 0.8800958
```

The risk difference can also be estimated using logistic regression or
the Farrington-Manning test. Note that the latter does not support
covariate adjustment.

``` r
## compute risk difference (ATE)
fitLogistic(bep ~ arm + x * biomarker, placebo = 'pbo', 
            data = locked_data, alternative = 'greater', 
            scale = 'risk difference')
#>    arm placebo   estimate            p info         z
#> 1 high     pbo 0.25622322 7.567886e-06  200 4.3267032
#> 2  low     pbo 0.04246899 1.909160e-01  200 0.8745257

## compute risk difference without covariate
fitLogistic(bep ~ arm, placebo = 'pbo', 
            data = locked_data, alternative = 'greater', 
            scale = 'risk difference')
#>    arm placebo estimate            p info         z
#> 1 high     pbo     0.26 4.271057e-06  200 4.4511262
#> 2  low     pbo     0.04 2.071073e-01  200 0.8164994

## analyze a sub-group
fitLogistic(bep ~ arm, placebo = 'pbo', 
            data = locked_data, alternative = 'greater', 
            scale = 'risk difference', 
            x < 2 & biomarker != 1) ## define a subgroup
#>    arm placebo   estimate           p info         z
#> 1 high     pbo 0.27359617 0.005291046   58 2.5562045
#> 2  low     pbo 0.07465438 0.184003001   66 0.9002147

## analyze the same sub-group using the FM test,
## same estimate but different p-values
fitFarringtonManning(endpoint = 'bep', placebo = 'pbo', 
                     data = locked_data, alternative = 'greater', 
                     x < 2 & biomarker != 1)
#>    arm placebo   estimate           p info         z
#> 1 high     pbo 0.27359618 0.006345068   58 2.4923488
#> 2  low     pbo 0.07465438 0.188880249   66 0.8820302
```
