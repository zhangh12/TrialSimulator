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
#>        patient_id  arm enroll_time dropout_time       pfs pfs_event        cep
#> pbo.1           1  pbo  0.00000000     8.015946  8.015946         0 -0.9347262
#> high.1          2 high  0.03333333   181.907651 59.719535         1  2.0994964
#> low.1           3  low  0.06666667   208.113743  2.093918         1  1.0685059
#> low.2           4  low  0.10000000   183.582186 19.297349         1  1.1374167
#> pbo.2           5  pbo  0.13333333    93.209441  2.798993         1 -1.0593631
#> high.2          6 high  0.16666667   342.746964  1.801312         1  1.5301519
#>        cep_readout bep bep_readout biomarker biomarker_readout          x
#> pbo.1            0   0           0         1                 0 -1.5830337
#> high.1           0   1           0         0                 0 -1.5856812
#> low.1            0   0           0         1                 0 -0.4026810
#> low.2            0   1           0         0                 0  1.5182975
#> pbo.2            0   1           0         1                 0  0.9951762
#> high.2           0   1           0         0                 0  0.1088044
#>        x_readout
#> pbo.1          0
#> high.1         0
#> low.1          0
#> low.2          0
#> pbo.2          0
#> high.2         0

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
#> 1 high     pbo 0.5446845 4.871544e-05  183 -3.896902
#> 2  low     pbo 0.7423389 2.289992e-02  184 -1.997233

fitLogrank(Surv(pfs, pfs_event) ~ arm, placebo = 'pbo', 
           data = locked_data, alternative = 'less')
#>    arm placebo            p info         z
#> 1 high     pbo 3.965163e-05  183 -3.946496
#> 2  low     pbo 2.314752e-02  184 -1.992693

## more details
fitLogrank(Surv(pfs, pfs_event) ~ arm, placebo = 'pbo', 
           data = locked_data, alternative = 'less', tidy = FALSE)
#>    arm placebo            p info         z info_pbo info_trt n_pbo n_trt
#> 1 high     pbo 3.965163e-05  183 -3.946496       94       89   100   100
#> 2  low     pbo 2.314752e-02  184 -1.992693       94       90   100   100

## with strata
fitLogrank(Surv(pfs, pfs_event) ~ arm + strata(biomarker), placebo = 'pbo', 
           data = locked_data, alternative = 'less')
#>    arm placebo           p info         z
#> 1 high     pbo 3.86834e-05  183 -3.952414
#> 2  low     pbo 2.27789e-02  184 -1.999467

## analyze a subset
fitCoxph(Surv(pfs, pfs_event) ~ arm + strata(biomarker), placebo = 'pbo', 
         data = locked_data, alternative = 'less', 
         scale = 'log hazard ratio', 
         x > -2 & x < 3) ## define a subset
#>    arm placebo   estimate            p info         z
#> 1 high     pbo -0.6389944 3.371561e-05  177 -3.985172
#> 2  low     pbo -0.3427958 1.359893e-02  177 -2.208666
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
#>    arm placebo estimate           p info         z
#> 1 high     pbo 1.099769 1.34337e-13  200  7.847892
#> 2  low     pbo 1.372198 0.00000e+00  200 10.453246

## marginal model
fitLinear(cep ~ arm, placebo = 'pbo', 
          data = locked_data, alternative = 'greater')
#>    arm placebo estimate            p info         z
#> 1 high     pbo 1.107296 7.882583e-14  200  7.929746
#> 2  low     pbo 1.380094 0.000000e+00  200 10.557089

## analyze a sub-group
fitLinear(cep ~ arm, placebo = 'pbo', 
          data = locked_data, alternative = 'greater', 
          biomarker == 1) ## define the subgroup
#>    arm placebo estimate            p info        z
#> 1 high     pbo 1.054402 1.458040e-08  131 5.906693
#> 2  low     pbo 1.275959 1.775913e-12  136 7.647631
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
#>    arm placebo  estimate            p info        z
#> 1 high     pbo 1.5890262 8.749865e-05  200 3.752618
#> 2  low     pbo 0.9166145 2.042494e-02  200 2.045051

## compute odds ratio (ATE)
fitLogistic(bep ~ arm + x*biomarker, placebo = 'pbo', 
            data = locked_data, alternative = 'greater', 
            scale = 'odds ratio')
#>    arm placebo estimate            p info        z
#> 1 high     pbo 4.831764 5.122154e-05  200 3.884731
#> 2  low     pbo 2.301630 2.284767e-02  200 1.998197

## compute risk ratio (ATE)
fitLogistic(bep ~ arm + x + biomarker, placebo = 'pbo', 
            data = locked_data, alternative = 'greater', 
            scale = 'risk ratio')
#>    arm placebo estimate            p info        z
#> 1 high     pbo 3.319826 0.0001568282  200 3.603752
#> 2  low     pbo 2.049590 0.0226392707  200 2.002058
```

The risk difference can also be estimated using logistic regression or
the Farrington-Manning test. Note that the latter does not support
covariate adjustment.

``` r
## compute risk difference (ATE)
fitLogistic(bep ~ arm + x * biomarker, placebo = 'pbo', 
            data = locked_data, alternative = 'greater', 
            scale = 'risk difference')
#>    arm placebo  estimate            p info        z
#> 1 high     pbo 0.2377685 1.236247e-05  200 4.217296
#> 2  low     pbo 0.1048956 2.106819e-02  200 2.032171

## compute risk difference without covariate
fitLogistic(bep ~ arm, placebo = 'pbo', 
            data = locked_data, alternative = 'greater', 
            scale = 'risk difference')
#>    arm placebo estimate            p info        z
#> 1 high     pbo     0.23 1.864081e-05  200 4.123711
#> 2  low     pbo     0.11 1.483177e-02  200 2.174554

## analyze a sub-group
fitLogistic(bep ~ arm, placebo = 'pbo', 
            data = locked_data, alternative = 'greater', 
            scale = 'risk difference', 
            x < 2 & biomarker != 1) ## define a subgroup
#>    arm placebo  estimate          p info         z
#> 1 high     pbo 0.2083333 0.01669696   68 2.1273151
#> 2  low     pbo 0.0750000 0.21135590   62 0.8017254

## analyze the same sub-group using the FM test,
## same estimate but different p-values
fitFarringtonManning(endpoint = 'bep', placebo = 'pbo', 
                     data = locked_data, alternative = 'greater', 
                     x < 2 & biomarker != 1)
#>    arm placebo  estimate          p info         z
#> 1 high     pbo 0.2083333 0.02161304   68 2.0215189
#> 2  low     pbo 0.0750000 0.21116074   62 0.8024002
```
