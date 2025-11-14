# Solve Parameters in a Three-State Ill-Death Model

The ill-death model consists of three states, `initial`, `progression`,
and `death`. It can be used to model the progression-free survival (PFS)
and overall survival (OS) in clinical trial simulation. It models the
correlation PFS and OS without assumptions on latent status and copula.
Also, it does not assume PFS and OS satisfy the proportional hazard
assumption simultaneously. The three-state ill-death model ensures a
nice property that PFS \<= OS with probability one. However, it requires
three hazard parameters under the homogeneous Markov assumption. In
practice, hazard parameters are hard to specify intuitively especially
when no trial data is available at the planning stage.

This function reparametrizes the ill-death model in term of three
parameters, i.e. median of PFS, median of OS, and correlation between
PFS and OS. The output of this function, which consists of the three
hazard parameters, can be used to generate PFS and OS with desired
property. It can be used with the built-in data generator
[`CorrelatedPfsAndOs3()`](https://zhangh12.github.io/TrialSimulator/reference/CorrelatedPfsAndOs3.md)
when defining endpoints in `TrialSimulator`.

For more information, refer to [this
vignette](https://zhangh12.github.io/TrialSimulator/articles/simulatePfsAndOs.html).

## Usage

``` r
solveThreeStateModel(
  median_pfs,
  median_os,
  corr,
  h12 = seq(0.05, 0.2, length.out = 50)
)
```

## Arguments

- median_pfs:

  numeric. Median of PFS.

- median_os:

  numeric. Median of OS.

- corr:

  numeric vector. Pearson correlation coefficients between PFS and OS.

- h12:

  numeric vector. A set of hazard from progression to death that may
  induce the target correlation `corr` given `median_pfs` and
  `median_os`. `solveThreeStateModel()` will do a grid search to find
  the best hazard parameters that matches to the medians of PFS and OS,
  and their correlations.

## Value

a data frame with columns:

- `corr`:

  target Peason's correlation coefficients.

- `h01`:

  hazard from stable to progression.

- `h02`:

  hazard from stable to death.

- `h12`:

  hazard from progression to death.

- `error`:

  absolute error between target correlation and correlation derived from
  `h01`, `h02`, and `h12`.

## Examples

``` r
dat <- CorrelatedPfsAndOs3(1e6, h01 = .1, h02 = .05, h12 = .12)

cor(dat$pfs, dat$os) ## 0.65
#> [1] 0.6467199

median(dat$pfs) ## 4.62
#> [1] 4.624702

median(dat$os) ## 9.61
#> [1] 9.601951

## find h01, h02, h12 that can match to median_pfs, median_os and corr
## should be close to h01 = 0.10, h02 = 0.05, h12 = 0.12 when corr = 0.65
ret <- solveThreeStateModel(median_pfs = 4.6, median_os = 9.6,
                            corr = seq(.5, .7, length.out=5))
ret
#>   corr        h01        h02        h12        error
#> 1 0.50 0.08077106 0.06991311 0.07755102 0.0019725523
#> 2 0.55 0.08619848 0.06448569 0.08979592 0.0008271392
#> 3 0.60 0.09334966 0.05733451 0.10510204 0.0027808278
#> 4 0.65 0.10091413 0.04977004 0.12040816 0.0055098914
#> 5 0.70 0.11388735 0.03679682 0.14489796 0.0002066345
```
