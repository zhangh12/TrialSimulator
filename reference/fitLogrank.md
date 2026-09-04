# Carry out log rank test

Compute log rank test statistic on an endpoint.

Refer to [this
vignette](https://zhangh12.github.io/TrialSimulator/articles/wrappers.html)
for more information and examples.

## Usage

``` r
fitLogrank(formula, placebo, data, alternative, ..., tidy = TRUE)
```

## Arguments

- formula:

  An object of class `formula` that can be used with
  [`survival::survdiff`](https://rdrr.io/pkg/survival/man/survdiff.html).
  Must consist `arm` and endpoint in `data`. No covariate is allowed.
  Stratification variables are supported and can be added using
  `strata(...)`.

- placebo:

  character. String of placebo in `data$arm`.

- data:

  data frame. Usually it is a locked data.

- alternative:

  a character string specifying the alternative hypothesis, must be one
  of `"greater"` or `"less"`, i.e., one-sided test is enforced. No
  default value. `"greater"` means superiority of treatment over placebo
  is established by an hazard ratio greater than 1.

- ...:

  subset condition that is compatible with
  [`dplyr::filter`](https://dplyr.tidyverse.org/reference/filter.html).
  The log rank test
  ([`survival::survdiff`](https://rdrr.io/pkg/survival/man/survdiff.html))
  is carried out on this subset only. This argument could be useful to
  create a subset of data for analysis when a trial consists of more
  than two arms. By default it is not specified, all data will be used
  to fit the model. More than one conditions can be specified in `...`,
  e.g.,
  `fitLogrank(formula, data, arm %in% c('pbo', 'low dose'), x > 0.5)`,
  which is equivalent to
  `fitLogrank(formula, data, arm %in% c('pbo', 'low dose') & x > 0.5)`.
  Note that if more than one treatment arm are present in the data after
  applying filter in `...`, models are fitted for placebo verse each of
  the treatment arms.

- tidy:

  logical. `FALSE` if more information are returned. Default `TRUE`.

## Value

a data frame with columns:

- `arm`:

  name of the treatment arm.

- `placebo`:

  name of the placebo arm.

- `p`:

  one-sided p-value for log-rank test (treated vs placebo).

- `info`:

  the number of events of the endpoint in the subset.

- `z`:

  the z statistic of the log rank test, with the sign of the log hazard
  ratio of treatment vs placebo.

If the statistic is undefined because its variance is zero (e.g., no
informative event comparison in the subset), a simulation placeholder
`z = 0` with the corresponding `p = 0.5` is returned with a warning
rather than an error, so that a few such replicates in a large
simulation do not require error handling in action functions. The
placeholder carries no evidence either way and is not a standardized
normal statistic.
