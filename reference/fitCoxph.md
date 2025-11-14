# Fit Cox Proportional Hazard Ratio model

Fit Cox proportional hazards model on an time-to-event endpoint.

Refer to [this
vignette](https://zhangh12.github.io/TrialSimulator/articles/wrappers.html)
for more information and examples.

## Usage

``` r
fitCoxph(formula, placebo, data, alternative, scale, ..., tidy = TRUE)
```

## Arguments

- formula:

  An object of class `formula` that can be used with
  [`survival::coxph`](https://rdrr.io/pkg/survival/man/coxph.html). The
  data frame `data` must consist a column `arm` and a column of the
  endpoint specified in `formula`. Covariates can be adjusted.
  Interactions between `arm` and covariates are allowed in `formula`,
  but `arm` must has a term of main effect, and only estimate of that
  main effect is tested.

- placebo:

  Character. String indicating the placebo in `data$arm`.

- data:

  Data frame. Usually it is a data snapshot locked at a milestone.

- alternative:

  a character string specifying the alternative hypothesis, must be one
  of `"greater"` or `"less"`, i.e., one-sided test is enforced. No
  default value. `"greater"` means superiority of treatment over placebo
  is established by an hazard ratio greater than 1.

- scale:

  character. The type of estimate in the output. Must be one of
  `"log hazard ratio"` or `"hazard ratio"`. No default value.

- ...:

  (optional) subset conditions compatible with
  [`dplyr::filter`](https://dplyr.tidyverse.org/reference/filter.html).
  `coxph` will be fitted on this subset only. This argument can be
  useful to create a subset of data for analysis when a trial consists
  of more than two arms. By default, it is not specified, all data will
  be used to fit the model. More than one condition can be specified in
  `...`, e.g.,
  `fitCoxph(formula, 'pbo', data, 'less', 'log hazard ratio', arm %in% c('pbo', 'low dose'), x > 0.5)`,
  which is equivalent to:
  `fitCoxph(formula, 'pbo', data, 'less', 'log hazard ratio', arm %in% c('pbo', 'low dose') & x > 0.5)`.
  Note that if more than one treatment arm are present in the data after
  applying filter in `...`, models are fitted and tested for placebo
  verse each of the treatment arms.

- tidy:

  logical. `FALSE` if more information are returned. Default: `TRUE`.

## Value

a data frame with three columns:

- `arm`:

  name of the treatment arm.

- `placebo`:

  name of the placebo arm.

- `estimate`:

  estimate of main effect of arm, depending on `scale`.

- `p`:

  one-sided p-value for log hazard ratio (treated vs placebo).

- `info`:

  the number of events of the endpoint in the subset.

- `z`:

  the z statistics of log hazard ratios.
