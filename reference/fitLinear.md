# Fit linear regression model

Fit linear regression model on a continuous endpoint.

Refer to [this
vignette](https://zhangh12.github.io/TrialSimulator/articles/wrappers.html)
for more information and examples.

## Usage

``` r
fitLinear(formula, placebo, data, alternative, ...)
```

## Arguments

- formula:

  an object of class `formula`. Must include `arm` and endpoint in
  `data`. Covariates can be adjusted.

- placebo:

  Character. String indicating the placebo arm in `data$arm`.

- data:

  Data frame. Usually it is a locked data set.

- alternative:

  a character string specifying the alternative hypothesis, must be one
  of `"greater"` or `"less"`, i.e., one-sided test is enforced. No
  default value. `"greater"` means superiority of treatment over placebo
  is established by a greater mean in treated arm.

- ...:

  Subset conditions compatible with
  [`dplyr::filter`](https://dplyr.tidyverse.org/reference/filter.html).
  `glm` will be fitted on this subset only. This argument can be useful
  to create a subset of data for analysis when a trial consists of more
  than two arms. By default, it is not specified, all data will be used
  to fit the model. More than one condition can be specified in `...`,
  e.g.,
  `fitLinear(cfb ~ arm, 'pbo', data, 'greater', arm %in% c('pbo', 'low dose'), cfb > 0.5)`,
  which is equivalent to:
  `fitLinear(cfb ~ arm, 'pbo', data, 'greater', arm %in% c('pbo', 'low dose') & cfb > 0.5)`.
  Note that if more than one treatment arm are present in the data after
  applying filter in `...`, models are fitted and tested for placebo
  verse each of the treatment arms.

## Value

a data frame with columns:

- `arm`:

  name of the treatment arm.

- `placebo`:

  name of the placebo arm.

- `estimate`:

  estimate of average treatment effect of `arm`.

- `p`:

  one-sided p-value for between-arm difference (treated vs placebo).

- `info`:

  sample size used in model with `NA` being removed.

- `z`:

  z statistics of between-arm difference (treated vs placebo).
