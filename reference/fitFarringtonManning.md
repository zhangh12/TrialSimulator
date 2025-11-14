# Farrington-Manning test for rate difference

Test rate difference by comparing it to a pre-specified value using the
Farrington-Manning test.

Refer to [this
vignette](https://zhangh12.github.io/TrialSimulator/articles/wrappers.html)
for more information and examples.

## Usage

``` r
fitFarringtonManning(endpoint, placebo, data, alternative, ..., delta = 0)
```

## Arguments

- endpoint:

  Character. Name of the endpoint in `data`.

- placebo:

  Character. String indicating the placebo in `data$arm`.

- data:

  Data frame. Usually it is a locked data set.

- alternative:

  a character string specifying the alternative hypothesis, must be one
  of `"greater"` or `"less"`, i.e., one-sided test is enforced. No
  default value. `"greater"` means superiority of treatment over placebo
  is established by rate difference greater than \`delta\`.

- ...:

  Subset conditions compatible with
  [`dplyr::filter`](https://dplyr.tidyverse.org/reference/filter.html).
  `glm` will be fitted on this subset only. This argument can be useful
  to create a subset of data for analysis when a trial consists of more
  than two arms. By default, it is not specified, all data will be used
  to fit the model. More than one condition can be specified in `...`,
  e.g.,
  `fitFarringtonManning('remission', 'pbo', data, delta, arm %in% c('pbo', 'low dose'), cfb > 0.5)`,
  which is equivalent to:
  `fitFarringtonManning('remission', 'pbo', data, delta, arm %in% c('pbo', 'low dose') & cfb > 0.5)`.
  Note that if more than one treatment arm are present in the data after
  applying filter in `...`, models are fitted for placebo verse each of
  the treatment arms.

- delta:

  the rate difference between a treatment arm and placebo under the
  null. 0 by default.

## Value

a data frame with three columns:

- `arm`:

  name of the treatment arm.

- `placebo`:

  name of the placebo arm.

- `estimate`:

  estimate of rate difference.

- `p`:

  one-sided p-value for log odds ratio (treated vs placebo).

- `info`:

  sample size in the subset with `NA` being removed.

- `z`:

  the z statistics of log odds ratio (treated vs placebo).

## References

Farrington, Conor P., and Godfrey Manning. "Test statistics and sample
size formulae for comparative binomial trials with null hypothesis of
non-zero risk difference or non-unity relative risk." Statistics in
medicine 9.12 (1990): 1447-1454.
