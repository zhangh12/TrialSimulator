# Generate Correlated PFS, OS and Objective Response

Generate correlated PFS, OS and objective response using the four-states
model. It can be used as custom generator of
[`endpoint()`](https://zhangh12.github.io/TrialSimulator/reference/endpoint.md).

## Usage

``` r
CorrelatedPfsAndOs4(
  n,
  transition_probability,
  duration,
  death_name = "death",
  progression_name = "progression",
  response_name = "response"
)
```

## Arguments

- n:

  integer. Number of observations.

- transition_probability:

  a 4x4 matrix defining transition probabilities between stable (initial
  state, 1), response (2), progression (3) and death (absorbing, 4).

- duration:

  integer. Duration of trial. Set it to a sufficient large integer in
  practice to cover the duration of the trial (potentially be extended).

- death_name:

  column name of OS in returned data frame. It must be consistent with
  `name` in the function
  [`endpoint()`](https://zhangh12.github.io/TrialSimulator/reference/endpoint.md).

- progression_name:

  column name of PFS in returned data frame. It must be consistent with
  `name` in the function
  [`endpoint()`](https://zhangh12.github.io/TrialSimulator/reference/endpoint.md).

- response_name:

  column name of objective response in returned data frame. It must be
  consistent with `name` in the function
  [`endpoint()`](https://zhangh12.github.io/TrialSimulator/reference/endpoint.md).

## Value

A data frame of `n` rows and 6 columns (response, progression, death,
and their event indicators with 1 means event and 0 means censored at
duration). The column names are `<death_name>`, `<death_name>_event`,
`<progression_name>`, `<progression_name>_event`, `<response_name>` and
`<response_name>_event`.

Note that it returns time-to-response for each patients with status of
censoring at pre-set duration. If a binary indicator of response at a
time point is needed as an endpoint, we may write a wrapper function to
convert the column `<response_name>` to binary and remove the column
`<response_name>_event` from return value.

## Examples

``` r
m <- matrix(c(0.99, 0.0035, 0.0055, 0.0010,
                 0, 0.9900, 0.0052, 0.0048,
                 0,      0, 0.9960, 0.0040,
                 0,      0,      0,      1),
             nrow = 4, byrow = TRUE)

## use as function (if you don't use TrialSimulator for simulation)

dat <- CorrelatedPfsAndOs4(1e4, m, 365 * 3)

## use as generator (if you use TrialSimulator for simulation)

ep <- endpoint(name = c('pfs', 'os', 'or'),
               type = c('tte', 'tte', 'tte'), ## OR is TTE, not binary
               generator = CorrelatedPfsAndOs4,
               transition_probability = m,
               duration = 365 * 3,
               death_name = 'os', ## rename output from generator to match with "name"
               progression_name = 'pfs',
               response_name = 'or')

ep # run it in console to see summary report
#> Summary generated.
```
