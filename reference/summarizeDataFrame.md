# Summarize A Data Frame

A minimum alternative to `summarytools::dfSummary` to avoid package
dependency. This function is used to generate summary reports of
endpoints and arms. No meant to be used by end users. However, users may
find it helpful in their own applications if the interface is okay with
them.

## Usage

``` r
summarizeDataFrame(
  data,
  exclude_vars = NULL,
  tte_vars = NULL,
  event_vars = NULL,
  categorical_vars = NULL,
  title = "Summary",
  sub_title = ""
)
```

## Arguments

- data:

  a data frame.

- exclude_vars:

  columns to be excluded from summary.

- tte_vars:

  character. Vector of time-to-event variables.

- event_vars:

  character. Vector of event indicators. Every time-to-event variable
  should be corresponding to an event indicator.

- categorical_vars:

  character. Vector of categorical variables. This can be used to
  specify variables with limited distinct values as categorical
  variables in summary.

- title:

  character. Title of the summary report.

- sub_title:

  character. Sub-title.

## Value

a data frame of summary

## Examples

``` r
set.seed(123)

n <- 1000
data <- data.frame(
  age = rnorm(n, 65, 10),
  gender = sample(c('M', 'F', NA), n, replace = TRUE, prob = c(.4, .4, .2)),
  time_to_death = rexp(n, .01),
  death = rbinom(n, 1, .6),
  type = sample(LETTERS[1:8], n, replace = TRUE)
)

summarizeDataFrame(data, tte_vars = 'time_to_death', event_vars = 'death')
#> Summary generated.
```
