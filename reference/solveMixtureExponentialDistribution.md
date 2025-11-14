# Solve Parameters in a Mixture Exponential Distribution

This is a helper function to explore parameters for endpoint generator,
likely in an enrichment design.

Assume that the overall population in an arm is a mixture of two
exponential distributions with medians `median1` (\\m_1\\) and `median2`
(\\m_2\\). Given the proportion of the first component (\\p_1\\) and the
overall median \\m\\, we have

\$\$p_1 (1 - e^{-\log(2)m/m_1}) + (1 - p_1) (1 - e^{-\log(2)m/m_2}) =
1/2\$\$

This function computes \\m_2\\ or \\m\\ given \\p_1\\ and \\m_1\\. These
parameters can be used in custom random number generator to define
exponential distributed endpoints.

Note that the math formula above may not be displayed correctly on a
html page. You can read it with better format by running
`?solveMixtureExponentialDistribution`.

## Usage

``` r
solveMixtureExponentialDistribution(
  weight1,
  median1,
  median2 = NULL,
  overall_median = NULL
)
```

## Arguments

- weight1:

  numeric. The proportion of the first component.

- median1:

  numeric. Median of the first component.

- median2:

  numeric. Median of the second component. If `NULL`, then
  `overall_median` must be specified, and this function will calculate
  and return `median2`.

- overall_median:

  numeric. Median of the overall population. If `NULL`, then `median2`
  must be specified, and this function will calculate and return
  `overall_median`.

## Value

a named vector of `median2` or `overall_median`.

## Examples

``` r
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union

median2 <-
  solveMixtureExponentialDistribution(
    weight1 = .3,
    median1 = 10,
    overall_median = 8)

median2
#>  median2 
#> 7.305935 

n <- 1e6
ifelse(
  runif(n) < .3,
  rexp(n, rate=log(2)/10),
  rexp(n, rate=log(2)/median2)) %>%
  median() ## should be close to 8
#> [1] 7.998471

overall_median <-
  solveMixtureExponentialDistribution(
    weight1 = .4,
    median1 = 12,
    median2 = 4)

overall_median
#> overall_median 
#>        5.90597 

ifelse(
  runif(n) < .4,
  rexp(n, rate=log(2)/12),
  rexp(n, rate=log(2)/4)) %>%
  median() ## should be close to overall_median
#> [1] 5.901173
```
