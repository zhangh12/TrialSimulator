# Generate Correlated PFS and OS Using Gumbel Copula

Generate correlated PFS and OS endpoints using the Gumbel copula.
Marginally, both PFS and OS follow exponential distributions. This
function can be used as custom `generator` in the function
[`endpoint()`](https://zhangh12.github.io/TrialSimulator/reference/endpoint.md).

Note that the Gumbel copula is applied to the survival functions of OS
and time-to-progression (TTP). PFS is defined as min(TTP, OS), which
also follows an exponential distribution.

For more information, refer to [this
vignette](https://zhangh12.github.io/TrialSimulator/articles/simulatePfsAndOsGumbel.html).

## Usage

``` r
CorrelatedPfsAndOs2(
  n,
  median_pfs,
  median_os,
  kendall,
  pfs_name = "pfs",
  os_name = "os"
)
```

## Arguments

- n:

  integer. Number of observations.

- median_pfs:

  numeric. Median of PFS.

- median_os:

  numeric. Median of OS.

- kendall:

  numeric. Kendall's tau between observed, uncensored PFS and OS. Must
  be positive and usually away from zero. Note that this argument is not
  the Kendall's tau between TTP and OS.

- pfs_name:

  column name of PFS in returned data frame. It must be consistent with
  name in the function
  [`endpoint()`](https://zhangh12.github.io/TrialSimulator/reference/endpoint.md).

- os_name:

  column name of OS in returned data frame. It must be consistent with
  name in the function
  [`endpoint()`](https://zhangh12.github.io/TrialSimulator/reference/endpoint.md).

## Value

A data frame of `n` rows and four columns, including PFS, OS and their
event indicators. The event indicators are all 1s. The column names are
`<pfs_name>`, `<pfs_name>_event`, `<os_name>`, and `<os_name>_event`.

## Examples

``` r
pfs_and_os <- endpoint(name = c('PFS', 'Os'),
                       type = c('tte', 'tte'),
                       generator = CorrelatedPfsAndOs2,
                       median_pfs = 5,
                       median_os = 11,
                       kendall = .6,
                       pfs_name = 'PFS',
                       os_name = 'Os')

pfs_and_os # run it in console to see summary report
#> Summary generated.

## for validation purpose only
## not the recommended way to use TrialSimulator
dat <- pfs_and_os$test_generator(n = 1e4)
cor(dat[, 1:2], method = 'kendall') ## close to 0.6
#>           PFS        Os
#> PFS 1.0000000 0.6012212
#> Os  0.6012212 1.0000000
```
