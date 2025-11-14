# Generate Correlated PFS and OS

Generate correlated PFS and OS endpoints using the three-states model.
This function can be used as custom `generator` in the function
[`endpoint()`](https://zhangh12.github.io/TrialSimulator/reference/endpoint.md).

## Usage

``` r
CorrelatedPfsAndOs3(n, h01, h02, h12, pfs_name = "pfs", os_name = "os")
```

## Arguments

- n:

  integer. Number of observations.

- h01:

  constant transition hazard from state "initial" to state
  "progression".

- h02:

  constant transition hazard from state "initial" to state "death".

- h12:

  constant transition hazard from state "progression" to state "death".

- pfs_name:

  column name of PFS in returned data frame. It must be consistent with
  `name` in the function
  [`endpoint()`](https://zhangh12.github.io/TrialSimulator/reference/endpoint.md).

- os_name:

  column name of OS in returned data frame. It must be consistent with
  `name` in the function
  [`endpoint()`](https://zhangh12.github.io/TrialSimulator/reference/endpoint.md).

## Value

A data frame of `n` rows and four columns, including PFS, OS and their
event indicators. The event indicators are all 1s. The column names are
`<pfs_name>`, `<pfs_name>_event`, `<os_name>`, and `<os_name>_event`.

## Examples

``` r
## use as function (if you don't use TrialSimulator for simulation)
pfs_and_os_trt <- CorrelatedPfsAndOs3(1e4, 0.06, 0.30, 0.30, 'PFS', 'OS')
pfs_and_os_pbo <- CorrelatedPfsAndOs3(1e4, 0.10, 0.40, 0.30, 'PFS', 'OS')

## use as generator (if you use TrialSimulator for simulation)

pfs_and_os <- endpoint(name = c('PFS', 'os'),
                       type = c('tte', 'tte'),
                       generator = CorrelatedPfsAndOs3,
                       h01 = .06, h02 = .30, h12 = .30,
                       pfs_name = 'PFS', os_name = 'os')

pfs_and_os # run it in console to see summary report
#> Summary generated.
```
