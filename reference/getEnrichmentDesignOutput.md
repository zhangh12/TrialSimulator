# Get simulation output in the vignette enrichmentDesign.Rmd

Internal function that retrieves precomputed simulation results. Not
meant for use by package users.

## Usage

``` r
getEnrichmentDesignOutput()
```

## Value

A data frame containing simulation results of 10000 replicates under the
alternative and 10000 replicates under the global null, distinguished by
the column `scenario`.
