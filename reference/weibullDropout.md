# Calculate Parameters of Weibull Distribution as a Dropout Method

Fit scale and shape parameters of the Weibull distribution to match
dropout rates at two specified time points. Weibull distribution can be
used as a dropout distribution because it has two parameters.

Note that It is users' responsibility to assure that the units of
dropout time, readout of non-tte endpoints, and trial duration are
consistent.

## Usage

``` r
weibullDropout(time, dropout_rate)
```

## Arguments

- time:

  a numeric vector of two time points at which dropout rates are
  specified.

- dropout_rate:

  a numeric vector of dropout rates at `time`.

## Value

a named vector for scale and shape parameters.

## Examples

``` r
## dropout rates are 8% and 18% at time 12 and 18.
weibullDropout(time = c(12, 18), dropout_rate = c(.08, .18))
#>     shape     scale 
#>  2.138567 38.343517 

```
