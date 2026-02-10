# Class of Endpoint

Create a class of endpoint to specify its name, type, readout time
(optional) and assign a random number generator.

Public methods in this R6 class are used in developing this package.
Thus, I have to export the whole R6 class which exposures all public
methods. However, none of the public methods is useful to end users
except for the one below.

- `$print()`

## Methods

### Public methods

- [`Endpoints$new()`](#method-Endpoints-new)

- [`Endpoints$test_generator()`](#method-Endpoints-test_generator)

- [`Endpoints$get_generator()`](#method-Endpoints-get_generator)

- [`Endpoints$update_generator()`](#method-Endpoints-update_generator)

- [`Endpoints$get_readout()`](#method-Endpoints-get_readout)

- [`Endpoints$get_uid()`](#method-Endpoints-get_uid)

- [`Endpoints$get_name()`](#method-Endpoints-get_name)

- [`Endpoints$get_type()`](#method-Endpoints-get_type)

- [`Endpoints$print()`](#method-Endpoints-print)

- [`Endpoints$clone()`](#method-Endpoints-clone)

------------------------------------------------------------------------

### Method `new()`

initialize an endpoint.

#### Usage

    Endpoints$new(name, type = c("tte", "non-tte"), readout = NULL, generator, ...)

#### Arguments

- `name`:

  character vector. Name(s) of endpoint(s)

- `type`:

  character vector. Type(s) of endpoint(s). It supports `"tte"` for
  time-to-event endpoints, and `"non-tte"` for all other types of
  endpoints (e.g., continous, binary, categorical, or repeated
  measurement. `TrialSimulator` will do some verification if an endpoint
  is of type `"tte"`. However, no special manipulation is done for
  non-tte endpoints.

- `readout`:

  a named numeric vector with name to be non-tte endpoint(s). `readout`
  must be specified for every non-tte endpoint. For example,
  `c(endpoint1 = 6, endpoint2 = 3)`, which means that it takes 6 and 3
  unit time to get readout of `endpoint1` and `endpoint2` of a patient
  since being randomized. Error message would be prompted if `readout`
  is not named or readout is not specified for some non-tte endpoint. If
  all endpoints are tte, `readout` should be `NULL` as default.

- `generator`:

  a random number generation (RNG) function. It supports all built-in
  random number generators in `stats`, e.g.,
  [`stats::rnorm`](https://rdrr.io/r/stats/Normal.html),
  [`stats::rexp`](https://rdrr.io/r/stats/Exponential.html), etc. that
  with `n` as the argument for number of observations and returns a
  vector. A custom RNG function is also supported. `generator` could be
  any functions as long as (1) its first argument is `n`; and (2) it
  returns a vector of length `n` (univariate endpoint) or a data frame
  of `n` rows (multiple endpoints), i.e., custom RNG can return data of
  more than one endpoint. This is useful when users need to simulate
  correlated endpoints or longitudinal data. The column names of
  returned data frame should match to `name` exactly, although order of
  columns does not matter. If an endpoint is of type `"tte"`, the custom
  `generator` should also return a column as its event indicator. For
  example, if `"pfs"` is `"tte"`, then custom `generator` should return
  at least two columns `"pfs"` and `"pfs_event"`. Usually `pfs_event`
  can be all 1s if no censoring. Some RNG functions, e.g.,
  [`TrialSimulator::PiecewiseConstantExponentialRNG()`](https://zhangh12.github.io/TrialSimulator/reference/PiecewiseConstantExponentialRNG.md)
  and
  [`TrialSimulator::CorrelatedPfsAndOs4()`](https://zhangh12.github.io/TrialSimulator/reference/CorrelatedPfsAndOs4.md),
  simulate TTE endpoint data with censoring simultaneously, thus 0
  exists in the columns of event indicators. Users can implement
  censorship in their own RNG. Censoring can also be specified later
  when defining a trial object through argument `dropout`. See
  [`?trial`](https://zhangh12.github.io/TrialSimulator/reference/trial.md).
  Note that if covariates, e.g., biomarker, subgroup, are needed in
  generating and analyzing trial data, they can and should be defined as
  endpoints in
  [`endpoint()`](https://zhangh12.github.io/TrialSimulator/reference/endpoint.md)
  as well.

- `...`:

  optional arguments for `generator`.

------------------------------------------------------------------------

### Method `test_generator()`

test random number generator of the endpoints. It returns an example
dataset of an endpoint object. Note that users of `TrialSimulator` does
not need to call this function to generate trial data; instead, the
package will call this function at milestone automatically. Users may
see example in vignette where this function is called. However, it is
for illustration purpose only. In practice, this function may be used
for debugging if users suspect some issues in custom generator,
otherwise, this function should never been called in formal simulation.

#### Usage

    Endpoints$test_generator(n = 1000)

#### Arguments

- `n`:

  integer. Number of random numbers generated from the generator.

------------------------------------------------------------------------

### Method `get_generator()`

return random number generator of an endpoint

#### Usage

    Endpoints$get_generator()

------------------------------------------------------------------------

### Method [`update_generator()`](https://zhangh12.github.io/TrialSimulator/reference/update_generator.md)

update endpoint generator

#### Usage

    Endpoints$update_generator(generator, ...)

#### Arguments

- `generator`:

  a random number generation (RNG) function. See `generator` of
  [`endpoint()`](https://zhangh12.github.io/TrialSimulator/reference/endpoint.md).

- `...`:

  optional arguments for `generator`.

------------------------------------------------------------------------

### Method `get_readout()`

return readout function

#### Usage

    Endpoints$get_readout()

------------------------------------------------------------------------

### Method `get_uid()`

return uid

#### Usage

    Endpoints$get_uid()

------------------------------------------------------------------------

### Method `get_name()`

return endpoints' name

#### Usage

    Endpoints$get_name()

------------------------------------------------------------------------

### Method `get_type()`

return endpoints' type

#### Usage

    Endpoints$get_type()

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

print an endpoint object

#### Usage

    Endpoints$print(categorical_vars = NULL)

#### Arguments

- `categorical_vars`:

  a character vector of endpoints. This can be used to force variables
  with limited distinct values as categorical variables in summary
  report. For example, a numeric endpoint may take integer values 0,
  1, 2. Instead of computing mean and standard derivation in the summary
  report, put this endpoint in `categorical_vars` can force it be a
  categorical variable and a barplot is generated in summary report
  instead.

#### Examples

    rng <- function(n){
      data.frame(x = sample(1:3, n, replace = TRUE),
                 y = sample(1:3, n, replace = TRUE)
                )
    }
    ep <- endpoint(name = c('x', 'y'),
                   type = c('non-tte', 'non-tte'),
                   readout = c(x = 0, y = 0),
                   generator = rng)

    ## x and y as continuous endpoints, thus mean and sd are reported
    ep

    ## force y to be categorical to create barplot of it
    print(ep, categorical_vars = 'y')

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Endpoints$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# Instead of using Endpoints$new(), please use endpoint(), a user-friendly
# wrapper to define endpoints. See examples in ?endpoint.


## ------------------------------------------------
## Method `Endpoints$print`
## ------------------------------------------------


rng <- function(n){
  data.frame(x = sample(1:3, n, replace = TRUE),
             y = sample(1:3, n, replace = TRUE)
            )
}
ep <- endpoint(name = c('x', 'y'),
               type = c('non-tte', 'non-tte'),
               readout = c(x = 0, y = 0),
               generator = rng)

## x and y as continuous endpoints, thus mean and sd are reported
ep
#> Summary generated.

## force y to be categorical to create barplot of it
print(ep, categorical_vars = 'y')
#> Summary generated.
```
