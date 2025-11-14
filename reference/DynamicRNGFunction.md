# A wrapper of random number generator.

This function may be useful to advanced users of `TrialSimulator`. It
creates a wrapper function of a random number generator, while fixing a
subset or all of arguments. This function is design to prevent
inadvertent changing to arguments of random number generator. See
examples below.

## Usage

``` r
DynamicRNGFunction(fn, ...)
```

## Arguments

- fn:

  random number generator, e.g., `rnorm`, `rchisq`, etc. It can be
  user-defined random number generator as well, e.g.,
  `PiecewiseConstantExponentialRNG`.

- ...:

  arguments for `fn`. Specifying invalid arguments can trigger error and
  be stopped. There are three exceptions. (1) `rng` can be passed
  through `...` to give true name of `fn`. This could be necessary as it
  may be hard to parse it accurately in `DynamicRNGFunction`, or simply
  for a more informative purpose in some scenarios. (2) `var_name` can
  be passed through `...` to specify the name of generated variable. (3)
  `simplify` can be set to `FALSE` to convert a vector into a one-column
  data frame in returned object. This happens for built-in random number
  generators, e.g., `rnorm`, `rbinom`, etc. These three arguments will
  not be passed into `fn`.

## Value

a function to generate random number based on `fn` and arguments in
`...`. Specified arguments will be fixed and cannot be changed when
invoking `DynamicRNGFunction(fn, ...)()`. For example, if
`foo <- DynamicRNGFunction(rnorm, sd = 2)`, then `foo(n = 100)` will
always generate data from normal distribution of variance 4.
`foo(n = 100, sd = 1)` will trigger an error. However, if an argument is
not specified in `DynamicRNGFunction`, then it can be specified later.
For example, `foo(n = 100, mean = -1)` will generate data from N(-1, 4).

## Examples

``` r
# example code
dfunc <- DynamicRNGFunction(rnorm, sd = 3.2)
x <- dfunc(1e3) # mean 0 and sd 3.2
hist(x)


y <- dfunc(1e3, mean = 3.5) # mean can be changed
mean(y)
#> [1] 3.477097

try(z <- dfunc(1e3, sd = 1)) # error because sd is fixed in dfunc
#> Error in dfunc(1000, sd = 1) : Cannot override fixed arguments: sd
```
