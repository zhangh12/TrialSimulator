# Class of GroupSequentialTest

Perform group sequential test for a single endpoint based on sequential
one-sided p-values at each stages. Selected alpha spending functions,
including user-defined functions, are supported. Boundaries are
calculated with \`rpact\`. At the final analysis, adjustment can be
applied for over-running or under-running trial where observed final
information is greater or lower than the planned maximum information.
See Wassmer & Brannath, 2016, p78f. The test is based on p-values not z
statistics because it is easier to not handling direction of alternative
hypothesis in current implementation. In addition, only one-sided test
is supported which should be sufficient for common use in clinical
design.

## Methods

### Public methods

- [`GroupSequentialTest$new()`](#method-GroupSequentialTest-new)

- [`GroupSequentialTest$get_name()`](#method-GroupSequentialTest-get_name)

- [`GroupSequentialTest$get_alpha()`](#method-GroupSequentialTest-get_alpha)

- [`GroupSequentialTest$set_alpha_spending()`](#method-GroupSequentialTest-set_alpha_spending)

- [`GroupSequentialTest$get_alpha_spending()`](#method-GroupSequentialTest-get_alpha_spending)

- [`GroupSequentialTest$get_max_info()`](#method-GroupSequentialTest-get_max_info)

- [`GroupSequentialTest$set_max_info()`](#method-GroupSequentialTest-set_max_info)

- [`GroupSequentialTest$get_stage()`](#method-GroupSequentialTest-get_stage)

- [`GroupSequentialTest$reset()`](#method-GroupSequentialTest-reset)

- [`GroupSequentialTest$set_trajectory()`](#method-GroupSequentialTest-set_trajectory)

- [`GroupSequentialTest$get_trajectory()`](#method-GroupSequentialTest-get_trajectory)

- [`GroupSequentialTest$get_stage_level()`](#method-GroupSequentialTest-get_stage_level)

- [`GroupSequentialTest$test_one()`](#method-GroupSequentialTest-test_one)

- [`GroupSequentialTest$test()`](#method-GroupSequentialTest-test)

- [`GroupSequentialTest$print()`](#method-GroupSequentialTest-print)

- [`GroupSequentialTest$clone()`](#method-GroupSequentialTest-clone)

------------------------------------------------------------------------

### Method `new()`

initialize a group sequential test. Now only support one-sided test
based on p-values.

#### Usage

    GroupSequentialTest$new(
      alpha = 0.025,
      alpha_spending = c("asP", "asOF", "asUser"),
      planned_max_info,
      name = "H0",
      silent = TRUE
    )

#### Arguments

- `alpha`:

  familywise error rate

- `alpha_spending`:

  alpha spending function. Use `"asUser"` if custom alpha spending
  schedule is used.

- `planned_max_info`:

  integer. Planned maximum number of patients for non-tte endpoints or
  number of events for tte endpoints

- `name`:

  character. Name of the hypothesis, e.g. endpoint, subgroup, etc.
  Optional.

- `silent`:

  `TRUE` if muting all messages.

------------------------------------------------------------------------

### Method `get_name()`

get name of hypothesis

#### Usage

    GroupSequentialTest$get_name()

------------------------------------------------------------------------

### Method `get_alpha()`

get overall alpha

#### Usage

    GroupSequentialTest$get_alpha()

------------------------------------------------------------------------

### Method `set_alpha_spending()`

set alpha spending function. This is useful when set 'asUser' at the
final stage to adjust for an under- or over-running trial.

#### Usage

    GroupSequentialTest$set_alpha_spending(asf)

#### Arguments

- `asf`:

  character of alpha spending function.

------------------------------------------------------------------------

### Method `get_alpha_spending()`

return character of alpha spending function

#### Usage

    GroupSequentialTest$get_alpha_spending()

------------------------------------------------------------------------

### Method `get_max_info()`

return planned maximum information

#### Usage

    GroupSequentialTest$get_max_info()

------------------------------------------------------------------------

### Method `set_max_info()`

set planned maximum information. This is used at the final stage to
adjust for an under- or over-running trial.

#### Usage

    GroupSequentialTest$set_max_info(obs_max_info)

#### Arguments

- `obs_max_info`:

  integer. Maximum information, which could be observed number of
  patients or events at the final stage.

------------------------------------------------------------------------

### Method `get_stage()`

get current stage.

#### Usage

    GroupSequentialTest$get_stage()

------------------------------------------------------------------------

### Method `reset()`

an object of class `GroupSequentialTest` is designed to be used
sequentially by calling `GroupSequentialTest$test`. When all planned
tests are performed, no further analysis could be done. In that case
keep calling `GroupSequentialTest$test` will trigger an error. To reuse
the object for a new set of staged p-values, call this function to reset
the status to stage 1. See examples. This implementation can prevent the
error that more than the planned number of stages are tested.

#### Usage

    GroupSequentialTest$reset()

------------------------------------------------------------------------

### Method `set_trajectory()`

save testing result at current stage

#### Usage

    GroupSequentialTest$set_trajectory(result, is_final = FALSE)

#### Arguments

- `result`:

  a data frame storing testing result at a stage.

- `is_final`:

  logical. `TRUE` if final test for the hypothesis, `FALSE` otherwise.

------------------------------------------------------------------------

### Method `get_trajectory()`

return testing trajectory until current stage. This function can be
called at any stage. See examples.

#### Usage

    GroupSequentialTest$get_trajectory()

------------------------------------------------------------------------

### Method `get_stage_level()`

compute boundaries given current (potentially updated) settings. It
returns different values if settings are changed over time.

#### Usage

    GroupSequentialTest$get_stage_level()

------------------------------------------------------------------------

### Method `test_one()`

test a hypothesis with the given p-value at current stage

#### Usage

    GroupSequentialTest$test_one(
      p_value,
      is_final,
      observed_info,
      alpha_spent = NA_real_
    )

#### Arguments

- `p_value`:

  numeric. A p-value.

- `is_final`:

  logical. `TRUE` if this test is carried out for the final analysis.

- `observed_info`:

  integer. Observed information at current stage. It can be the number
  of samples (non-tte) or number of events (tte) at test. If the current
  stage is final, observed_info will be used to update planned_max_info,
  the alpha spending function (`typeOfDesign` in `rpact`) will be
  updated to `'asUser'`, and the argument `userAlphaSpending` will be
  used when calling
  [`rpact::getDesignGroupSequential`](https://rpact-com.github.io/rpact/reference/getDesignGroupSequential.html).

- `alpha_spent`:

  numeric if `alpha_spending = "asUser"`. It must be between 0 and
  `alpha`, the overall alpha of the test. `NA_real_` for other alpha
  spending functions `"asOF"` and `"asP"`.

------------------------------------------------------------------------

### Method `test()`

Carry out test based on group sequential design. If `p_values` is
`NULL`, dummy values will be use and boundaries are calculated for users
to review.

#### Usage

    GroupSequentialTest$test(
      observed_info,
      is_final,
      p_values = NULL,
      alpha_spent = NULL
    )

#### Arguments

- `observed_info`:

  a vector of integers, observed information at stages.

- `is_final`:

  logical vector. `TRUE` if the test is for the final analysis.

- `p_values`:

  a vector of p-values. If specified, its length should equal to the
  length of `observed_info`.

- `alpha_spent`:

  accumulative alpha spent at observed information. It is a numeric
  vector of values between 0 and 1, and of length that equals
  `length(observed_info)` if alpha-spending function is `"asUser"`.
  Otherwise `NULL`.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

generic function for `print`

#### Usage

    GroupSequentialTest$print()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    GroupSequentialTest$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
## Note: examples showed here replicate the results from
## https://www.rpact.org/vignettes/planning/rpact_boundary_update_example/

## Example 1. Generate boundaries for a pre-fix group sequential design
gst <- GroupSequentialTest$new(
  alpha = .025, alpha_spending = 'asOF',
  planned_max_info = 387)

## without giving p-values, boundaries are returned without actual testing
gst$test(observed_info = c(205, 285, 393), is_final = c(FALSE, FALSE, TRUE))
gst
#>   typeOfDesign stages informationRates alpha sided alphaSpent criticalValues
#> 1         asOF      1        0.5216285 0.025     1 0.00207258       2.866898
#> 2         asOF      2        0.7251908 0.025     1 0.00900462       2.392987
#> 3       asUser      3        1.0000000 0.025     1 0.02499999       2.013686
#>   stageLevels obs_p_value decision hypothesis
#> 1 0.002072584          NA       NA         H0
#> 2 0.008355905          NA       NA         H0
#> 3 0.022021239          NA       NA         H0

## Example 2. Calculate boundaries with observed information at stages
## No p-values are provided

## get an error without resetting an used object
try( gst$test(observed_info = 500, is_final = FALSE) )
#> Error in gst$test(observed_info = 500, is_final = FALSE) : 
#>   Group sequential test has been completed. 
#> No further test is available. 
#> Run GroupSequentialTest$reset() and try again. 

## reset the object for re-use
gst$reset()
#> GroupSequentialTest object <H0> has been reset and is ready to use. 
gst$test(observed_info = c(205, 285, 393), is_final = c(FALSE, FALSE, TRUE))
gst
#>   typeOfDesign stages informationRates alpha sided alphaSpent criticalValues
#> 1         asOF      1        0.5216285 0.025     1 0.00207258       2.866898
#> 2         asOF      2        0.7251908 0.025     1 0.00900462       2.392987
#> 3       asUser      3        1.0000000 0.025     1 0.02499999       2.013686
#>   stageLevels obs_p_value decision hypothesis
#> 1 0.002072584          NA       NA         H0
#> 2 0.008355905          NA       NA         H0
#> 3 0.022021239          NA       NA         H0

## Example 3. Test stagewise p-values sequentially
gst$reset()
#> GroupSequentialTest object <H0> has been reset and is ready to use. 

gst$test(observed_info = 205, is_final = FALSE, p_values = .09)
gst$test(285, FALSE, .006)

## print testing trajectory by now
gst
#>   typeOfDesign stages informationRates alpha sided alphaSpent criticalValues
#> 1         asOF      1        0.5297158 0.025     1 0.00207258       2.866898
#> 2         asOF      2        0.7364341 0.025     1 0.00900462       2.392987
#>   stageLevels obs_p_value decision hypothesis
#> 1 0.002072584       0.090   accept         H0
#> 2 0.008355905       0.006   reject         H0

gst$test(393, TRUE, .002)

## print all testing trajectory
gst
#>   typeOfDesign stages informationRates alpha sided alphaSpent criticalValues
#> 1         asOF      1        0.5216285 0.025     1 0.00207258       2.866898
#> 2         asOF      2        0.7251908 0.025     1 0.00900462       2.392987
#> 3       asUser      3        1.0000000 0.025     1 0.02499999       2.013686
#>   stageLevels obs_p_value decision hypothesis
#> 1 0.002072584       0.090   accept         H0
#> 2 0.008355905       0.006   reject         H0
#> 3 0.022021239       0.002   reject         H0

## you can also test all stages at once
## the result is the same as calling test() for each of the stages
gst$reset()
#> GroupSequentialTest object <H0> has been reset and is ready to use. 
gst$test(c(205, 285, 393), c(FALSE, FALSE, TRUE), c(.09, .006, .002))
gst
#>   typeOfDesign stages informationRates alpha sided alphaSpent criticalValues
#> 1         asOF      1        0.5216285 0.025     1 0.00207258       2.866898
#> 2         asOF      2        0.7251908 0.025     1 0.00900462       2.392987
#> 3       asUser      3        1.0000000 0.025     1 0.02499999       2.013686
#>   stageLevels obs_p_value decision hypothesis
#> 1 0.002072584       0.090   accept         H0
#> 2 0.008355905       0.006   reject         H0
#> 3 0.022021239       0.002   reject         H0

## Example 4. use user-define alpha spending
gst <- GroupSequentialTest$new(
  alpha = .025, alpha_spending = 'asUser',
  planned_max_info = 387)

gst$test(
  observed_info = c(205, 285, 393),
  is_final = c(FALSE, FALSE, TRUE),
  alpha_spent = c(.005, .0125, .025))
gst
#>   typeOfDesign stages informationRates alpha sided alphaSpent criticalValues
#> 1       asUser      1        0.5216285 0.025     1 0.00499999       2.575898
#> 2       asUser      2        0.7251908 0.025     1 0.01249999       2.309700
#> 3       asUser      3        1.0000000 0.025     1 0.02499999       2.065005
#>   stageLevels obs_p_value decision hypothesis
#> 1 0.004999005          NA       NA         H0
#> 2 0.010452387          NA       NA         H0
#> 3 0.019461267          NA       NA         H0
```
