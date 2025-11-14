# Class of GraphicalTesting

Perform graphical testing under group sequential design for one or
multiple endpoints. See Maurer & Bretz (2013).

## Methods

### Public methods

- [`GraphicalTesting$new()`](#method-GraphicalTestingProcedure-new)

- [`GraphicalTesting$reset()`](#method-GraphicalTestingProcedure-reset)

- [`GraphicalTesting$is_valid_hid()`](#method-GraphicalTestingProcedure-is_valid_hid)

- [`GraphicalTesting$get_hypothesis_name()`](#method-GraphicalTestingProcedure-get_hypothesis_name)

- [`GraphicalTesting$get_weight()`](#method-GraphicalTestingProcedure-get_weight)

- [`GraphicalTesting$set_weight()`](#method-GraphicalTestingProcedure-set_weight)

- [`GraphicalTesting$get_alpha()`](#method-GraphicalTestingProcedure-get_alpha)

- [`GraphicalTesting$set_alpha()`](#method-GraphicalTestingProcedure-set_alpha)

- [`GraphicalTesting$get_hypotheses_ids()`](#method-GraphicalTestingProcedure-get_hypotheses_ids)

- [`GraphicalTesting$get_number_hypotheses()`](#method-GraphicalTestingProcedure-get_number_hypotheses)

- [`GraphicalTesting$get_hids_not_in_graph()`](#method-GraphicalTestingProcedure-get_hids_not_in_graph)

- [`GraphicalTesting$get_testable_hypotheses()`](#method-GraphicalTestingProcedure-get_testable_hypotheses)

- [`GraphicalTesting$has_testable_hypotheses()`](#method-GraphicalTestingProcedure-has_testable_hypotheses)

- [`GraphicalTesting$is_in_graph()`](#method-GraphicalTestingProcedure-is_in_graph)

- [`GraphicalTesting$is_testable()`](#method-GraphicalTestingProcedure-is_testable)

- [`GraphicalTesting$get_hid()`](#method-GraphicalTestingProcedure-get_hid)

- [`GraphicalTesting$reject_a_hypothesis()`](#method-GraphicalTestingProcedure-reject_a_hypothesis)

- [`GraphicalTesting$set_trajectory()`](#method-GraphicalTestingProcedure-set_trajectory)

- [`GraphicalTesting$get_trajectory()`](#method-GraphicalTestingProcedure-get_trajectory)

- [`GraphicalTesting$test_hypotheses()`](#method-GraphicalTestingProcedure-test_hypotheses)

- [`GraphicalTesting$test()`](#method-GraphicalTestingProcedure-test)

- [`GraphicalTesting$get_current_testing_results()`](#method-GraphicalTestingProcedure-get_current_testing_results)

- [`GraphicalTesting$get_current_decision()`](#method-GraphicalTestingProcedure-get_current_decision)

- [`GraphicalTesting$print()`](#method-GraphicalTestingProcedure-print)

- [`GraphicalTesting$clone()`](#method-GraphicalTestingProcedure-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize an object for graphical testing procedure. Group sequential
design is also supported.

#### Usage

    GraphicalTesting$new(
      alpha,
      transition,
      alpha_spending,
      planned_max_info,
      hypotheses = NULL,
      silent = FALSE
    )

#### Arguments

- `alpha`:

  initial alpha allocated to each of the hypotheses.

- `transition`:

  matrix of transition weights. Its diagonals should be all 0s. The row
  sums should be 1s (for better power) or 0s (if no outbound edge from a
  node).

- `alpha_spending`:

  character vector of same length of `alpha`. Currently it supports
  `'asP'`, `'asOF'`, and `'asUser'`.

- `planned_max_info`:

  vector of integers. Maximum numbers of events (tte endpoints) or
  patients (non-tte endpoints) at the final analysis of each hypothesis
  when planning a trial. The actual numbers could be different, which
  can be specified elsewhere.

- `hypotheses`:

  vector of characters. Names of hypotheses.

- `silent`:

  `TRUE` if muting all messages and not generating plots.

------------------------------------------------------------------------

### Method `reset()`

reset an object of class `GraphicalTesting` to original status so that
it can be reused.

#### Usage

    GraphicalTesting$reset()

------------------------------------------------------------------------

### Method `is_valid_hid()`

determine if index of a hypothesis is valid

#### Usage

    GraphicalTesting$is_valid_hid(hid)

#### Arguments

- `hid`:

  an integer

------------------------------------------------------------------------

### Method `get_hypothesis_name()`

get name of a hypothesis given its index.

#### Usage

    GraphicalTesting$get_hypothesis_name(hid)

#### Arguments

- `hid`:

  an integer

------------------------------------------------------------------------

### Method `get_weight()`

return weight between two nodes

#### Usage

    GraphicalTesting$get_weight(hid1, hid2)

#### Arguments

- `hid1`:

  an integer

- `hid2`:

  an integer

------------------------------------------------------------------------

### Method `set_weight()`

update weight between two nodes

#### Usage

    GraphicalTesting$set_weight(hid1, hid2, value)

#### Arguments

- `hid1`:

  an integer

- `hid2`:

  an integer

- `value`:

  numeric value to be set as a weight two nodes

------------------------------------------------------------------------

### Method `get_alpha()`

return alpha allocated to a hypothesis when calling this function. Note
that a function can be called several time with the graph is updated
dynamically. Thus, returned alpha can be different even for the same
`hid`.

#### Usage

    GraphicalTesting$get_alpha(hid)

#### Arguments

- `hid`:

  an integer

------------------------------------------------------------------------

### Method `set_alpha()`

update alpha of a hypothesis

#### Usage

    GraphicalTesting$set_alpha(hid, value)

#### Arguments

- `hid`:

  integer. Index of a hypothesis

- `value`:

  numeric value to be allocated

------------------------------------------------------------------------

### Method `get_hypotheses_ids()`

return all valid `hid`

#### Usage

    GraphicalTesting$get_hypotheses_ids()

------------------------------------------------------------------------

### Method `get_number_hypotheses()`

return number of hypotheses, including those been rejected.

#### Usage

    GraphicalTesting$get_number_hypotheses()

------------------------------------------------------------------------

### Method `get_hids_not_in_graph()`

return index of hypotheses that are rejected.

#### Usage

    GraphicalTesting$get_hids_not_in_graph()

------------------------------------------------------------------------

### Method `get_testable_hypotheses()`

return index of hypotheses with non-zero alphas, thus can be tested at
the current stage.

#### Usage

    GraphicalTesting$get_testable_hypotheses()

------------------------------------------------------------------------

### Method `has_testable_hypotheses()`

determine whether at least one hypothesis is testable. If return
`FALSE`, the testing procedure is completed.

#### Usage

    GraphicalTesting$has_testable_hypotheses()

------------------------------------------------------------------------

### Method `is_in_graph()`

determine whether a hypothesis is not yet rejected (in graph).

#### Usage

    GraphicalTesting$is_in_graph(hid)

#### Arguments

- `hid`:

  integer. Index of a hypothesis

------------------------------------------------------------------------

### Method `is_testable()`

determine whether a hypothesis has a non-zero alpha allocated.

#### Usage

    GraphicalTesting$is_testable(hid)

#### Arguments

- `hid`:

  integer. Index of a hypothesis

------------------------------------------------------------------------

### Method `get_hid()`

convert hypothesis's name into (unique) index.

#### Usage

    GraphicalTesting$get_hid(hypothesis)

#### Arguments

- `hypothesis`:

  character. Name of a hypothesis. It is different from `hid`, which is
  an index.

------------------------------------------------------------------------

### Method `reject_a_hypothesis()`

remove a node from graph when a hypothesis is rejected

#### Usage

    GraphicalTesting$reject_a_hypothesis(hypothesis)

#### Arguments

- `hypothesis`:

  name of a hypothesis. It is different from `hid`, which is an index.

------------------------------------------------------------------------

### Method `set_trajectory()`

save new testing results at current stage

#### Usage

    GraphicalTesting$set_trajectory(result)

#### Arguments

- `result`:

  a data frame of specific columns.

------------------------------------------------------------------------

### Method `get_trajectory()`

return testing results by the time this function is called. Note that
graphical test is carried out in a sequential manner. Users may want to
review the results anytime. Value returned by this function can possibly
vary over time.

#### Usage

    GraphicalTesting$get_trajectory()

------------------------------------------------------------------------

### Method `test_hypotheses()`

test hypotheses using p-values (and other information in `stats`) base
on the current graph. All rows should have the same order number.

#### Usage

    GraphicalTesting$test_hypotheses(stats)

#### Arguments

- `stats`:

  a data frame. It must contain the following columns:

  `order`

  :   integer. P-values (among others) of hypotheses that can be tested
      at the same time (e.g., an interim, or final analysis) should be
      labeled with the same order number. If a hypothesis is not tested
      at a stage, simply don't put it in `stats` with that order number.

  `hypotheses`

  :   character. Name of hypotheses to be tested. They should be
      identical to those when calling `GraphicalTesting$new`.

  `p`

  :   nominal p-values.

  `info`

  :   observed number of events or samples at test. These will be used
      to compute information fractions in group sequential design.

  `max_info`

  :   integers. Maximum information at test. At interim, `max_info`
      should be equal to `planned_max_info` when calling
      `GraphicalTesting$new`. At the final stage of a hypothesis, one
      can update it with observed numbers.

------------------------------------------------------------------------

### Method `test()`

test hypotheses using p-values (and other information in `stats`) base
on the current graph. Users can call this function multiple times.
P-values of the same order should be passed through `stats` together.
P-values of multiple orders can be passed together as well. For example,
if users only have p-values at current stage, they can call this
function and update the graph accordingly. In this case, column `order`
in `stats` is a constant. They can call this function again when
p-values of next stage is available, where `order` is another integer.
In simulation, if p-values of all stages are on hand, users can call
this function to test them all in a single pass. In this case, column
`order` in `stats` can have different values.

#### Usage

    GraphicalTesting$test(stats)

#### Arguments

- `stats`:

  a data frame. It must contain the following columns:

  `order`

  :   integer. P-values (among others) of hypotheses that can be tested
      at the same time (e.g., an interim, or final analysis) should be
      labeled with the same order number. If a hypothesis is not tested
      at a stage, simply don't put it in `stats` with that order number.
      If all p-values in `stats` are tested at the same stage, `order`
      can be absent.

  `hypotheses`

  :   character. Name of hypotheses to be tested. They should be
      identical to those when calling `GraphicalTesting$new`.

  `p`

  :   nominal p-values.

  `info`

  :   observed number of events or samples at test. These will be used
      to compute information fractions in group sequential design.

  `max_info`

  :   integers. Maximum information at test. At interim, `max_info`
      should be equal to `planned_max_info` when calling
      `GraphicalTesting$new`. At the final stage of a hypothesis, one
      can update it with observed numbers.

  `alpha_spent`

  :   accumulative proportion of allocated alpha to be spent if
      `alpha_spending = "asUser"`. Set it to `NA_real_` otherwise. If no
      hypothesis uses `"asUser"` in `stats`, this column could be
      ignored.

#### Returns

a data frame returned by `get_current_testing_results`. It contains
details of each of the testing steps.

------------------------------------------------------------------------

### Method `get_current_testing_results()`

return testing results with details by the time this function is called.
This function can be called by users by multiple times, thus the
returned value varies over time. This function is called by
`GraphicalTesting::test`, and returns a data frame consisting of columns

- `hypothesis`:

  name of hypotheses.

- `obs_p_value`:

  observed p-values.

- `max_allocated_alpha`:

  maximum allocated alpha for the hypothesis.

- `decision`:

  `'reject'` or `'accept'` the hypotheses.

- `stages`:

  stage of a hypothesis.

- `order`:

  order number that this hypothesis is tested for the last time. It is
  different from `stages`.

- `typeOfDesign`:

  name of alpha spending functions.

#### Usage

    GraphicalTesting$get_current_testing_results()

------------------------------------------------------------------------

### Method `get_current_decision()`

get current decisions for all hypotheses. Returned value could changes
over time because it depends on the stages being tested already.

#### Usage

    GraphicalTesting$get_current_decision()

#### Returns

a named vector of values `"accept"` or `"reject"`. Note that if a
hypothesis is not yet tested when calling this function, the decision
for that hypothesis would be `"accept"`.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

generic function for `print`

#### Usage

    GraphicalTesting$print(graph = TRUE, trajectory = TRUE, ...)

#### Arguments

- `graph`:

  logic. `TRUE` if visualizing the current graph, which can vary over
  time.

- `trajectory`:

  logic. `TRUE` if print the current data frame of trajectory, which can
  vary over time.

- `...`:

  other arguments supported in
  [`gMCPLite::hGraph`](https://merck.github.io/gMCPLite/reference/hGraph.html),
  e.g., `trhw` and `trhh` to control the size of transition box, and
  `trdigits` to control the digits displayed for transition weights.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    GraphicalTesting$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
## Example 1
## dry-run to study the behavior of a graph
## without group sequential design
if(interactive()){
eps <- .01
alpha <- c(.01, .04, 0, 0, 0)
transition <- matrix(c(
  0, 0, 0, 0, 1,
  0, 0, .75, 0, .25,
  0, 1/2-eps/2, 0, eps, 1/2-eps/2,
  0, 0, 0, 0, 0,
  0, 1/2, 1/2, 0, 0
), nrow = 5, byrow = TRUE)

## dummy can be anything, we don't actually use it
asf <- rep('asOF', 5)
## dummy can be anything, we don't actually use it
max_info <- c(300, 1100, 1100, 1100, 500)

hs <- c('H1: UPCR IgA', 'H2: eGFR GN', 'H3: eGFR GN 10wk', 'H5: 2nd Endpoints', 'H4: eGFR IgA')

## initialize an object
gt <- GraphicalTesting$new(alpha, transition, asf, max_info, hs)
print(gt)

## reject hypotheses based on customized order
## to understand the behavior of a testing strategy
## Any other rejection order is possible
gt$reject_a_hypothesis('H1: UPCR IgA')
print(gt)

gt$reject_a_hypothesis('H2: eGFR GN')
print(gt)

gt$reject_a_hypothesis('H4: eGFR IgA')
print(gt)

gt$reject_a_hypothesis('H3: eGFR GN 10wk')
print(gt)

gt$reset()
}

## Example 2
## Example modified from vignettes in gMCPLite:
## Graphical testing for group sequential design
if(interactive()){
## initial alpha split to each of the hypotheses
alpha <- c(.01, .01, .004, .0, .0005, .0005)

## transition matrix of the initial graph
transition <- matrix(c(
  0, 1, 0, 0, 0, 0,
  0, 0, .5, .5, 0, 0,
  0, 0, 0, 1, 0, 0,
  0, 0, 0, 0, .5, .5,
  0, 0, 0, 0, 0, 1,
  .5, .5, 0, 0, 0, 0
), nrow = 6, byrow = TRUE)

## alpha spending functions per hypothesis
asf <- c('asUser', 'asOF', 'asUser', 'asOF', 'asOF', 'asOF')

## planned maximum number of events per hypothesis
max_info <- c(295, 800, 310, 750, 500, 1100)

## name of hypotheses
hs <- c('H1: OS sub',
        'H2: OS all',
        'H3: PFS sub',
        'H4: PFS all',
        'H5: ORR sub',
        'H6: ORR all')

gt <- GraphicalTesting$new(alpha, transition, asf, max_info, hs)

## print initial graph
gt

## nominal p-values at each stage
## Note: p-values with same order are calculated with the same locked data
## Note: alpha_spent is only specified for hypotheses using custom alpha
##       spending function "asUser"
stats <-
  data.frame(
    order = c(1:3, 1:3, 1:2, 1:2, 1, 1),
    hypotheses = c(rep('H1: OS sub', 3), rep('H2: OS all', 3),
                   rep('H3: PFS sub', 2), rep('H4: PFS all', 2),
                   'H5: ORR sub', 'H6: ORR all'),
    p = c(.03, .0001, .000001, .2, .15, .1, .2, .001, .3, .2, .00001, .1),
    info = c(185, 245, 295, 529, 700, 800, 265, 310, 675, 750, 490, 990),
    is_final = c(F, F, T, F, F, T, F, T, F, T, T, T),
    max_info = c(rep(295, 3), rep(800, 3), rep(310, 2), rep(750, 2), 490, 990),
    alpha_spent = c(c(.1, .4, 1), rep(NA, 3), c(.3, 1), rep(NA, 2), NA, NA)
  )

## test the p-values from the first analysis, plot the updated graph
gt$test(stats %>% dplyr::filter(order==1))

## test the p-values from the second analysis, plot the updated graph
gt$test(stats %>% dplyr::filter(order==2))

## test the p-values from the third analysis, plot the updated graph
## because no futher test would be done, displayed results are final
gt$test(stats %>% dplyr::filter(order==3))


## plot the final status of the graph
print(gt, trajectory = FALSE)

## you can get final testing results as follow
gt$get_current_testing_results()

## if you want to see step-by-step details
print(gt$get_trajectory())

## equivalently, you can call gt$test(stats) for only once to get same results.
gt$reset()
gt$test(stats)

## if you only want to get the final testing results
gt$get_current_decision()
}
```
