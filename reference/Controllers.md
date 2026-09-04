# Class of Controller

Create a class of controller to run a trial.

Public methods in this R6 class are used in developing this package.
Thus, we have to export the whole R6 class which exposures all public
methods. However, only the public methods in the list below are useful
to end users.

- `$run()` run trial simulation, sequentially or in parallel. It cannot
  be called twice on a controller unless `$reset()` is called in
  between.

- `$get_output()` return a data frame of all outputs saved during
  simulation.

- `$reset()` reset the trial and listener registered to the controller
  before starting a new simulation with `$run()`.

## Value

an `R6Class` generator object; use
[`controller()`](https://zhangh12.github.io/TrialSimulator/reference/controller.md)
to create a controller.

## Methods

### Public methods

- [`Controllers$new()`](#method-Controllers-new)

- [`Controllers$reset()`](#method-Controllers-reset)

- [`Controllers$get_output()`](#method-Controllers-get_output)

- [`Controllers$run()`](#method-Controllers-run)

- [`Controllers$clone()`](#method-Controllers-clone)

------------------------------------------------------------------------

### Method `new()`

initialize a controller of the trial

#### Usage

    Controllers$new(trial, listener)

#### Arguments

- `trial`:

  a trial object returned from
  [`trial()`](https://zhangh12.github.io/TrialSimulator/reference/trial.md).

- `listener`:

  a listener object returned from
  [`listener()`](https://zhangh12.github.io/TrialSimulator/reference/listener.md).

------------------------------------------------------------------------

### Method `reset()`

reset the trial and listener registered to the controller so that a new
simulation can be started with `controller$run()`. The trial and the
milestones are restored to their as-designed version.

#### Usage

    Controllers$reset()

------------------------------------------------------------------------

### Method `get_output()`

return a data frame of all current outputs saved by calling
[`save()`](https://rdrr.io/r/base/save.html).

#### Usage

    Controllers$get_output(cols = NULL, simplify = TRUE, tidy = FALSE)

#### Arguments

- `cols`:

  character vector. Columns to be returned from the data frame of
  simulation outputs. If `NULL`, all columns are returned.

- `simplify`:

  logical. Return vector rather than a data frame of one column when
  `length(cols) == 1` and `simplify == TRUE`.

- `tidy`:

  logical. `TrialSimulator` automatically records a set of standard
  outputs at milestones, even when `doNothing` is used as action
  functions. These includes time of triggering milestones, number of
  observed events for time-to-event endpoints, and number of non-missing
  readouts for non-TTE endpoints (see
  [`vignette('actionFunctions')`](https://zhangh12.github.io/TrialSimulator/articles/actionFunctions.md)).
  This usually mean a large number of columns in outputs. If users have
  no intent to summarize a trial on these columns, setting `tidy = TRUE`
  can eliminate these columns from `get_output()`. This is useful to
  reduced the size of output data frame when a large number of
  replicates are done for simulation. Note that currently we use regex
  `"^n_events_<.*?>_<.*?>$"` and `"^milestone_time_<.*?>$"` to match
  columns to be eliminated. If users plan to use `tidy = TRUE`, caution
  is needed when naming custom outputs in
  [`save()`](https://rdrr.io/r/base/save.html). Default `FALSE`.

------------------------------------------------------------------------

### Method `run()`

run trial simulation. It cannot be called again on the same controller
unless `reset()` is called first.

#### Usage

    Controllers$run(
      n = 1,
      n_workers = 1,
      plot_event = TRUE,
      silent = FALSE,
      tidy = FALSE
    )

#### Arguments

- `n`:

  integer. Number of replicates of simulation. `n = 1` by default.
  Simulation results can be accessed by `controller$get_output()`.

- `n_workers`:

  integer. Number of parallel workers. When `n_workers = 1` (default),
  replicates are run sequentially. When `n_workers > 1`, replicates are
  distributed across parallel workers using the `mirai` package, which
  must be installed separately. Each worker receives a serialized copy
  of the trial and listener objects and runs its share of replicates
  independently. If any replicate encounters an error, execution stops
  and already-collected results are preserved in `$get_output()`. To
  debug, manually set `seed` in
  [`trial()`](https://zhangh12.github.io/TrialSimulator/reference/trial.md)
  and `n_workers = 1` in `run()` for reproduced results. Note that
  optimal `n_workers` may not be
  [`parallel::detectCores()`](https://rdrr.io/r/parallel/detectCores.html).
  For example, Macbook with M1/M2/M3 chips may have performance cores
  and efficiency cores. To achieve the best parallel performance, one
  may want to use the performance cores only. For a M1 laptop with 4
  performance cores, `n_workers = 3` may give the best performance.

- `plot_event`:

  logical. Create event plot if `TRUE`. Forced to `FALSE` when `n > 1`
  or `n_workers > 1`.

- `silent`:

  logical. `TRUE` if muting all messages during a trial. Note that
  warning messages are still displayed. When `silent = TRUE` and
  replicates are run sequentially (`n_workers = 1`), a progress bar is
  displayed automatically if the simulation is expected to take more
  than 1 minute.

- `tidy`:

  logical. If `TRUE`, the per-arm event count table (output column
  `n_events_<milestone>_<arms>`) is not saved at milestones; the
  per-endpoint totals and milestone times are still saved. Saving that
  table is the most expensive part of the standard outputs, so
  `tidy = TRUE` is recommended for a large number of replicates unless
  the per-arm counts are needed in the summary. This differs from `tidy`
  in `$get_output()`, which removes all standard columns from the
  returned data frame after the fact. Default `FALSE`.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Controllers$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
##
```
