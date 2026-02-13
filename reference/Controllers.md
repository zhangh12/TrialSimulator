# Class of Controller

Create a class of controller to run a trial.

Public methods in this R6 class are used in developing this package.
Thus, we have to export the whole R6 class which exposures all public
methods. However, only the public methods in the list below are useful
to end users.

- `$run()`

- `$get_output()`

- `$reset()`

## Methods

### Public methods

- [`Controllers$new()`](#method-Controllers-new)

- [`Controllers$get_listener()`](#method-Controllers-get_listener)

- [`Controllers$get_trial()`](#method-Controllers-get_trial)

- [`Controllers$mute()`](#method-Controllers-mute)

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

### Method `get_listener()`

return listener in a controller.

#### Usage

    Controllers$get_listener()

------------------------------------------------------------------------

### Method `get_trial()`

return trial in a controller.

#### Usage

    Controllers$get_trial()

------------------------------------------------------------------------

### Method `mute()`

mute all messages (not including warnings).

#### Usage

    Controllers$mute()

#### Arguments

- `silent`:

  logical.

------------------------------------------------------------------------

### Method `reset()`

reset the trial and listener registered to the controller before running
additional replicate of simulation. This is usually done between two
calls of `controller$run()`.

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

run trial simulation.

#### Usage

    Controllers$run(
      n = 1,
      n_workers = 1,
      plot_event = TRUE,
      silent = FALSE,
      dry_run = FALSE
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
  and already-collected results are preserved in `$get_output()`.

- `plot_event`:

  logical. Create event plot if `TRUE`. Users should set it to be
  `FALSE` if `n > 1`. Forced to `FALSE` when `n_workers > 1`.

- `silent`:

  logical. `TRUE` if muting all messages during a trial. Note that
  warning messages are still displayed.

- `dry_run`:

  logical. We are considering retire this argument. `TRUE` if action
  function provided by users is ignored and an internal default action
  `.default_action` is called instead. This default function only locks
  data when the milestone is triggered. Milestone time and number of
  endpoints' events or sample sizes are saved. It is suggested to set
  `dry_run = TRUE` to estimate distributions of triggering time and
  number of events before formally using custom action functions if a
  fixed design is in use. This helps determining planned maximum
  information for group sequential design and reasonable time of
  milestone of interest when planning a trial. Set it to `FALSE` for
  formal simulations. However, for an adaptive design where arm(s) could
  possibly be added or removed, setting `dry_run` to `TRUE` is usually
  not helpful because adaption should be executed before estimating the
  milestone time.

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
