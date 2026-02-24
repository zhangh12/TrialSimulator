# Changelog

## TrialSimulator 1.11.0

### New Feature

- Add function
  [`solvePiecewiseConstantExponentialDistribution()`](https://zhangh12.github.io/TrialSimulator/reference/solvePiecewiseConstantExponentialDistribution.md)
  to compute event rates in time windows given survival probabilities at
  changepoints.
- Add `qPiecewiseExponent()`, the quantile function of piecewise
  exponential distribution. This function is useful to simulate
  time-to-event endpoint that is correlated to other endpoints using the
  copula method. For example, the `simdata` package needs marginal
  quantile functions.

## TrialSimulator 1.10.0

CRAN release: 2026-02-15

### New Feature

- Support parallelization in `Controllers$run()` through new argument
  `n_workers`. The package `mirai` is used. Although `mirai` advocates
  the use of L’Ecuyer-CMRG streams to maintain independence between
  multiple streams, however, `TrialSimulator` resets it to be
  Mersenne-Twister streams to force identical behavior between
  `n_workers = 1` and `n_workers > 1`. This enables debuggability and
  reproduciability under single-process mode by setting seed that causes
  issues under multi-process mode.

## TrialSimulator 1.9.0

### New Feature

- Support wrapper functions for adaptation, including
  [`remove_arms()`](https://zhangh12.github.io/TrialSimulator/reference/remove_arms.md),
  [`add_arms()`](https://zhangh12.github.io/TrialSimulator/reference/add_arms.md),
  [`update_sample_ratio()`](https://zhangh12.github.io/TrialSimulator/reference/update_sample_ratio.md),
  [`set_duration()`](https://zhangh12.github.io/TrialSimulator/reference/set_duration.md),
  [`resize()`](https://zhangh12.github.io/TrialSimulator/reference/resize.md)
  and
  [`update_generator()`](https://zhangh12.github.io/TrialSimulator/reference/update_generator.md).
- More informative message is prompted when error is throwed from an
  action function; milestone’s name is printed.

## TrialSimulator 1.8.0

### New Feature

- Support new adaptation `Trials$resize()` that resizes an ongoing
  trial.

## TrialSimulator 1.7.0

CRAN release: 2025-12-19

### New Feature

- Columns automatically recorded at milestones can be eliminated from
  `get_output()` by setting new argument `tidy = TRUE`.

### Minor Updates

- Some minor fixes for CRAN submission.

## TrialSimulator 1.6.0

### New Feature

- Generator of an endpoint can be updated during a running trial with
  `Trials$update_generator()`.

## TrialSimulator 1.5.0

### Update

- `enforce = TRUE` is no longer needed when adding new arms to an
  existing trial with at least one arm through `Trials$add_arms()`.
  However, for backward compatibility, legacy codes with
  `enforce = TRUE` still behaves as expected and no need to update.

## TrialSimulator 1.4.0

### Update

- Add vignette of dose-ranging study.

## TrialSimulator 1.3.0

CRAN release: 2025-09-26

### Major Updates

- Action function no longer needs argument `milestone_name`. Now action
  function only requires argument `trial` and supports optional
  arguments.
- [`milestone()`](https://zhangh12.github.io/TrialSimulator/reference/milestone.md)
  now support `...` to pass arguments to action functions.
- Documents and vignettes are updated.

### Minor Updates

- Some minor fixes.

## TrialSimulator 1.2.0

### New Feature

- [`enrollment()`](https://zhangh12.github.io/TrialSimulator/reference/enrollment.md)
  now supports `min_treatment_duration` to ensure minimum treatment
  duration received by patients at a milestone. With its default value
  0, milestone is triggered when a specific number of patients are
  enrolled.
- No longer print the return value of action function, thus
  `invisible(NULL)` is no longer recommended as return value of action
  function.

## TrialSimulator 1.1.0

### Update

- Add notes to R6 class indicating public methods that can be used by
  end users.
- Update help documents.

## TrialSimulator 1.0.0

CRAN release: 2025-09-03

### Update

- Fix issues to meet CRAN submission conditions.

## TrialSimulator 0.97.0

### Bug Fix

- Fix a bug in function `event_plot()` for plot of cumulative events
  number when endpoint name is `"ep"`. This is due to data masking in
  `dplyr`.
- Fix issues in unit tests caused by new dropout mechanisum.
- use Bonferroni method in unit test of `update_sample_ratio`. This test
  is probably broken by randomness (it is okay).

## TrialSimulator 0.96.0

### Update

- Update mechanism of simulating dropout time. Switching from trial
  level to patient level, i.e. dropout time is now the time from a
  patient is enrolled until leaving a trial. This aligns with common
  practice and popular softwares.

## TrialSimulator 0.95.0

### Update

- Add vignette of action function.

## TrialSimulator 0.94.0

### New Feature

- Add function `summarizeMilestoneTime` and its plot method to summarize
  triggering time of milestones.

## TrialSimulator 0.93.0

### New Feature

- [`update_sample_ratio()`](https://zhangh12.github.io/TrialSimulator/reference/update_sample_ratio.md)
  now supports updating multiple arms simultaneously. When ratio is not
  a whole number, [`sample()`](https://rdrr.io/r/base/sample.html) is
  used to replace the permuted block algorithm to randomize patients to
  arms. This enable response-adaptive design.
- Add a vignette of response-adaptive design using
  [`update_sample_ratio()`](https://zhangh12.github.io/TrialSimulator/reference/update_sample_ratio.md).

## TrialSimulator 0.92.0

### Update

-Add vignette of fixed design. -Add vignette of wrapper functions of
commom statistical tests.

## TrialSimulator 0.91.0

### New Feature

- Add function that maps medians of PFS and OS, and their correlation to
  the hazard parameters. The induced hazard parameters can be used with
  PFS-OS generator .

### Update

- Add vignette of simulating PFS and OS.

## TrialSimulator 0.90.0

### New Feature

- Save event counts per arm in simulation output.
- No long stop the program when all planned patients are already
  randomized into the trial when calling the function `enroll_patients`.
  This is useful when a milestone is triggered after all patients are
  recruited.

### Bug Fix

- Fix a bug that affects functions and when patient recruitment is
  completed fast thus no sample increment between some milestones. This
  bug can substantially reduce testing powers.

## TrialSimulator 0.89.0

### New Feature

- Support `...` in `eventNumber` to count event in subset of trial data.
  This is useful in enrichment design when milestone is defined based on
  biomarker.

## TrialSimulator 0.88.0

### New Feature

- Add function `get` as alias of `get_custom_data` in the `Trials`
  class.

### Bug Fix

- Fix a bug that no results is returned to controller when an error is
  triggered.

### Update

- Print informative message when
  `trial$dunnettTest(..., planned_info = "default")` triggers an error.

## TrialSimulator 0.87.0

### Bug Fix

- Revise `Trials$dunnettTest` to be compatible to one-sided logrank
  test.

## TrialSimulator 0.86.0

### Bug Fix

- Fix a bug in the fourth-state model.

## TrialSimulator 0.85.0

### New Feature

- Add data generators of time to response, progression and death.

## TrialSimulator 0.84.0

### New Feature

- The function `fitLogrank` now supports `formula`. `strata(...)` can be
  in `formula`. No covariate is accepted.
- Add unit tests for `fitLogistic`.

## TrialSimulator 0.83.0

### New Feature

- The function `fitLogistic` now supports `scale = "coefficient"` to
  compute regression coefficient as main effect of `arm` in the presence
  of covariates. It is also equivalent to `scale = "log odds ratio"` in
  the absence of covariates.
- Add unit tests for `fitLogistic`.

## TrialSimulator 0.82.0

### New Feature

- The function `fitCoxph` now supports `formula` to compute log hazard
  ratio or hazard ratio as main effect of `arm`. It will detect whether
  arm’s main effect is specified in formula. It allows covariates and
  interaction between covariates and arm. However, only the main effect
  of arm is tested and returned.
- Add unit tests for `fitCoxph`.

## TrialSimulator 0.81.0

### New Feature

- The function `fitLogistic` now supports computing log odds ratio, odds
  ratio, risk ratio, and risk difference using `emmeans` contrast, in
  the presence of covariates.
- Add unit tests for `fitLogistic` and `fitLinear`.

## TrialSimulator 0.80.0

### New Feature

- The function `fitLogistic` now supports `formula` with covariates, and
  uses `emmeans` contrast to compute average treatment effect (ATE) on
  the `logit` scale.

## TrialSimulator 0.79.0

### New Feature

- The function `fitLinear` now supports `formula` with covariates, and
  uses `emmeans` contrast to compute average treatment effect (ATE) on
  the mean scale.

## TrialSimulator 0.78.0

### Updates

- The `trigger_condition` in the function `milestone` is deprecated and
  is replaced with `when`. Note that `trigger_condition` is still
  supported in the `R6` class `Milestones`.

## TrialSimulator 0.77.0

### Updates

- Add case for unit test.

## TrialSimulator 0.76.0

### Bug Fixes

- Fix a bug that `trial$get_custom_data` throws an error when `n > 1` in
  `controller$run(n)` because custom data is wiped out in `trial$reset`.

## TrialSimulator 0.75.0

### Update

- Add vignette of defining arms.

## TrialSimulator 0.74.0

### New Feature

- Support inclusion criteria in `arm` through the `...` argument.

## TrialSimulator 0.73.0

### New Feature

- Print summary report of arms when printing an arm object in console or
  `rmarkdown`.

## TrialSimulator 0.72.0

### New Feature

- Print summary report of endpoints when printing an endpoint object in
  console or `rmarkdown`.

## TrialSimulator 0.70.0

### Updates

- Add vignette for longitudinal endpoints.

## TrialSimulator 0.68.0

### Updates

- Rename trial event as milestone. This a major update. Relevant codes
  and documents are updated accordingly.
- Add executable examples for
  [`controller()`](https://zhangh12.github.io/TrialSimulator/reference/controller.md)
  as per suggestion from CRAN team.

## TrialSimulator 0.67.0

### Updates

- Print event counts at trial events using
  [`message()`](https://rdrr.io/r/base/message.html) so that Shiny app
  can display it properly.

## TrialSimulator 0.66.0

### Updates

- Add cases for unit test.

## TrialSimulator 0.65.0

### New Features

- Add function `solveMixtureExponentialDistribution` to compute median
  of exponential endpoint of subgroup or the overall population.

## TrialSimulator 0.63.0

### New Features

- Add wrapper functions `endpoints`, `arm`, `trial`, `event`, `listener`
  and `controller` for `Endpoint$new`, `Arm$new`, `Trial$new`,
  `Event$new`, `Listener$new` and `Controller$new`.

## TrialSimulator 0.62.0

### New Features

- Allow extending trial duration with `Trial$set_duration`.

## TrialSimulator 0.61.0

### Updates

- Deprecate function `enroll_a_patient`. Use `enroll_patients` only.

## TrialSimulator 0.58.0

### Updates

- Update vignette of adaptive seamless design.

### Bug Fixes

- Fix a bug to use `n > 1` in `Controller$run` when an arm can possibly
  be removed adaptively during a trial.

### New Features

- Allow specifying arms in `enrollment`. This is useful to count
  randomized patients of all arms even if some are removed adaptively.

## TrialSimulator 0.57.0

### Updates

- Move vignette of comparison between `GraphicalTesting` and
  `graphicalMCP` to repository
  [TrialSimulatorDocuments](https://github.com/zhangh12/TrialSimulatorDocuments).

## TrialSimulator 0.56.0

### New Features

- `Controller$run` now can specify number of simulation replicates by
  newly added argument `n`. If `n` is greater than 1, simulation results
  can be accessed in `Controller$get_output()`.

## TrialSimulator 0.55.0

### Bug Fixes

- Fix a bug in `StaggeredRecruiter` to force the enrollment time of the
  first patient is zero. This is an known issue but I was too lazy to
  fix it. Earlier version may have overestimated time of events.

### New Features

- add function `fitFarringtonManning` of Farrington-Manning test for
  rate difference.

## TrialSimulator 0.54.0

### New Features

- Add function `Trial$bind` to row bind data frame in action functions.
  It is useful to prepare inputs of group sequential or graphical test.

## TrialSimulator 0.53.0

### New Features

- Add vignette of condition system.

## TrialSimulator 0.52.0

### Bug Fixes

- Fix a bug that data is not censored correctly at events. This bug does
  not affect a trial without interims.

## TrialSimulator 0.51.0

### New Features

- Add vignette of non-time-to-event endpoints.
- Add function `weibullDropout` to compute parameters of Weibull
  distribution when using it for dropout distribution.

## TrialSimulator 0.49.0

### New Features

- Add vignette of time-to-event endpoints.

## TrialSimulator 0.48.0

### Bug Fixes

- Fix a bug when alpha of a node in graph is set to rounding error bound
  `1e-5` while no alpha should have been propagated.

## TrialSimulator 0.47.0

### New Features

- Support new condition system for event triggering. Built-in functions
  `enrollment`, `eventNumber` and `calendarTime` can be combined with
  `&` and `|`. Nested combination is supported by using parentheses.
- `TriggerByEventNumbers` and `TriggerByCalendarTime` are therefore
  deprecated.

## TrialSimulator 0.44.0

### Bug Fixes

- Fix a bug when adding an arm that is already in the trial.

## TrialSimulator 0.43.0

### New Features

- Add function `fitLogistic` to fit logistic regression model.
- Support model fitting for multiple treatment arms in logistic
  regression, Cox PH model, and logrank test.

## TrialSimulator 0.42.0

### Minor Updates

- Add a logo.

## TrialSimulator 0.40.0

### Bug Fixes

- Revise examples for CRAN submission.

## TrialSimulator 0.39.0

### Bug Fixes

- Remove space in class name to eliminate R CMD check note.

## TrialSimulator 0.38.2

### Bug Fixes

- Throw error message when none of the hypotheses at test has non-zero
  alpha in graphical test.
- Update vignette.

## TrialSimulator 0.38.1

### New Features

- Plot stacked area chart for accumulative event numbers of endpoints.

### Bug Fixes

- Fix a bug when small weight (epsilon) is used in graph in graphical
  testing. Small weight can be conflict with integral tolerance error.

## TrialSimulator 0.37.0

### New Features

- Return more informative error message when custom random number
  generators are used to define endpoints. Specifically, it guides users
  to return columns for time-to-event endpoints properly.
- Update manual for `generator` in `Endpoint`.

## TrialSimulator 0.36.0

### Bug Fixes

- Fix a bug in `GraphicalTesting` when a hypothesis is tested multiple
  times at the same stage because more alpha is passed from other
  rejected hypothesis.

## TrialSimulator 0.35.0

### New Features

- Warn when incremental information is too low that can affect normality
  approximation of combination test.

### Bug Fixes

- Minor bugs fixed.

## TrialSimulator 0.33.0

### New Features

- Add README.

## TrialSimulator 0.33.0

### New Features

- Provide a default action function `do_nothing()` if users have no
  intent to do anything at a triggered event. This function can be
  passed to the argument `action` when creating a new event, e.g.,
  `Event$new(name = 'interim', trigger_condition = TriggerByCalendarTime, action = do_nothing, calendar_time = 64)`.

## TrialSimulator 0.30.0

### New Features

- Capture error inside `Controller$run()` and insert error message into
  output (see `Trial$get_output()$error_message`). It helps to integrate
  `TrialSimulator` with `targets`.

## TrialSimulator 0.29.0

#### New Features

- Support closed test based on inverse normal combination test.
- Seed can be accessed by `Trial$get_seed()`.

## TrialSimulator 0.28.0

#### New Features

- Support inverse normal combination test when multiple treatment arms
  present. Dunnett’s test is used for comparison.
- Specify random seed if user dose not pick one. Seed is saved into
  Trial’s output for reproducibility.

## TrialSimulator 0.27.0

#### New Features

- Support dry run for fixed design.

## TrialSimulator 0.26.0

#### New Features

- Adjust boundary at final analysis for over- or under-running trials.
- Support custom alpha spending function in graphical testing procedure.

## TrialSimulator 0.25.0

#### New Features

- Support inverse normal combination test for logrank statistics.

## TrialSimulator 0.24.0

#### New Features

- Update `GraphicalTesting` based on simplified interface of
  `GroupSequentialTest`.
