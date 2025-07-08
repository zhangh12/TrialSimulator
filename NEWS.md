# TrialSimulator 0.74.0

## New Feature

- Support inclusion criteria in `arm` through the `...` argument. 

# TrialSimulator 0.73.0

## New Feature

- Print summary report of arms when printing an arm object in console or `rmarkdown`. 

# TrialSimulator 0.72.0

## New Feature

- Print summary report of endpoints when printing an endpoint object in console or `rmarkdown`. 

# TrialSimulator 0.70.0

## Updates

- Add vignette for longitudinal endpoints. 

# TrialSimulator 0.68.0

## Updates

- Rename trial event as milestone. This a major update. Relevant codes and documents are updated accordingly. 
- Add executable examples for `controller()` as per suggestion from CRAN team. 

# TrialSimulator 0.67.0

## Updates

- Print event counts at trial events using `message()` so that Shiny app can display it properly. 

# TrialSimulator 0.66.0

## Updates

- Add cases for unit test. 

# TrialSimulator 0.65.0

## New Features

- Add function `solveMixtureExponentialDistribution` to compute median of exponential endpoint of subgroup or the overall population. 

# TrialSimulator 0.63.0

## New Features

- Add wrapper functions `endpoints`, `arm`, `trial`, `event`, `listener` and `controller` for `Endpoint$new`, `Arm$new`, `Trial$new`, `Event$new`, `Listener$new` and `Controller$new`.   

# TrialSimulator 0.62.0

## New Features

- Allow extending trial duration with `Trial$set_duration`. 

# TrialSimulator 0.61.0

## Updates

- Deprecate function `enroll_a_patient`. Use `enroll_patients` only. 

# TrialSimulator 0.58.0

## Updates

- Update vignette of adaptive seamless design. 

## Bug Fixes

- Fix a bug to use `n > 1` in `Controller$run` when an arm can possibly be removed adaptively during a trial. 

## New Features

- Allow specifying arms in `enrollment`. This is useful to count randomized patients of all arms even if some are removed adaptively. 

# TrialSimulator 0.57.0

## Updates

- Move vignette of comparison between `GraphicalTesting` and `graphicalMCP` to repository [TrialSimulatorDocuments](https://github.com/zhangh12/TrialSimulatorDocuments). 

# TrialSimulator 0.56.0

## New Features

- `Controller$run` now can specify number of simulation replicates by newly added argument `n`. If `n` is greater than 1, simulation results can be accessed in `Controller$get_output()`.  

# TrialSimulator 0.55.0

## Bug Fixes

- Fix a bug in `StaggeredRecruiter` to force the enrollment time of the first patient is zero. This is an known issue but I was too lazy to fix it. Earlier version may have overestimated time of events. 

## New Features

- add function `fitFarringtonManning` of Farrington-Manning test for rate difference. 

# TrialSimulator 0.54.0

## New Features

- Add function `Trial$bind` to row bind data frame in action functions. It is useful to prepare inputs of group sequential or graphical test. 

# TrialSimulator 0.53.0

## New Features

- Add vignette of condition system. 

# TrialSimulator 0.52.0

## Bug Fixes

- Fix a bug that data is not censored correctly at events. This bug does not affect a trial without interims. 

# TrialSimulator 0.51.0

## New Features

- Add vignette of non-time-to-event endpoints. 
- Add function `weibullDropout` to compute parameters of Weibull distribution when using it for dropout distribution. 

# TrialSimulator 0.49.0

## New Features

- Add vignette of time-to-event endpoints. 

# TrialSimulator 0.48.0

## Bug Fixes

- Fix a bug when alpha of a node in graph is set to rounding error bound `1e-5` while no alpha should have been propagated. 

# TrialSimulator 0.47.0

## New Features

- Support new condition system for event triggering. Built-in functions `enrollment`, `eventNumber` and `calendarTime` can be combined with `&` and `|`. Nested combination is supported by using parentheses. 
- `TriggerByEventNumbers` and `TriggerByCalendarTime` are therefore deprecated. 

# TrialSimulator 0.44.0

## Bug Fixes

- Fix a bug when adding an arm that is already in the trial. 

# TrialSimulator 0.43.0

## New Features

- Add function `fitLogistic` to fit logistic regression model.
- Support model fitting for multiple treatment arms in logistic regression, Cox PH model, and logrank test. 

# TrialSimulator 0.42.0

## Minor Updates

- Add a logo. 

# TrialSimulator 0.40.0

## Bug Fixes

- Revise examples for CRAN submission. 

# TrialSimulator 0.39.0

## Bug Fixes

- Remove space in class name to eliminate R CMD check note. 

# TrialSimulator 0.38.2

## Bug Fixes

- Throw error message when none of the hypotheses at test has non-zero alpha in graphical test. 
- Update vignette. 

# TrialSimulator 0.38.1

## New Features

- Plot stacked area chart for accumulative event numbers of endpoints. 

## Bug Fixes

- Fix a bug when small weight (epsilon) is used in graph in graphical testing. Small weight can be conflict with integral tolerance error. 

# TrialSimulator 0.37.0

## New Features

- Return more informative error message when custom random number generators are used to define endpoints. Specifically, it guides users to return columns for time-to-event endpoints properly. 
- Update manual for `generator` in `Endpoint`. 

# TrialSimulator 0.36.0

## Bug Fixes

- Fix a bug in `GraphicalTesting` when a hypothesis is tested multiple times at the same stage because more alpha is passed from other rejected hypothesis. 

# TrialSimulator 0.35.0

## New Features

- Warn when incremental information is too low that can affect normality approximation of combination test. 

## Bug Fixes

- Minor bugs fixed. 

# TrialSimulator 0.33.0

## New Features

- Add README. 

# TrialSimulator 0.33.0

## New Features

- Provide a default action function `do_nothing()` if users have no intent to do anything at a triggered event. This function can be passed to the argument `action` when creating a new event, e.g., `Event$new(name = 'interim', trigger_condition = TriggerByCalendarTime, action = do_nothing, calendar_time = 64)`. 

# TrialSimulator 0.30.0

## New Features

- Capture error inside `Controller$run()` and insert error message into output (see `Trial$get_output()$error_message`). It helps to integrate `TrialSimulator` with `targets`. 

# TrialSimulator 0.29.0

### New Features

- Support closed test based on inverse normal combination test. 
- Seed can be accessed by `Trial$get_seed()`. 


# TrialSimulator 0.28.0

### New Features

- Support inverse normal combination test when multiple treatment arms present. Dunnett's test is used for comparison. 
- Specify random seed if user dose not pick one. Seed is saved into Trial's output for reproducibility. 

# TrialSimulator 0.27.0

### New Features

- Support dry run for fixed design. 

# TrialSimulator 0.26.0

### New Features

- Adjust boundary at final analysis for over- or under-running trials.
- Support custom alpha spending function in graphical testing procedure. 

# TrialSimulator 0.25.0

### New Features

- Support inverse normal combination test for logrank statistics. 

# TrialSimulator 0.24.0

### New Features

- Update `GraphicalTesting` based on simplified interface of `GroupSequentialTest`. 

