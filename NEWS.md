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

