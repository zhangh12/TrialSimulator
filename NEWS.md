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

