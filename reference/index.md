# Package index

## Define a trial

Building blocks of a simulated trial.

- [`trial()`](https://zhangh12.github.io/TrialSimulator/reference/trial.md)
  : Define a Trial
- [`arm()`](https://zhangh12.github.io/TrialSimulator/reference/arm.md)
  : Define an Arm
- [`endpoint()`](https://zhangh12.github.io/TrialSimulator/reference/endpoint.md)
  : Define Endpoints
- [`regimen()`](https://zhangh12.github.io/TrialSimulator/reference/regimen.md)
  : Define a Regimen
- [`StaggeredRecruiter()`](https://zhangh12.github.io/TrialSimulator/reference/StaggeredRecruiter.md)
  : Generate Enrollment Time from Piecewise Constant Uniform
  Distribution

## Milestones and triggering conditions

Register milestones and the conditions that trigger them.

- [`milestone()`](https://zhangh12.github.io/TrialSimulator/reference/milestone.md)
  : Define a Milestone
- [`listener()`](https://zhangh12.github.io/TrialSimulator/reference/listener.md)
  : Define a Listener
- [`controller()`](https://zhangh12.github.io/TrialSimulator/reference/controller.md)
  : Define a Controller
- [`calendarTime()`](https://zhangh12.github.io/TrialSimulator/reference/calendarTime.md)
  : Triggering Condition by Calendar Time
- [`enrollment()`](https://zhangh12.github.io/TrialSimulator/reference/enrollment.md)
  : Triggering Condition by Number of Randomized Patients
- [`eventNumber()`](https://zhangh12.github.io/TrialSimulator/reference/eventNumber.md)
  : Triggering Condition by Number of Events or Non-missing Observations
  of an Endpoint
- [`doNothing()`](https://zhangh12.github.io/TrialSimulator/reference/doNothing.md)
  : An Action Function that Does Nothing

## Adaptations

Methods called within action functions to adapt an ongoing trial.

- [`add_arms()`](https://zhangh12.github.io/TrialSimulator/reference/add_arms.md)
  : Adding One or More Arms to a Trial
- [`remove_arms()`](https://zhangh12.github.io/TrialSimulator/reference/remove_arms.md)
  : Removing One or More Arms From a Trial
- [`resize()`](https://zhangh12.github.io/TrialSimulator/reference/resize.md)
  : Resizing a Trial
- [`set_duration()`](https://zhangh12.github.io/TrialSimulator/reference/set_duration.md)
  : Extending Duration of a Trial
- [`update_sample_ratio()`](https://zhangh12.github.io/TrialSimulator/reference/update_sample_ratio.md)
  : Updating Sampling Ratios of Existing Arms in a Trial
- [`update_generator()`](https://zhangh12.github.io/TrialSimulator/reference/update_generator.md)
  : Updating Data Generators of One or More Endpoints in an Arm
- [`crossover()`](https://zhangh12.github.io/TrialSimulator/reference/crossover.md)
  : Crossover at a Milestone
- [`stop_followup()`](https://zhangh12.github.io/TrialSimulator/reference/stop_followup.md)
  : Stopping Follow-Up of Selected Patients in a Trial
- [`update_accrual_rate()`](https://zhangh12.github.io/TrialSimulator/reference/update_accrual_rate.md)
  : Updating the Accrual Rate of a Trial at a Milestone

## Statistical analysis

Model fitting and multiplicity-controlled testing.

- [`fitCoxph()`](https://zhangh12.github.io/TrialSimulator/reference/fitCoxph.md)
  : Fit Cox Proportional Hazard Ratio model
- [`fitLogrank()`](https://zhangh12.github.io/TrialSimulator/reference/fitLogrank.md)
  : Carry out log rank test
- [`fitLinear()`](https://zhangh12.github.io/TrialSimulator/reference/fitLinear.md)
  : Fit linear regression model
- [`fitLogistic()`](https://zhangh12.github.io/TrialSimulator/reference/fitLogistic.md)
  : Fit logistic regression model
- [`fitFarringtonManning()`](https://zhangh12.github.io/TrialSimulator/reference/fitFarringtonManning.md)
  : Farrington-Manning test for rate difference
- [`GraphicalTesting`](https://zhangh12.github.io/TrialSimulator/reference/GraphicalTesting.md)
  : Class of GraphicalTesting
- [`GroupSequentialTest`](https://zhangh12.github.io/TrialSimulator/reference/GroupSequentialTest.md)
  : Class of GroupSequentialTest

## Data generators and distribution helpers

- [`DynamicRNGFunction()`](https://zhangh12.github.io/TrialSimulator/reference/DynamicRNGFunction.md)
  : A wrapper of random number generator.
- [`PiecewiseConstantExponentialRNG()`](https://zhangh12.github.io/TrialSimulator/reference/PiecewiseConstantExponentialRNG.md)
  : Generate Time-to-Event Endpoint from Piecewise Constant Exponential
  Distribution
- [`CorrelatedPfsAndOs2()`](https://zhangh12.github.io/TrialSimulator/reference/CorrelatedPfsAndOs2.md)
  : Generate Correlated PFS and OS Using Gumbel Copula
- [`CorrelatedPfsAndOs3()`](https://zhangh12.github.io/TrialSimulator/reference/CorrelatedPfsAndOs3.md)
  : Generate Correlated PFS and OS Using the Three-States Illness-Death
  Model
- [`CorrelatedPfsAndOs4()`](https://zhangh12.github.io/TrialSimulator/reference/CorrelatedPfsAndOs4.md)
  : Generate Correlated PFS, OS and Objective Response
- [`rconst()`](https://zhangh12.github.io/TrialSimulator/reference/rconst.md)
  : Generate Constant Variable
- [`qPiecewiseExponential()`](https://zhangh12.github.io/TrialSimulator/reference/qPiecewiseExponential.md)
  : Quantile Function of Piecewise Exponential Distribution
- [`solveMixtureExponentialDistribution()`](https://zhangh12.github.io/TrialSimulator/reference/solveMixtureExponentialDistribution.md)
  : Solve Parameters in a Mixture Exponential Distribution
- [`solvePiecewiseConstantExponentialDistribution()`](https://zhangh12.github.io/TrialSimulator/reference/solvePiecewiseConstantExponentialDistribution.md)
  : Compute Constant Rates of Piecewise Exponential Distribution
- [`solveThreeStateModel()`](https://zhangh12.github.io/TrialSimulator/reference/solveThreeStateModel.md)
  : Solve Parameters in a Three-State Illness-death Model
- [`weibullDropout()`](https://zhangh12.github.io/TrialSimulator/reference/weibullDropout.md)
  : Calculate Parameters of Weibull Distribution as a Dropout Method

## Summaries and utilities

- [`summarizeDataFrame()`](https://zhangh12.github.io/TrialSimulator/reference/summarizeDataFrame.md)
  : Summarize A Data Frame
- [`summarizeMilestoneTime()`](https://zhangh12.github.io/TrialSimulator/reference/summarizeMilestoneTime.md)
  : Summary of Milestone Time from Simulated Trials
- [`plot(`*`<milestone_time_summary>`*`)`](https://zhangh12.github.io/TrialSimulator/reference/plot.milestone_time_summary.md)
  : Plot Triggering Time of Milestones in Simulated Trials
- [`plot(`*`<three_state_model>`*`)`](https://zhangh12.github.io/TrialSimulator/reference/plot.three_state_model.md)
  : Plot result of three-state illness-death model
- [`expandRegimen()`](https://zhangh12.github.io/TrialSimulator/reference/expandRegimen.md)
  : Expand regimen trajectory into long format

## R6 classes

Underlying classes; most users interact through the wrappers above.

- [`Trials`](https://zhangh12.github.io/TrialSimulator/reference/Trials.md)
  : Class of Trial
- [`Arms`](https://zhangh12.github.io/TrialSimulator/reference/Arms.md)
  : Class of Arm
- [`Endpoints`](https://zhangh12.github.io/TrialSimulator/reference/Endpoints.md)
  : Class of Endpoint
- [`Milestones`](https://zhangh12.github.io/TrialSimulator/reference/Milestones.md)
  : Class of Milestones
- [`Listeners`](https://zhangh12.github.io/TrialSimulator/reference/Listeners.md)
  : Class of Listener
- [`Controllers`](https://zhangh12.github.io/TrialSimulator/reference/Controllers.md)
  : Class of Controller
- [`Regimens`](https://zhangh12.github.io/TrialSimulator/reference/Regimens.md)
  : Class of Regimens
