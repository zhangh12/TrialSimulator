
# TrialSimulator <img src="man/figures/logo.png" align="right" width="175" />

<!-- badges: start -->
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
<!-- badges: end -->

**&#x2695; No `for` loop in clinical trial simulation &#x2695;**

The goal of `TrialSimulator` is to provide a pipeline for implementing simulation of clinical trial more efficiently and reliably. 
It follows principle of modularity to isolate codes of statistical testing from data generation and management. 
It provides a set of tools to sample endpoints of common or custom distributions, manage trial data, and summarize simulation results under fixed or adaptive designs. 
It also provides functions of group sequential design, graphical testing procedure, combination test, and closed test that are widely used in analyzing complex trial designs. 

## Shiny App

An online version of this app is hosted on [shinyapps.io](https://bx7ttm-han-zhang.shinyapps.io/trialsimulatorstarter/). 

## Validation

The validation documents for this R package are hosted [here](https://github.com/zhangh12/TrialSimulatorDocuments) and are continuously updated.

## Installation

You can install the development version of `TrialSimulator` from [GitHub](https://github.com/zhangh12/TrialSimulator) with:

``` r
if (!require("remotes")) {
  install.packages("remotes")
}
remotes::install_github(
  "zhangh12/TrialSimulator", 
  build_manual = TRUE, 
  build_vignettes = TRUE, 
  force = TRUE
)
```

## Getting Started

`TrialSimulator` is designed with a modular architecture, allowing users to systematically construct and simulate clinical trials in a structured and flexible manner. The simulation process is broken down into three key steps:

1. **Defining Endpoints**: Specify the treatment endpoints that will be evaluated throughout the trial.
2. **Specifying Milestone**: Establish the conditions under which specific milestones (e.g., interim analysis, futility assessment, or stopping rules) will be triggered.
3. **Defining Actions for Milestones**: Determine the appropriate actions to be taken once a milestone is triggered, such as adjusting enrollment, terminating treatment arms, or conducting statistical analysis.

To efficiently utilize `TrialSimulator` for clinical trial simulations, we recommend reading the following vignettes in order:

- Define endpoints and arms
  - [Time-to-event (TTE) endpoints](https://zhangh12.github.io/TrialSimulator/articles/defineTimeToEventEndpoints.html)
  - [Non-TTE endpoints](https://zhangh12.github.io/TrialSimulator/articles/defineNonTimeToEventEndpoints.html)
  - [Longitudinal endpoints](https://zhangh12.github.io/TrialSimulator/articles/defineLongitudinalEndpoints.html)
  - [Define and summarize arms](https://zhangh12.github.io/TrialSimulator/articles/defineArms.html)
  - [Simulate correlated PFS and OS](https://zhangh12.github.io/TrialSimulator/articles/simulatePfsAndOs.html)
- Condition system
  - [Triggering trial milestones](https://zhangh12.github.io/TrialSimulator/articles/conditionSystem.html)
- Define actions for milestones
  - [Action function]
- Examples: fixed design
  - [Design with correlated endpoints, and custom data generator](https://zhangh12.github.io/TrialSimulator/articles/fixedDesign.html)
- Examples: adaptive design
  - [Design with dose selection, interim, and multiple endpoints](https://zhangh12.github.io/TrialSimulator/articles/adaptiveDesign.html)
- Built-in methods supported in `TrialSimulator`
  - [The logrank test]
  - [Cox proportional hazard model]
  - [Linear regression model]
  - [Logistic regression model]
  - [Group sequential test]
  - [Graphical testing procedure]
  - [Combination test based on independent increment]





