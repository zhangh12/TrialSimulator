
# TrialSimulator <img src="man/figures/logo.png" align="right" width="175" />

<!-- badges: start -->
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
<!-- badges: end -->

**&#x2695; No `for` loop in clinical trial simulation &#x2695;**

`TrialSimulator` is a system for declaratively implementing clinical trial simulations, inspired by the modular grammar of trial design. You provide the components—arms, endpoint-generation rules, milestones, and analysis methods—specify how they are combined under fixed or adaptive designs, and `TrialSimulator` takes care of the details, allowing you to focus on the design while it handles the mechanics.

`TrialSimulator` provides a suite of helper functions to support the full trial life cycle. 

- For **data generation**, it offers utilities to sample endpoints from common distributions. 
- For **adaptation**, it includes execution functions that modify trial settings *in place*. 
- For **analysis**, it implements methods such as regression models, group sequential tests, graphical multiple testing procedures, combination tests, and closed tests. These tools are entirely optional, but they make it simple to construct and evaluate complex trial designs without sacrificing flexibility.

## Shiny App

An online version of this app is hosted on [shinyapps.io](https://bx7ttm-han-zhang.shinyapps.io/trialsimulatorstarter/). 

## Validation

The validation documents for this R package are hosted [here](https://github.com/zhangh12/TrialSimulatorDocuments) and are continuously updated.

## Installation

### Release

You can install the current release version from `CRAN` with: 

``` r
install.packages("TrialSimulator")
```

### Development

You can install the development version from 
[GitHub](https://github.com/zhangh12/TrialSimulator) with:

``` r
if(!requireNamespace("remotes", quietly = TRUE)){
  install.packages("remotes")
}
remotes::install_github(
  "zhangh12/TrialSimulator", 
  dependencies = TRUE
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
    - [Special case: simulate correlated PFS and OS](https://zhangh12.github.io/TrialSimulator/articles/simulatePfsAndOs.html)
  - [Non-TTE endpoints](https://zhangh12.github.io/TrialSimulator/articles/defineNonTimeToEventEndpoints.html)
    - [Special case: longitudinal endpoints](https://zhangh12.github.io/TrialSimulator/articles/defineLongitudinalEndpoints.html)
  - [Define and summarize arms](https://zhangh12.github.io/TrialSimulator/articles/defineArms.html)
- Condition system for milestones
  - [Triggering trial milestones](https://zhangh12.github.io/TrialSimulator/articles/conditionSystem.html)
- Define actions for milestones
  - [Action function](https://zhangh12.github.io/TrialSimulator/articles/actionFunctions.html)
- Examples: fixed design
  - [Design with correlated endpoints, and custom data generator](https://zhangh12.github.io/TrialSimulator/articles/fixedDesign.html)
- Examples: adaptive design
  - [Seamless design with dose selection, interim, and multiple endpoints](https://zhangh12.github.io/TrialSimulator/articles/adaptiveDesign.html)
  - [Response-adaptive design](https://zhangh12.github.io/TrialSimulator/articles/responseAdaptive.html)
  - [Dose-ranging study](https://zhangh12.github.io/TrialSimulator/articles/doseRanging.html)
  - [Enrichment design] vignette is under development. 
  - [Platform trial] vignette is under development. 
  - [Basket trial] vignette is under development. 
- Built-in methods supported in `TrialSimulator`
  - [Wrapper functions of common statistical methods for estimating treatment effect](https://zhangh12.github.io/TrialSimulator/articles/wrappers.html)
  - [Group sequential test] See `?GroupSequentialTest`. 
  - [Graphical testing procedure] See `?GraphicalTesting`. 
  - [Combination test based on independent increment] extended from [Jorgens et al. 2019](https://onlinelibrary.wiley.com/doi/10.1002/pst.1926). Vignette is under development. 





