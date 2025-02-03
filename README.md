
# TrialSimulator

<!-- badges: start -->
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
<!-- badges: end -->

The goal of `TrialSimulator` is to provide a pipeline for implementing simulation of clinical trial more efficiently and reliably. 
It follows principle of modularity to isolate codes of statistical testing from data generation and management. 
It provides a set of tools to sample endpoints of common distributions, manage trial data, and summarize simulation results under fixed or adaptive designs. 
It also provides functions of group sequential design, graphical testing procedure, combination test, and closed test that are widely used in analyze complex trial designs. 

## Installation

You can install the development version of TrialSimulator from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("zhangh12/TrialSimulator")
```


