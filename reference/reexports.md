# Objects re-exported from the survival package

These are re-exported so that user-supplied formulas in action functions
(e.g. `Surv(time, event) ~ arm` or `... + strata(site)`) resolve
correctly after
[`library(TrialSimulator)`](https://zhangh12.github.io/TrialSimulator/)
alone, without requiring
[`library(survival)`](https://github.com/therneau/survival) or the
`survival::` prefix. This is particularly relevant for parallel runs
(`n_workers > 1`), where each worker only attaches TrialSimulator.
