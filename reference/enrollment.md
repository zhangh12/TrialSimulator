# Triggering Condition by Number of Randomized Patients

Define a condition to trigger trial milestone by the number of
randomized patients. The milestone will be triggered when a trial has
enrolled at least the specified number of patients. It can be used
combined with conditions specified by
[calendarTime](https://zhangh12.github.io/TrialSimulator/reference/calendarTime.md)
and
[eventNumber](https://zhangh12.github.io/TrialSimulator/reference/eventNumber.md).

Refer to the
[vignette](https://zhangh12.github.io/TrialSimulator/articles/conditionSystem.html)
to learn how to define milestones when performing simulation using
`TrialSimulator`.

## Usage

``` r
enrollment(n, ..., arms = NULL, min_treatment_duration = 0)
```

## Arguments

- n:

  integer. Number of randomized patients.

- ...:

  subset conditions compatible with
  [`dplyr::filter`](https://dplyr.tidyverse.org/reference/filter.html).
  Number of randomized patients will be counted on subset of trial data
  only.

- arms:

  vector of character. Name of arms on which the number of patients is
  counted. If `NULL`, use all arms that are not yet removed from the
  trial by the time of calculation.

- min_treatment_duration:

  numeric. Zero or positive value. minimum treatment duration of
  enrolled patients. Default is 0, i.e., looking for triggering time
  based on number of enrolled patients in population specified by `...`
  and `arms`. If positive, it means that milestone is triggered when a
  specific number of enrolled patients have received treatment for at
  least `min_treatment_duration` duration. It is users' responsibility
  to assure that the unit of `min_treatment_duration` are consistent
  with readout of non-tte endpoints, dropout time, and trial duration.

## Value

an object of class \`Condition\`

## Examples

``` r
## ensure sufficient sample size of whole trial
enrollment(n = 100)
#> Number of randomized patients >=  100

## ensure sufficient sample size in sub-group of interest
enrollment(n = 100, biomarker1 == 'positive' & biomarker2 == 'high')
#> Number of randomized patients >=  100 with conditions: ~biomarker1 == "positive" & biomarker2 == "high"

## ensure sufficient sample size in high dose + placebo
enrollment(n = 1000, arms = c('high dose', 'placebo'))
#> Number of randomized patients >=  1000 in arms < high dose, placebo > 
#> 

## ensure sufficient treatment duration
enrollment(n = 500, min_treatment_duration = 2)
#> Number of randomized patients >=  500 and all enrolled patients have been treated for < 2 > (unit time) 
#> 

```
