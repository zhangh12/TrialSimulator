# An Example of Simulating a Trial with Adaptive Enrichment Design

In this vignette, we walk through the simulation of a two-stage adaptive
enrichment design with `TrialSimulator`. The idea behind the design is
simple. A baseline biomarker splits the patients into a
biomarker-positive subgroup and the rest, and we suspect that the
treatment works mainly in the subgroup. At an interim analysis, we
compute the conditional power for the full population and for the
subgroup, and the pair of numbers tells us what to do next: stop for
futility, carry on as planned, enroll biomarker-positive patients only
from then on (enrichment), or enlarge the trial (promising zone). The
two stages are analyzed separately and combined with the inverse normal
method, and a closed test with Simes’ test for the intersection
hypothesis keeps the family-wise error rate under control across the
full-population and subgroup hypotheses.

Most of the building blocks appear in other vignettes. Conditional power
and event number reassessment are explained in [Conditional power and
event number
reassessment](https://zhangh12.github.io/TrialSimulator/articles/conditionalPower.md),
and the adaptation functions in [Action
function](https://zhangh12.github.io/TrialSimulator/articles/actionFunctions.md).
Here we put them together.

## Simulation Settings

- The trial has a placebo arm (`pbo`) and a treatment arm (`trt`),
  randomized `1:1`.
- A binary baseline `biomarker` is measured at randomization, with a
  prevalence of 40%. Patients with `biomarker = 1` form the subgroup of
  interest.
- The only efficacy endpoint is progression-free survival (`PFS`). It is
  exponential, with a median that depends on the arm and on the
  biomarker:
  - 6 months in the placebo arm, whatever the biomarker;
  - 6.5 months in biomarker-negative patients of the treatment arm
    (hazard ratio 0.92);
  - 10 months in biomarker-positive patients of the treatment arm
    (hazard ratio 0.6).
- Accrual is 6 patients per month for the first 6 months, then 12
  patients per month. The planned sample size is 320 patients, and it
  can grow adaptively.
- Dropout is exponential, with 5% of patients lost by month 12.
- The trial runs in two stages. The first 80 randomized patients form
  the stage 1 cohort, everybody randomized afterwards belongs to the
  stage 2 cohort. The two cohorts are analyzed separately and their
  p-values are combined with the inverse normal method, using the
  pre-specified weights $`w_1 = \sqrt{65/240}`$ and
  $`w_2 = \sqrt{175/240}`$, i.e., the planned split of the 240 events
  between the stages. Stage 1 is deliberately small: its job is to
  inform the interim decision, and most of the information comes from
  stage 2, after the adaptation.
- As planned, the final analysis takes place when 240 `PFS` events have
  been observed in the whole trial and the stage 1 cohort has reached 65
  events. Keep in mind that this is only the plan; the condition is
  replaced when the trial is adapted at the interim, as described next.
- The interim decision is made when 45 `PFS` events have been observed
  in the stage 1 cohort. Let $`CP_{full}`$ be the conditional power for
  the full population given the planned 240 events, and $`CP_{sub}`$ the
  conditional power for the subgroup given 150 events in the subgroup.
  Both are computed under the trend observed at the interim, at the
  one-sided level $`\alpha = 0.025`$ of the final test. The five
  decision zones are
  - *futility* if $`CP_{full} < 0.01`$ and $`CP_{sub} < 0.01`$: the
    trial stops at the interim and neither hypothesis is rejected. We
    picked this cutoff so that, under the alternative described above,
    about 8% of the trials stop for futility;
  - *unfavorable* if $`CP_{full} < 0.2`$ and $`CP_{sub} < 0.6`$ but not
    in the futility zone: the trial continues as planned;
  - *enrichment* if $`CP_{full} < 0.2`$ and $`CP_{sub} \ge 0.6`$: from
    now on only biomarker-positive patients are enrolled, the accrual
    rate drops to 5 patients per month because of screening, up to 280
    biomarker-positive patients are enrolled in total, and the number of
    events in the subgroup at the final analysis is re-estimated to
    reach a conditional power of 0.95, with a cap of 200 events and a
    floor of 50 events;
  - *promising* if $`0.2 \le CP_{full} < 0.9`$: the sample size grows to
    480 patients and the total number of events at the final analysis is
    re-estimated to reach a conditional power of 0.95, with a cap of 400
    events;
  - *favorable* if $`CP_{full} \ge 0.9`$: the trial continues as
    planned.
- The stage 1 analysis takes place when 65 `PFS` events have been
  observed in the stage 1 cohort. Follow-up of that cohort stops there,
  which makes the stage 2 data independent of the stage 1 data. We
  compute one-sided logrank p-values for the full population
  ($`p_{F,1}`$) and for the subgroup ($`p_{S,1}`$), and Simes’ p-value
  $`p_{FS,1} = \min\{2\min(p_{F,1}, p_{S,1}), \max(p_{F,1}, p_{S,1})\}`$
  for the intersection hypothesis.
- At the final analysis, the same three p-values are computed from the
  stage 2 cohort. Under enrichment no biomarker-negative patient joins
  stage 2, so the full-population hypothesis cannot be tested there: its
  stage 2 p-value is set to 1 and the intersection p-value is just the
  subgroup p-value.
- Each of the three p-values is combined across the stages with the
  inverse normal method. The closed test rejects the full-population
  hypothesis if the combined intersection and full-population p-values
  are both at most 0.025 (one-sided), and rejects the subgroup
  hypothesis if the combined intersection and subgroup p-values are both
  at most 0.025.

## Define Two Arms

Since the distribution of `PFS` depends on the biomarker, we generate
the two together with a custom generator. A generator returns a data
frame with one column per endpoint. For a time-to-event endpoint, an
extra column with the suffix `_event` holds the event indicator. It is
always 1 here because the generator produces event times; censoring is
handled by `TrialSimulator` at data locks, at adaptations and through
dropout. The biomarker is declared with `type = 'baseline'`, a
non-time-to-event endpoint observed at randomization, so it needs no
readout time.

``` r

rng <- function(n, median0, median1, prevalence){
  biomarker <- rbinom(n, size = 1, prob = prevalence)
  pfs <- rexp(n, rate = log(2) / ifelse(biomarker == 1, median1, median0))
  data.frame(biomarker = biomarker, pfs = pfs, pfs_event = 1)
}
```

One call of
[`endpoint()`](https://zhangh12.github.io/TrialSimulator/reference/endpoint.md)
gives each arm both of its endpoints. We will meet these generators
again at enrichment, when they are swapped for versions that only
produce biomarker-positive patients.

``` r

pbo_endpoints <- endpoint(name = c('biomarker', 'pfs'),
                          type = c('baseline', 'tte'),
                          generator = rng, median0 = 6, median1 = 6,
                          prevalence = .4)
pbo <- arm(name = 'pbo')
pbo$add_endpoints(pbo_endpoints)

trt_endpoints <- endpoint(name = c('biomarker', 'pfs'),
                          type = c('baseline', 'tte'),
                          generator = rng, median0 = 6.5, median1 = 10,
                          prevalence = .4)
trt <- arm(name = 'trt')
trt$add_endpoints(trt_endpoints)
```

## Define a Trial

The trial starts with 320 planned patients and a piecewise constant
accrual rate. Note that there is no end date here: the trial ends when
its last milestone is triggered, and the milestones are defined below.
We fix a seed so that the single run shown later can be reproduced. In a
real simulation study, leave `seed = NULL` and let `TrialSimulator` pick
and record a seed for every replicate.

``` r

accrual_rate <- data.frame(end_time = c(6, Inf), piecewise_rate = c(6, 12))
trial <- trial(name = 'enrichment', n_patients = 320, seed = 97,
               enroller = StaggeredRecruiter, accrual_rate = accrual_rate,
               dropout = rexp, rate = -log(1 - .05) / 12, ## 5% by month 12
               silent = TRUE)
trial$add_arms(sample_ratio = c(1, 1), pbo, trt)
```

## Define Trial Milestones and Action Functions

The trial has three milestones, named `interim`, `stage 1 analysis` and
`final`, and all three are event-driven. The conditions of `interim` and
`stage 1 analysis` count events in the stage 1 cohort only, through the
subset condition `patient_id <= 80`. Triggering conditions like these,
and the way they can be filtered and combined with `&` and `|`, are
described in the vignette [Condition System for Triggering Milestones in
a
Trial](https://zhangh12.github.io/TrialSimulator/articles/conditionSystem.md).

The condition of `final` deserves a closer look. It asks for 240 events
in the whole trial, as planned, but also for 65 events in the stage 1
cohort, which is the condition of `stage 1 analysis`. The second part
looks redundant, since 65 events in a cohort of 80 patients will
normally be reached long before 240 events in the whole trial. It is
there for a different reason: `TrialSimulator` triggers milestones in
the order in which they are registered, and a milestone whose condition
is met before an earlier one raises an error. Repeating the stage 1
condition inside `final` guarantees that `final` can never fire before
`stage 1 analysis`, whatever the accrual pattern in a particular
replicate. We keep this part in every condition the interim action
installs later, for the same reason. The 240 events, on the other hand,
are only the plan; the action function of `interim` replaces that part
whenever the trial is adapted.

Every milestone comes with an action function, which we write in the
following subsections; see the vignette [Action
function](https://zhangh12.github.io/TrialSimulator/articles/actionFunctions.md)
for what an action function can do. The milestone calls below are only
displayed at this point; they are run once the action functions exist.

``` r

interim <- milestone(name = 'interim', action = interim_action,
                     when = eventNumber(endpoint = 'pfs', n = 45, patient_id <= 80))
stage1 <- milestone(name = 'stage 1 analysis', action = stage1_action,
                    when = eventNumber(endpoint = 'pfs', n = 65, patient_id <= 80))
final <- milestone(name = 'final', action = final_action,
                   when = eventNumber(endpoint = 'pfs', n = 240) &
                     eventNumber(endpoint = 'pfs', n = 65, patient_id <= 80))

listener <- listener(silent = TRUE)
listener$add_milestones(interim, stage1, final)
```

### Interim decision

The action function of `interim` starts by computing the two conditional
powers with `trial$conditionalPower()`. The subset condition
`patient_id <= 80` restricts the calculation to the stage 1 cohort, and
`biomarker == 1` narrows it further to the subgroup. The argument `D` is
the number of events we expect at the final analysis, `alpha` is the
one-sided level of the final test, and `effect = 'trend'` means the
hazard ratio observed at the interim is carried forward.

What happens next depends on the zone. In the futility zone the trial
stops. `TrialSimulator` has no dedicated method for stopping a trial
early, because a trial simply ends when its last milestone is triggered.
So we stop the trial with `trial$update_milestone()`, moving the two
remaining milestones to the current calendar time. Both are then
triggered right after the interim, enrollment stops there, and their
action functions, written below, skip the analyses when they see a
futility decision.

In the enrichment zone we make four adaptations:

- `trial$eventNumberReestimationFromConditionalPower()` finds the
  smallest number of events in the subgroup that reaches the target
  conditional power, subject to the cap `D_cap`;
- `trial$resize()` raises the maximum sample size so that 280
  biomarker-positive patients are enrolled in total;
- `trial$update_accrual_rate()` slows accrual down, because screening
  for the biomarker takes time;
- `trial$update_generator()` replaces the generators of both arms by the
  same generator with `prevalence = 1`, so that every patient enrolled
  from now on is biomarker-positive.

Finally, `trial$update_milestone()` rewrites the triggering condition of
the final analysis around the re-estimated number of events in the
subgroup. In the promising zone, only the sample size and the total
number of events change. Either way, the new condition keeps asking for
65 events in the stage 1 cohort, which guarantees that the stage 1
analysis always comes before the final analysis.

``` r

interim_action <- function(trial){

  locked_data <- trial$get_locked_data('interim')

  cp_full <- trial$conditionalPower(
    milestone = 'interim', Surv(pfs, pfs_event) ~ arm, placebo = 'pbo',
    alternative = 'less', alpha = .025, D = 240, effect = 'trend',
    patient_id <= 80)$cp

  cp_sub <- trial$conditionalPower(
    milestone = 'interim', Surv(pfs, pfs_event) ~ arm, placebo = 'pbo',
    alternative = 'less', alpha = .025, D = 150, effect = 'trend',
    patient_id <= 80 & biomarker == 1)$cp

  decision <- if(cp_full < .01 && cp_sub < .01){
    'futility'
  }else if(cp_full < .2 && cp_sub < .6){
    'unfavorable'
  }else if(cp_full < .2){
    'enrichment'
  }else if(cp_full < .9){
    'promising'
  }else{
    'favorable'
  }

  if(decision == 'futility'){

    ## stop the trial now: both remaining milestones are triggered at the
    ## current time, and their action functions skip the analyses
    now <- trial$get_current_time()
    trial$update_milestone(name = 'stage 1 analysis',
                           when = calendarTime(time = now))
    trial$update_milestone(name = 'final',
                           when = calendarTime(time = now))

  }else if(decision == 'enrichment'){

    en <- trial$eventNumberReestimationFromConditionalPower(
      milestone = 'interim', Surv(pfs, pfs_event) ~ arm, placebo = 'pbo',
      alternative = 'less', alpha = .025, target_cp = .95, effect = 'trend',
      patient_id <= 80 & biomarker == 1, D_cap = 200)

    max_events_in_subgroup <- ifelse(en$target_reached, en$D, 200)
    max_patients_in_subgroup <- 280
    enrolled_in_subgroup <- sum(locked_data$biomarker)

    trial$resize(n_patients = nrow(locked_data) +
                   max_patients_in_subgroup - enrolled_in_subgroup)
    trial$update_accrual_rate(
      accrual_rate = data.frame(end_time = Inf, piecewise_rate = 5))

    trial$update_generator(
      arm_name = 'pbo', endpoint_name = c('pfs', 'biomarker'),
      generator = rng, median0 = 6, median1 = 6, prevalence = 1.0)
    trial$update_generator(
      arm_name = 'trt', endpoint_name = c('pfs', 'biomarker'),
      generator = rng, median0 = 6.5, median1 = 10, prevalence = 1.0)

    trial$update_milestone(
      name = 'final',
      when = eventNumber(endpoint = 'pfs', n = max(max_events_in_subgroup, 50),
                         biomarker == 1) &
        eventNumber(endpoint = 'pfs', n = 65, patient_id <= 80))

  }else if(decision == 'promising'){

    en <- trial$eventNumberReestimationFromConditionalPower(
      milestone = 'interim', Surv(pfs, pfs_event) ~ arm, placebo = 'pbo',
      alternative = 'less', alpha = .025, target_cp = .95, effect = 'trend',
      patient_id <= 80, D_cap = 400)

    max_events <- ifelse(en$target_reached, en$D, 400)

    trial$resize(n_patients = 480)
    trial$update_milestone(
      name = 'final',
      when = eventNumber(endpoint = 'pfs', n = max_events) &
        eventNumber(endpoint = 'pfs', n = 65, patient_id <= 80))

  }
  ## 'unfavorable' and 'favorable': no adaptation

  trial$save(value = decision, name = 'decision')
  trial$save(value = cp_full, name = 'cp_full')
  trial$save(value = cp_sub, name = 'cp_subgroup')
}
```

### Stage 1 analysis

The first thing the action function of `stage 1 analysis` does is to
call `trial$stop_followup(patient_id <= 80, additional_followup = 0)`,
which freezes the stage 1 cohort at the milestone time. From now on
these patients look the same in every data lock, and that is exactly
what makes the stage 2 p-values independent of the stage 1 p-values. The
three stage 1 p-values are then saved with `trial$save()`, ready to be
picked up at the final analysis. Note that
[`fitLogrank()`](https://zhangh12.github.io/TrialSimulator/reference/fitLogrank.md)
accepts subset conditions such as `patient_id <= 80` too.

If the trial has already stopped for futility, the action simply returns
without saving anything. **There is no need to save `NA` placeholders to
keep the output rectangular** — this is a feature of `TrialSimulator`
working for you: when the outputs of the replicates are combined, a
column that a replicate never saved is filled with `NA` in that
replicate’s row automatically. An action function only records what its
replicate actually computed, and the summary still sees the same columns
in every row.

``` r

stage1_action <- function(trial){

  if(trial$get_output('decision') == 'futility'){
    return(invisible(NULL))
  }

  trial$stop_followup(patient_id <= 80, additional_followup = 0)

  locked_data <- trial$get_locked_data('stage 1 analysis')

  pf1 <- fitLogrank(Surv(pfs, pfs_event) ~ arm, placebo = 'pbo',
                    data = locked_data, alternative = 'less',
                    patient_id <= 80)$p
  ps1 <- fitLogrank(Surv(pfs, pfs_event) ~ arm, placebo = 'pbo',
                    data = locked_data, alternative = 'less',
                    patient_id <= 80 & biomarker == 1)$p

  trial$save(value = pf1, name = 'pf1')
  trial$save(value = ps1, name = 'ps1')
  trial$save(value = min(2 * min(pf1, ps1), max(pf1, ps1)), name = 'simes1')
}
```

### Final analysis

At the final analysis, we fetch the values saved earlier in the same
replicate with `trial$get_output()`, and compute the stage 2 p-values
from the stage 2 cohort only (`patient_id > 80`). After that, the
combination and the closed test are a few lines of arithmetic.

Under futility the action again returns early without saving `NA`
placeholders for the stage 2 p-values — missing columns are filled with
`NA` automatically, as explained at the stage 1 analysis. **The two
rejection indicators are a different matter**: a trial stopped for
futility rejects nothing, so `FALSE` is their true value, not a
placeholder. Saving `FALSE` explicitly is what keeps `mean(reject_full)`
in the summary an *unconditional* rejection rate. Had these two columns
been left `NA`, the rates would silently have been computed among
continuing trials only.

``` r

final_action <- function(trial){

  locked_data <- trial$get_locked_data('final')
  decision <- trial$get_output('decision')

  trial$save(value = sum(locked_data$biomarker), name = 'n_positive')

  if(decision == 'futility'){
    trial$save(value = FALSE, name = 'reject_full')
    trial$save(value = FALSE, name = 'reject_subgroup')
    return(invisible(NULL))
  }

  pf1 <- trial$get_output('pf1')
  ps1 <- trial$get_output('ps1')
  simes1 <- trial$get_output('simes1')

  if(decision == 'enrichment'){
    ps2 <- fitLogrank(Surv(pfs, pfs_event) ~ arm, placebo = 'pbo',
                      data = locked_data, alternative = 'less',
                      patient_id > 80 & biomarker == 1)$p
    pf2 <- 1
    simes2 <- ps2
  }else{
    pf2 <- fitLogrank(Surv(pfs, pfs_event) ~ arm, placebo = 'pbo',
                      data = locked_data, alternative = 'less',
                      patient_id > 80)$p
    ps2 <- fitLogrank(Surv(pfs, pfs_event) ~ arm, placebo = 'pbo',
                      data = locked_data, alternative = 'less',
                      patient_id > 80 & biomarker == 1)$p
    simes2 <- min(2 * min(pf2, ps2), max(pf2, ps2))
  }

  trial$save(value = pf2, name = 'pf2')
  trial$save(value = ps2, name = 'ps2')
  trial$save(value = simes2, name = 'simes2')

  w1 <- sqrt(65 / 240)
  w2 <- sqrt(175 / 240)
  combine <- function(p1, p2){
    1 - pnorm(w1 * qnorm(1 - p1) + w2 * qnorm(1 - p2))
  }
  pf <- combine(pf1, pf2)
  ps <- combine(ps1, ps2)
  simes <- combine(simes1, simes2)

  trial$save(value = (simes <= .025 && pf <= .025), name = 'reject_full')
  trial$save(value = (simes <= .025 && ps <= .025), name = 'reject_subgroup')
}
```

Now that the three action functions exist, we run the milestone calls
shown at the beginning of this section and register the milestones with
a listener.

## Execute a Trial

Let’s run the trial once. With the seed set above, the interim lands in
the enrichment zone. The event plot shows the cumulative counts by arm
over calendar time, with the three milestones as dashed lines. The
`biomarker` panel is really the enrollment curve, since a baseline
endpoint is observed as soon as a patient is randomized. It bends at the
interim, where accrual slows down to biomarker-positive patients only.
In this particular replicate the re-estimated number of subgroup events
is reached while enrollment is still going on, so the final analysis
fires before the enlarged sample size is used up. That is by design: the
final condition is a number of events, and the sample size is only an
upper bound.

``` r

controller <- controller(trial, listener)
controller$run(plot_event = TRUE, silent = TRUE)
```

![](enrichmentDesign_files/figure-html/oeaif-1.png)

The output of the replicate collects the milestone times, the number of
patients and events at each milestone, and everything we saved in the
action functions. Here the subgroup hypothesis is rejected at the final
analysis and the full-population hypothesis is not, which is the only
possible outcome under enrichment.

``` r

controller$get_output() %>%
  kable(escape = FALSE) %>%
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                position = "left") %>%
  scroll_box(width = "100%")
```

| trial | seed | milestone_time\_\<interim\> | n_events\_\<interim\>\_\<pfs\> | n_events\_\<interim\>\_\<biomarker\> | n_events\_\<interim\>\_\<patient_id\> | n_events\_\<interim\>\_\<arms\> | decision | cp_full | cp_subgroup | milestone_time\_\<stage 1 analysis\> | n_events\_\<stage 1 analysis\>\_\<pfs\> | n_events\_\<stage 1 analysis\>\_\<biomarker\> | n_events\_\<stage 1 analysis\>\_\<patient_id\> | n_events\_\<stage 1 analysis\>\_\<arms\> | pf1 | ps1 | simes1 | milestone_time\_\<final\> | n_events\_\<final\>\_\<pfs\> | n_events\_\<final\>\_\<biomarker\> | n_events\_\<final\>\_\<patient_id\> | n_events\_\<final\>\_\<arms\> | n_positive | pf2 | ps2 | simes2 | reject_full | reject_subgroup | error_message |
|:---|---:|---:|---:|---:|---:|:---|:---|---:|---:|---:|---:|---:|---:|:---|---:|---:|---:|---:|---:|---:|---:|:---|---:|---:|---:|---:|:---|:---|:---|
| enrichment | 97 | 13.33963 | 54 | 124 | 124 | c(“pbo”,…. | enrichment | 0.0104861 | 0.9936771 | 20.41128 | 100 | 159 | 159 | c(“pbo”,…. | 0.3237104 | 0.014804 | 0.0296079 | 33.6292 | 159 | 225 | 225 | c(“pbo”,…. | 150 | 1 | 0.0230686 | 0.0230686 | FALSE | TRUE |  |

## Execute Trial Simulation

To study the operating characteristics of the design, we run many
replicates. The code below was run with 10000 replicates under the
alternative described in the settings, and with another 10000 replicates
under the global null, where the median of `PFS` in the treatment arm
equals the placebo median of 6 months in both biomarker strata. The
results are stored in the package, so the vignette does not have to
rerun them.

``` r

## reset a controller if $run has been executed before
controller$reset()
controller$run(n = 10000, plot_event = FALSE, silent = TRUE, tidy = TRUE)
output <- controller$get_output()
```

``` r

output %>%
  filter(scenario == 'alternative') %>%
  select(-scenario) %>%
  head(5) %>%
  kable(escape = FALSE) %>%
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                position = "left") %>%
  scroll_box(width = "100%")
```

| seed | milestone_time\_\<interim\> | n_events\_\<interim\>\_\<pfs\> | n_events\_\<interim\>\_\<biomarker\> | n_events\_\<interim\>\_\<patient_id\> | decision | cp_full | cp_subgroup | milestone_time\_\<stage 1 analysis\> | n_events\_\<stage 1 analysis\>\_\<pfs\> | n_events\_\<stage 1 analysis\>\_\<biomarker\> | n_events\_\<stage 1 analysis\>\_\<patient_id\> | pf1 | ps1 | simes1 | milestone_time\_\<final\> | n_events\_\<final\>\_\<pfs\> | n_events\_\<final\>\_\<biomarker\> | n_events\_\<final\>\_\<patient_id\> | n_positive | pf2 | ps2 | simes2 | reject_full | reject_subgroup |
|---:|---:|---:|---:|---:|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|:---|
| 1716968512 | 13.24 | 48 | 122 | 122 | promising | 0.3478 | 0.0086 | 22.90 | 133 | 238 | 238 | 0.25950 | 0.146400 | 0.259500 | 49.61 | 400 | 480 | 480 | 198 | 0.5680000 | 0.0066820 | 0.0133600 | FALSE | TRUE |
| 1151366066 | 14.92 | 67 | 143 | 143 | promising | 0.7659 | 0.9915 | 20.01 | 117 | 204 | 204 | 0.01271 | 0.003335 | 0.006669 | 48.36 | 400 | 480 | 480 | 191 | 0.0001214 | 0.0018620 | 0.0002429 | TRUE | TRUE |
| 473618302 | 15.09 | 57 | 145 | 145 | promising | 0.4032 | 0.0200 | 23.51 | 128 | 246 | 246 | 0.37890 | 0.464100 | 0.464100 | 49.38 | 400 | 480 | 480 | 196 | 0.0000168 | 0.0000095 | 0.0000168 | TRUE | TRUE |
| 1521899696 | 13.80 | 55 | 129 | 129 | favorable | 0.9999 | 1.0000 | 28.75 | 186 | 308 | 308 | 0.04332 | 0.042350 | 0.043320 | 35.38 | 240 | 320 | 320 | 121 | 0.0066500 | 0.0185700 | 0.0133000 | TRUE | TRUE |
| 1786717175 | 12.28 | 48 | 111 | 111 | promising | 0.8714 | 0.9987 | 24.80 | 148 | 261 | 261 | 0.08308 | 0.022370 | 0.044750 | 42.15 | 329 | 469 | 469 | 171 | 0.0545000 | 0.0006147 | 0.0012290 | TRUE | TRUE |

Under the alternative, here is how often each zone is reached, and how
the adaptations change the duration and the size of the trial. In the
futility zone the trial ends at the interim, which is why the three
milestone times coincide there.

``` r

output %>%
  filter(scenario == 'alternative') %>%
  group_by(decision) %>%
  summarise(
    zone = n() / 100,
    time_interim = mean(`milestone_time_<interim>`),
    time_stage1 = mean(`milestone_time_<stage 1 analysis>`),
    time_final = mean(`milestone_time_<final>`),
    n_final = mean(`n_events_<final>_<patient_id>`),
    n_positive_final = mean(n_positive),
    events_final = mean(`n_events_<final>_<pfs>`)
  ) %>%
  kable(col.names = c('Zone', 'Zone (%)', 'Interim', 'Stage 1', 'Final',
                      'Patients', 'Positive Patients', 'PFS Events'),
        digits = 1, align = 'r',
        caption = 'Decision Zones, Milestone Times, and Trial Size at the Final Analysis') %>%
  add_header_above(c(' ' = 2, 'Time (Months)' = 3, 'At Final Analysis' = 3)) %>%
  kable_styling(full_width = TRUE)
```

[TABLE]

Decision Zones, Milestone Times, and Trial Size at the Final Analysis
{.table .table style="margin-left: auto; margin-right: auto;"}

Power is summarized by zone and overall. The three columns “Full Only”,
“Subgroup Only” and “Both” are mutually exclusive outcomes, and “Any”,
their sum, is the probability of rejecting at least one of the two
hypotheses.

``` r

power_table <- function(dat){
  dat %>%
    summarise(
      zone = n() / 100,
      reject_full_only = 100 * mean(reject_full & !reject_subgroup),
      reject_subgroup_only = 100 * mean(!reject_full & reject_subgroup),
      reject_both = 100 * mean(reject_full & reject_subgroup),
      reject_any = 100 * mean(reject_full | reject_subgroup)
    )
}

alt <- output %>% filter(scenario == 'alternative')
bind_rows(
  alt %>% group_by(decision) %>% power_table(),
  alt %>% power_table() %>% mutate(decision = 'overall')
) %>%
  kable(col.names = c('Zone', 'Zone (%)', 'Full Only', 'Subgroup Only',
                      'Both', 'Any'),
        digits = 1, align = 'r',
        caption = 'Power (%) under the Alternative') %>%
  add_header_above(c(' ' = 2, 'Rejected Hypotheses (%)' = 4)) %>%
  kable_styling(full_width = TRUE)
```

[TABLE]

Power (%) under the Alternative {.table .table
style="margin-left: auto; margin-right: auto;"}

Under the global null, the probability of rejecting at least one
hypothesis is the family-wise error rate. The closed test with the
inverse normal combination keeps it at the one-sided level 0.025 in the
strong sense, whatever we decide at the interim. The rates within a
zone, however, are conditional on the interim decision and are not
controlled one by one. They are large in the favorable zone precisely
because that zone is reached when the interim data happen to look good.
Only the overall rate matters. With 10000 replicates, the Monte Carlo
standard error of the overall rate is about 0.16 percentage points.

``` r

null <- output %>% filter(scenario == 'null')
bind_rows(
  null %>% group_by(decision) %>% power_table(),
  null %>% power_table() %>% mutate(decision = 'overall')
) %>%
  kable(col.names = c('Zone', 'Zone (%)', 'Full Only', 'Subgroup Only',
                      'Both', 'Any'),
        digits = 1, align = 'r',
        caption = 'Type I Error (%) under the Global Null') %>%
  add_header_above(c(' ' = 2, 'Rejected Hypotheses (%)' = 4)) %>%
  kable_styling(full_width = TRUE)
```

[TABLE]

Type I Error (%) under the Global Null {.table .table
style="margin-left: auto; margin-right: auto;"}
