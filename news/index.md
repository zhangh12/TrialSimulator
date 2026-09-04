# Changelog

## TrialSimulator 1.35.8

### Performance

- [`fitLogrank()`](https://zhangh12.github.io/TrialSimulator/reference/fitLogrank.md)
  is faster. It now computes the (stratified) log rank statistic
  directly from
  [`survival::survdiff()`](https://rdrr.io/pkg/survival/man/survdiff.html)
  as observed-minus-expected events over its standard error, instead of
  fitting two Cox models per treatment arm (one for the sign of the
  effect and one with `ties = "exact"` for the score test). Its
  magnitude agrees with the previous exact-score statistic up to
  floating point rounding, while its sign now consistently follows the
  treatment log-rank score; with tied event times this can correct the
  previous sign when the Efron Cox coefficient pointed in the opposite
  direction. `info` and the `tidy = FALSE` columns are unchanged. When
  the statistic is undefined because its variance is zero (e.g., no
  event in the subset), a warning is issued and a simulation placeholder
  `z = 0` with the corresponding `p = 0.5` is returned instead of an
  error. On a three-arm dose-selection design with log rank tests at two
  milestones this removes roughly 40% of the per-replicate time, and
  roughly a third on an enrichment design driven by conditional power.

## TrialSimulator 1.35.7

### Performance

- Data locking is faster for trials with a regimen. The truncation of
  `regimen_trajectory` to the switches that have happened by the lock
  time, and the `n_switches` count, are now computed for all switching
  patients at once (one
  [`strsplit()`](https://rdrr.io/r/base/strsplit.html) plus
  [`tabulate()`](https://rdrr.io/r/base/tabulate.html)), instead of a
  per-patient [`mapply()`](https://rdrr.io/r/base/mapply.html) followed
  by a per-patient regular expression. Both columns are unchanged for
  names without the reserved characters below; on a 1000-patient
  three-arm design with crossover this removes roughly 14% of the
  per-replicate time.

### Updates

- `'@'` and `';'` are now reserved characters of the
  `regimen_trajectory` encoding:
  [`arm()`](https://zhangh12.github.io/TrialSimulator/reference/arm.md)
  rejects a name containing either, whether or not the trial uses a
  regimen (e.g., `arm(name = 'dose@5mg')` used to be accepted and now is
  an error), and `what()` of a regimen or of
  [`crossover()`](https://zhangh12.github.io/TrialSimulator/reference/crossover.md)
  must not return a `new_treatment` containing either. Previously such
  names silently corrupted the trajectory and `n_switches`. Data locking
  also asserts that every trajectory with a switch parses cleanly and
  keeps its initial segment.

## TrialSimulator 1.35.6

### Performance

- New argument `tidy` in `controller$run()` (default `FALSE`). With
  `tidy = TRUE`, the per-arm event count table (output column
  `n_events_<milestone>_<arms>`) is not saved at milestones;
  per-endpoint totals and milestone times are still saved, and the table
  remains available in the attributes of locked data, so `event_plot()`
  is unaffected. Saving that table is the most expensive part of the
  standard outputs: skipping it removes roughly 15% of the per-replicate
  time on a small two-arm design with trivial actions (on top of 1.35.5)
  and about 3% on a three-arm dose-selection design with log-rank tests
  at milestones. Unlike `tidy` in `get_output()`, which removes columns
  after the fact, this avoids the cost entirely.

## TrialSimulator 1.35.5

### Performance

- `controller$run()` collects the output of each replicate in a list and
  row-binds once at the end, instead of calling
  [`bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html)
  after every replicate (a fixed cost of roughly 1 ms per call). The
  same applies to the parallel path. Outputs are unchanged, including
  the partial output kept when a replicate fails. Removes roughly 10% of
  the per-replicate time on a small two-arm design with trivial actions
  and 3-5% on heavier designs.

## TrialSimulator 1.35.4

### Performance

- Patient enrollment is faster for trials without stratification factors
  (the common case). Internally, `enroll_patients()` now assembles the
  patient data of each arm directly from one generated pool, skipping
  the per-stratum bookkeeping
  ([`table()`](https://rdrr.io/r/base/table.html),
  [`merge()`](https://rdrr.io/r/base/merge.html),
  [`split()`](https://rdrr.io/r/base/split.html),
  [`bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html))
  that stratified randomization requires. Generator and dropout
  functions are called in exactly the same order and with the same sizes
  as before, so the random number stream and therefore all results are
  unchanged for a given seed (a regression test pins the patient data of
  a fixed-seed trial to reference values). On a small two-arm design
  with trivial actions this removes roughly 19% of the per-replicate
  time; the relative gain shrinks as the statistical work per milestone
  grows.

## TrialSimulator 1.35.3

### Performance

- Patient generation is faster for arms with inclusion criteria (subset
  conditions in `arm(name, ...)`). The criteria are now applied as a
  logical row mask with the semantics of
  [`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)
  (conditions combined with `&`, rows with `NA` dropped, `.data`/`.env`
  pronouns supported), instead of calling
  [`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)
  in every round of the rejection-sampling loop. The mask evaluation is
  shared with the lock-time fast path of 1.35.2 (internal
  `filter_conditions_mask()`). The human-readable criteria string used
  in error messages is also built once, when the arm is created, instead
  of on every generator call (deparsing quosures is not free). Results
  are unchanged; on a 1000-patient three-arm design with `pfs <= os` as
  inclusion criterion this removes roughly 9% of the per-replicate time.

## TrialSimulator 1.35.2

### Performance

- Milestone conditions with subset conditions, e.g.,
  `eventNumber(endpoint = 'pfs', n = 40, patient_id <= 70)` or
  `enrollment(n = 100, biomarker == 1)`, now take the C++ lock-time fast
  path. The subset conditions are reduced to a logical row mask with the
  semantics of
  [`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)
  (conditions combined with `&`, rows with `NA` dropped, `.data`/`.env`
  pronouns supported) and the existing C++ helpers are applied to the
  subset, instead of building per-endpoint event tables with `dplyr` for
  every endpoint in the trial. Results are unchanged; the pure-R path
  remains available through `options(trialsimulator.use_cpp = FALSE)`.
  On an enrichment design whose milestones are all subgroup-filtered
  this removes roughly 11% of the per-replicate time.

## TrialSimulator 1.35.1

### Performance

- Data locking is faster for trials with a regimen. The `n_switches`
  column of locked data is now computed on the subset of patients who
  switched treatment, instead of running a regular expression over the
  `regimen_trajectory` of every enrolled patient at every milestone.
  Results are unchanged; for a 1000-patient three-arm design with
  crossover this removes roughly 9% of the per-replicate time.

## TrialSimulator 1.35.0

### Updates

- The trial now takes ownership of registered objects by capturing
  independent deep copies. `trial$add_arms()` deep-clones every arm it
  registers, and `trial$add_regimen()` deep-clones the regimen.
  Consequently, changes made to the caller’s arm or regimen objects
  after registration no longer affect the trial, and adaptations within
  the trial (e.g., `trial$update_generator()`, `trial$crossover()`) no
  longer modify the caller’s objects, so those objects can be safely
  reused, e.g., to build another trial. Complete the configuration of an
  arm before registering it; after registration, change it only through
  the trial’s adaptation methods. The copy covers the arm, endpoint, and
  regimen objects themselves; a mutable environment or R6 object
  captured by a user-supplied function (generator, action,
  `what`/`when`/`how`) is shared by design of R closures and is not
  isolated.
- `trial$add_arms()` registration is now transactional: all incoming
  arms are validated (including endpoint-set consistency and re-use of
  the name of a previously removed arm) before any of them is installed,
  so a failing batch no longer leaves the trial partially modified.
  Re-adding an arm under the name of a removed arm is rejected
  explicitly; previously it failed halfway with the trial left partially
  modified.
- `controller$run()` now restores the listener’s milestones to their
  as-designed state before the first replicate. Previously, reusing a
  listener from an earlier controller in the same session left its
  milestones in a triggered state, silently suppressing them in the new
  run.

## TrialSimulator 1.34.1

### Bug fixes

- Generator updates no longer leak across simulation replicates.
  Previously, the arms snapshot taken at the beginning of
  `controller$run()` shared its endpoint objects with the live arms, so
  a `trial$update_generator()` call in one replicate silently altered
  the data-generating process of all later replicates, and a failing
  replicate could not be reproduced from the seed reported in its error
  message. The snapshot now deep-clones endpoints, and every replicate
  starts from freshly cloned, as-designed arms.

### Updates

- `trial$update_generator()` now accepts `endpoint_name` in any order.
  Endpoint names registered together in one
  [`endpoint()`](https://zhangh12.github.io/TrialSimulator/reference/endpoint.md)
  call are matched as a set, so, e.g., `c('pfs', 'biomarker')` and
  `c('biomarker', 'pfs')` are equivalent. Previously the names had to be
  given in registration order, and a reordered vector was rejected with
  a misleading error message. In addition, when `endpoint_name` covers
  only part of a registration, or mixes names from different
  registrations, the error message now spells out the exact
  `endpoint_name` vector(s) to use.

## TrialSimulator 1.34.0

### New features

- New method `trial$eventNumberReestimationFromConditionalPower()`
  re-estimates the number of events at the final analysis for every
  treatment-vs-placebo comparison of a time-to-event endpoint: the
  smallest whole number of events, greater than the number observed at a
  triggered interim milestone, at which the conditional power of
  `trial$conditionalPower()` reaches a target. Conditional power can be
  evaluated at the interim trend or at a user-specified hazard ratio,
  and a practical cap can be imposed through `D_cap`. When no solution
  exists in the requested range, `D` and `achieved_cp` are `NA` and
  `target_reached` is `FALSE`. The search is exact even when conditional
  power is not monotone in the event number.

### Updates

- Executable examples are added to the documentation of
  `trial$conditionalPower()` and the new method.
- A new vignette, “Conditional Power and Event-Number Reassessment”,
  describes the canonical conditional-power calculation, effect
  assumptions, alternative directions, nonmonotone event-number search,
  practical caps, and promising-zone usage in detail, including a
  complete action-based simulation program and precomputed
  operating-characteristic summaries.

## TrialSimulator 1.33.1

### Updates

- The `effect` argument of `trial$conditionalPower()` no longer has a
  default value; it must be specified explicitly as `'trend'`, `'null'`,
  or a hazard ratio.

## TrialSimulator 1.33.0

### New features

- New method `trial$conditionalPower()` computes conditional power at a
  triggered interim milestone for every treatment-vs-placebo comparison
  of a time-to-event endpoint, under a group sequential design with one
  interim and one final analysis. It pulls the milestone’s locked data
  automatically and calls
  [`fitLogrank()`](https://zhangh12.github.io/TrialSimulator/reference/fitLogrank.md)
  internally to obtain the observed z statistic and number of events of
  each comparison. Conditional power can be evaluated at the interim
  trend (default), under the null, or at a user-specified hazard ratio,
  which is converted internally using the allocation ratio in effect at
  the milestone. Its `alpha` argument is the one-sided nominal
  significance level corresponding to the planned final critical
  boundary, not the total design alpha or alpha spent at the final look.
  The calculation assumes the trial continues as designed (constant
  allocation ratio of the compared arms, planned final statistic and
  boundary); it is users’ responsibility to call it only when
  legitimate, e.g., not after the allocation ratio of the compared arms
  was adapted, as `TrialSimulator` does not verify this. Results are
  validated against `rpact` and `gsDesign` in unit tests.

## TrialSimulator 1.32.0

### Updates

- The `dry_run` argument of `controller$run()` is now defunct.

## TrialSimulator 1.31.0

### Updates

- `controller$run()` can no longer be called twice without `reset()` in
  between. Previously, a second `run()` silently continued the
  already-executed trial (all milestones already triggered, snapshots
  partially overwritten) instead of starting a new simulation. To define
  a milestone whose triggering condition or action depends on interim
  results, register it upfront and revise it within an action function
  through
  [`update_milestone()`](https://zhangh12.github.io/TrialSimulator/reference/update_milestone.md).
- A milestone that would lock data at a time earlier than the current
  trial time now raises an informative error before any trial state is
  modified, naming both milestones involved and the seed to debug with.
  Previously the violation was caught by `save_milestone_time()` only
  after the trial clock had been moved backwards and the locked snapshot
  stored; that check remains as an internal assertion, and
  `set_current_time()` gains a backstop against backward clock moves.
- `listener$add_milestones()` now raises an error when a milestone with
  the same name is already registered. Previously it warned and silently
  over-wrote the registered milestone. To modify a not-yet-triggered
  milestone within an action function, use
  [`update_milestone()`](https://zhangh12.github.io/TrialSimulator/reference/update_milestone.md).

## TrialSimulator 1.30.1

### Updates

- Continue minimizing the public API of R6 classes:
  `Arms$get_number_endpoints()`, `Endpoints$get_readout()` and
  `Endpoints$get_type()` are now private. The machinery methods of
  `Arms` (`get_name()`, `get_endpoints_name()`, `has_endpoint()` and
  `update_endpoint_generator()`) and of `Endpoints` (`get_uid()` and
  [`update_generator()`](https://zhangh12.github.io/TrialSimulator/reference/update_generator.md)),
  which stay public only because trials and arms invoke them, now carry
  a bold warning that users should not call them directly.
- Polish class documentation: user-facing methods of `Controllers`,
  `Arms` and `Endpoints` now come with brief usage notes in the class
  description; exploratory methods (`test_generator()`,
  `get_generator()` and `get_name()` of `Endpoints`; `generate_data()`
  and `get_endpoints()` of `Arms`) are documented as helpful for
  understanding the classes but not needed in formal simulation.

## TrialSimulator 1.30.0

### Updates

- Continue minimizing the public API of R6 classes:
  `Controllers$get_trial()`, `Controllers$get_listener()`,
  `Controllers$mute()`, `Milestones$execute_action()`,
  `Milestones$get_type()`, `Milestones$get_trigger_condition()`,
  `Milestones$get_action()`, `Regimens$get_number_time_selector()` and
  `Regimens$get_number_data_modifier()` are now private. They were used
  only internally by their own classes.
- The remaining public methods of `Milestones` and all public methods of
  `Regimens` are invoked on their objects by other components of the
  package (listeners and trials) and stay public only for that reason;
  they now carry a bold warning that users should not call them
  directly, in the same style as the `Trials` and `Listeners` classes.

## TrialSimulator 1.29.0

### Updates

- Minimize the public API of the `Listeners` class: `get_milestones()`
  is now private. The machinery methods `monitor()`, `mute()` and
  `reset()`, which stay public only because the controller invokes them,
  now carry a bold warning that users should not call them directly, in
  the same style as the `Trials` class.

## TrialSimulator 1.28.1

### Updates

- `trial$set_duration()`, `trial$resize()`, `trial$remove_arms()`,
  `trial$update_sample_ratio()` and `trial$update_generator()` now raise
  an error when called before any milestone has been triggered, i.e.,
  outside an action function.

## TrialSimulator 1.28.0

### New Feature

- New adaptation method `trial$update_milestone()` (wrapper
  [`update_milestone()`](https://zhangh12.github.io/TrialSimulator/reference/update_milestone.md)):
  update the triggering condition and/or the action of a
  not-yet-triggered milestone from within an action function. The update
  takes effect right after the current action function returns, and the
  as-designed milestone is restored between simulation replicates.

### Updates

- The `action` of a milestone must be a function; `NULL` is no longer
  accepted. Use `doNothing` if no action is intended.

## TrialSimulator 1.27.0

### Updates

- Minimize the public API of the `Trials` class: 38 methods used only
  internally are now private (including `censor_trial_data()`,
  `roll_back()`, `enroll_patients()`, `get_trial_data()` and
  `independentIncrement()`), leaving 34 public members whose definitions
  and documentation are reorganized into sections: adaptation methods,
  methods callable within action functions (data access and
  manipulation, trial status queries, statistical testing), trial setup
  (`add_regimen()`, which must not be called within action functions),
  and internal machinery. Machinery methods that stay public only
  because other components of the package invoke them now carry a bold
  warning that users should not call them directly.
- `make_snapshot()` distinguishes private methods from data fields via
  [`bindingIsLocked()`](https://rdrr.io/r/base/bindenv.html) (R6 locks
  the binding of every member defined as a function) instead of a
  hardcoded name list, so snapshot/reset stay correct as methods are
  added.

## TrialSimulator 1.26.2

### Updates

- Replace deprecated `.data$` in tidyselect contexts
  ([`rename()`](https://dplyr.tidyverse.org/reference/rename.html)/[`select()`](https://dplyr.tidyverse.org/reference/select.html)
  in the correlated PFS/OS generators,
  [`PiecewiseConstantExponentialRNG()`](https://zhangh12.github.io/TrialSimulator/reference/PiecewiseConstantExponentialRNG.md),
  and endpoint naming) with string literals, eliminating the tidyselect
  deprecation warnings that flooded test output. `.data$` remains in
  data-masking contexts
  ([`filter()`](https://dplyr.tidyverse.org/reference/filter.html),
  [`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html),
  `aes()`), where it is still the recommended idiom; `R CMD check` stays
  clean.
- `set_dropout()` documentation no longer claims adaptive use: dropout
  times are generated at enrollment, so updating the generator within an
  action function would not apply to enrolled patients.
- Ignore the knitr vignette cache in `.gitignore` and exclude
  `.DS_Store` from the build via `.Rbuildignore`.
- Fix the remaining test-time warnings: `event_plot()` now selects the
  endpoint column via
  [`all_of()`](https://tidyselect.r-lib.org/reference/all_of.html)
  (tidyselect external-vector deprecation), and
  [`plot.three_state_model()`](https://zhangh12.github.io/TrialSimulator/reference/plot.three_state_model.md)
  zooms with `coord_cartesian()` instead of `xlim()`, so the dashed
  guide segments anchored at the axes are clipped and rendered rather
  than dropped with a ggplot2 warning.
- Fix the root cause of a stray `Rplots.pdf` appearing after test runs:
  [`summarizeDataFrame()`](https://zhangh12.github.io/TrialSimulator/reference/summarizeDataFrame.md)
  restored [`par()`](https://rdrr.io/r/graphics/par.html) settings via
  [`on.exit()`](https://rdrr.io/r/base/on.exit.html) after its
  [`png()`](https://rdrr.io/r/grDevices/png.html) device was already
  closed, which implicitly opened the default device.
  [`par()`](https://rdrr.io/r/graphics/par.html) settings are local to
  the device and need no restore.

## TrialSimulator 1.26.1

### Bug Fix

- `make_snapshot()` silently dropped `NULL`-valued private fields from
  the snapshot (assigning `NULL` with `[[<-` deletes a list entry);
  `reset()`’s explicit re-null list masked this for existing fields. The
  snapshot now preserves `NULL` fields, so any future field is restored
  across simulation replicates by construction.
- Fix an error message in `dunnettTest()` that referenced an undefined
  variable.
- Fix a summary line in the `doseRanging` vignette that compared a
  string literal instead of the `decision` column.

### Updates

- Regenerate the precomputed outputs of the `doseRanging` and
  `fixedDesign` vignettes under the 1.25.2 enrollment convention; the
  `fixedDesign` output now contains the 1,000 replicates its code states
  (previously 100).
- Add an `R CMD check` GitHub Actions workflow (macOS/Windows/Ubuntu; R
  release/devel/oldrel).
- Add `\value` sections to exported help topics, a grouped reference
  index for the pkgdown site, and mention the newest adaptations in the
  `actionFunctions` vignette introduction.
- Remove the unused `graphicalMCP` entry from `Suggests`.

## TrialSimulator 1.26.0

### New Feature

- New adaptation method `trial$update_accrual_rate()` (wrapper
  [`update_accrual_rate()`](https://zhangh12.github.io/TrialSimulator/reference/update_accrual_rate.md)):
  update the accrual rate of the recruitment curve at a milestone, e.g.,
  to revise recruitment after dose selection or enrichment, or to pause
  it for a period after an interim decision. `end_time` of the new
  `accrual_rate` is measured from the milestone; patients not yet
  enrolled (and the enrollment reserves used by
  [`resize()`](https://zhangh12.github.io/TrialSimulator/reference/resize.md))
  are re-planned and re-randomized under the new schedule, while
  enrolled patients are left unchanged. Like other adaptations, it must
  be called within an action function, after a milestone has been
  triggered.

## TrialSimulator 1.25.2

### Updates

- [`StaggeredRecruiter()`](https://zhangh12.github.io/TrialSimulator/reference/StaggeredRecruiter.md)
  now enrolls patient `k` when the planned cumulative accrual reaches
  `k` (previously `k - 1`): under a constant rate `r` the `n`-th patient
  enrolls exactly at `n / r`, so a milestone triggered by
  `enrollment(n)` occurs exactly at the planned accrual time. The first
  patient enrolls at `1 / piecewise_rate` instead of time 0, and all
  enrollment times shift by one inter-arrival accordingly; simulation
  results under a fixed seed differ slightly from 1.25.1. The
  precomputed output of the `adaptiveDesign` vignette is regenerated
  under the new convention.

## TrialSimulator 1.25.1

### Bug Fix

- Fix an infinite loop in patient enrollment when fractional sample
  ratios are combined with stratification factors: the pool labels now
  match the unstratified randomization queue in that case.

### Updates

- Warn when fractional sample ratios are used in a trial with
  stratification factors: stratified randomization is not supported in
  that case and unenrolled patients are randomized by
  [`sample()`](https://rdrr.io/r/base/sample.html) without
  stratification. The warning is suppressed when `silent = TRUE`.

## TrialSimulator 1.25.0

### New Feature

- New adaptation method `trial$stop_followup()` (wrapper
  [`stop_followup()`](https://zhangh12.github.io/TrialSimulator/reference/stop_followup.md)):
  stop follow-up of a subset of enrolled patients at a milestone,
  optionally after extra follow-up time (`additional_followup`).
  Patients are selected by conditions compatible with
  [`dplyr::filter`](https://dplyr.tidyverse.org/reference/filter.html);
  affected time-to-event endpoints are censored and non-time-to-event
  readouts are set to missing. This adaptation supports, e.g., treatment
  discontinuation and enrichment design. Like other adaptations, it must
  be called within an action function, after a milestone has been
  triggered.

### Updates

- `censor_trial_data()` gains `...` to further restrict the patients to
  be censored by conditions compatible with
  [`dplyr::filter`](https://dplyr.tidyverse.org/reference/filter.html),
  in addition to `selected_arms` and `enrolled_before`. Internal callers
  are unchanged and benchmark at parity with 1.24.0; simulation outputs
  are identical.

## TrialSimulator 1.24.0

### New Feature

- `controller$run()` now displays a progress bar when `silent = TRUE`
  and `n_workers = 1` if the simulation is expected to take more than 1
  minute. Package `cli` is added to `Imports` for this feature.

## TrialSimulator 1.23.0

### Updates

- [`trial()`](https://zhangh12.github.io/TrialSimulator/reference/trial.md)
  now defaults `enroller` to `StaggeredRecruiter` and accepts no other
  enroller: any non-`StaggeredRecruiter` value is rejected with an
  informative error. The `enroller` argument is retained for backward
  compatibility, so existing code that passes
  `enroller = StaggeredRecruiter` explicitly is unaffected, and code
  that omits it now gets the default.

### Unit Tests

- Add `test-enroller.R` covering the default, explicit
  `StaggeredRecruiter`, rejection of other enrollers, and
  post-construction `set_enroller()` enforcement.

## TrialSimulator 1.22.0

### New Feature

- [`StaggeredRecruiter()`](https://zhangh12.github.io/TrialSimulator/reference/StaggeredRecruiter.md)
  now supports recruitment pauses: a window with `piecewise_rate = 0`
  enrolls no one while calendar time still advances, so accrual resumes
  at the window’s `end_time`. Pauses may occur in the first window or
  span several consecutive windows (e.g., a safety hold, a site not yet
  activated, or a seasonal gap).

### Updates

- [`StaggeredRecruiter()`](https://zhangh12.github.io/TrialSimulator/reference/StaggeredRecruiter.md)
  enrollment times are now the deterministic inverse of the cumulative
  accrual intensity: the cumulative accrual capacity increases by window
  length × `piecewise_rate` across each window (no per-window
  truncation), so an integer-capacity window holds exactly that many
  patients. Single open-ended schedules and integer-capacity windows are
  unchanged; schedules with fractional per-window capacity now yield
  slightly different (more accurate) times.
- [`StaggeredRecruiter()`](https://zhangh12.github.io/TrialSimulator/reference/StaggeredRecruiter.md)
  input validation is stricter: the last `end_time` must be `Inf` with a
  positive rate (so the schedule can always supply the patients the
  engine requests, including the inflated count used for adaptive
  resizing); a positive rate too low to enroll even one patient (window
  length × `piecewise_rate` \< 1) is now an error pointing to
  `piecewise_rate = 0`; and `n` must be a positive integer.

### Unit Tests

- Add `test-StaggeredRecruiter.R` covering pauses (leading, middle, and
  consecutive), the per-window count property, equidistant spacing, the
  low-rate and schedule-shape validation errors, and `n` validation.

## TrialSimulator 1.21.0

### Bug Fix

- `GraphicalTesting$test()` no longer errors with
  `alpha_spent should be monotonically increasing` when a hypothesis is
  first tested only at a later (or final) look. This happens when a
  hypothesis holds zero or very small allocated alpha at early looks and
  inherits alpha after another hypothesis is rejected: those looks yield
  a degenerate (flat, near-zero) reconstructed cumulative alpha-spent
  sequence. Such entries are now truncated and floored to a strictly
  increasing sequence before being passed to the group sequential
  boundary computation.

### Unit Tests

- Add regression tests for the above fix (alpha inherited only at the
  final look; tiny-alpha early looks with degenerate spending
  boundaries), plus tests that re-including an already-rejected
  hypothesis at a later look is ignored safely and that
  `computeCumulativeAlphaSpent` treats `Inf` boundaries as zero
  increments.

## TrialSimulator 1.20.0

### New Feature

- [`endpoint()`](https://zhangh12.github.io/TrialSimulator/reference/endpoint.md)
  now accepts `type = "baseline"` for a non-tte endpoint observed at
  randomization (e.g., a baseline covariate, biomarker, or subgroup
  indicator). Its readout is `0` by definition and must be omitted from
  `readout`. For forward compatibility, a non-tte endpoint observed at
  randomization can still be defined the existing way with
  `readout = 0`. `Endpoints$new()` continues to recognize only `"tte"`
  and `"non-tte"`; the conversion happens in
  [`endpoint()`](https://zhangh12.github.io/TrialSimulator/reference/endpoint.md).

## TrialSimulator 1.19.1

### Documentation

- Add the “Crossover at milestone” vignette demonstrating
  [`crossover()`](https://zhangh12.github.io/TrialSimulator/reference/crossover.md).
- Update the
  [`regimen()`](https://zhangh12.github.io/TrialSimulator/reference/regimen.md)/[`crossover()`](https://zhangh12.github.io/TrialSimulator/reference/crossover.md)
  examples and help pages to use the natural
  `ifelse(condition, new_value, original)` idiom in `how()` and to
  return only the switching patients in `what()`, rather than relying on
  `NA` to mark unchanged cells or non-switchers. The engine still
  accepts `NA` for backward compatibility.

## TrialSimulator 1.19.0

### New Feature

- Add
  [`crossover()`](https://zhangh12.github.io/TrialSimulator/reference/crossover.md)
  for milestone-triggered treatment crossover. Called inside a
  milestone’s action function, it lets patients still in the trial
  switch treatment at (or after) the milestone, altering only their
  post-switch endpoint values. Regimens are unified under one contract
  keyed off an earliest crossover time; the classic enrollment regimen
  added via `add_regimen()` is the `T = 0` case.

### Updates

- A regimen’s `how()` may now only modify post-switch outcomes:
  returning a value that differs from the original for an endpoint whose
  readout/event is at or before `switch_time` (a pre-switch or
  already-observed outcome) raises an error. Guard such `how()`
  functions accordingly,
  e.g. `os = ifelse(os > switch_time, new_os, os)`.

## TrialSimulator 1.18.4

CRAN release: 2026-05-13

### Bug Fix

- Re-export [`Surv()`](https://rdrr.io/pkg/survival/man/Surv.html) and
  [`strata()`](https://rdrr.io/pkg/survival/man/strata.html) from the
  `survival` package so that user-supplied formulas in action functions
  (e.g. `Surv(os, os_event) ~ arm`) work after
  [`library(TrialSimulator)`](https://zhangh12.github.io/TrialSimulator/)
  alone, without requiring
  [`library(survival)`](https://github.com/therneau/survival) or the
  `survival::` prefix. This fixes errors in parallel runs
  (`n_workers > 1`) where each worker only attaches `TrialSimulator`
  (reported in
  [\#14](https://github.com/zhangh12/TrialSimulator/issues/14)).

## TrialSimulator 1.18.1

### Bug Fix

- Fix a bug where a composite milestone triggering condition combined
  with `'or'` would error out when not all of its branches could be
  reached, instead of resolving to the reachable branch.

## TrialSimulator 1.18.0

### New Feature

- Accelerate milestone trigger evaluation via C++ helpers (Rcpp) for
  [`eventNumber()`](https://zhangh12.github.io/TrialSimulator/reference/eventNumber.md)
  and
  [`enrollment()`](https://zhangh12.github.io/TrialSimulator/reference/enrollment.md)
  conditions. Wall-time reduction ranges from a few percent to ~20%
  depending on how condition-evaluation-heavy the design is, with no
  change to results. Set `options(trialsimulator.use_cpp = FALSE)` to
  fall back to the original R implementation.

## TrialSimulator 1.17.1

CRAN release: 2026-04-27

### Updates

- Reduce vignette compilation time for CRAN (`doseRanging.Rmd` now loads
  pre-computed simulation output; `simulatePfsAndOsGumbel.Rmd` uses a
  smaller validation sample).
- Skip a few stochastic unit tests on CRAN to avoid occasional flakes
  from RNG variation.

## TrialSimulator 1.17.0

### New Feature

- Add
  [`CorrelatedPfsAndOs2()`](https://zhangh12.github.io/TrialSimulator/reference/CorrelatedPfsAndOs2.md)
  to simulate correlated PFS and OS using a Gumbel copula while
  targeting marginal PFS/OS medians and Kendall’s tau between observed,
  uncensored PFS and OS times.

### Updates

- Add a vignette for simulating correlated PFS and OS using the Gumbel
  copula method, alongside the existing illness-death model vignette.
- Skip an unnecessary
  [`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)
  call in `Trials$get_event_tables()` when no filter expression is
  provided, reducing total simulation wall time by roughly 5-10% in
  trials with frequent milestone condition checks.

## TrialSimulator 1.16.0

CRAN release: 2026-04-13

### New Feature

- Add `n_switches` column to locked data returned by
  `get_locked_data()`, counting the number of treatment switches per
  patient within the data lock window.
- Add
  [`expandRegimen()`](https://zhangh12.github.io/TrialSimulator/reference/expandRegimen.md)
  to expand the `regimen_trajectory` column in locked data into a
  long-format data frame with one row per regimen segment per patient.

### Updates

- Cut running time by ~75% via base R rewrites.

## TrialSimulator 1.15.0

### New Feature

- Support stratified permuted block randomization in
  [`trial()`](https://zhangh12.github.io/TrialSimulator/reference/trial.md)
  using argument `stratification_factors`.
- Support `...` in
  [`regimen()`](https://zhangh12.github.io/TrialSimulator/reference/regimen.md)
  to pass arguments to `what()`, `when()` and `how()`.

### Bug Fix

- Fix a bug in
  [`regimen()`](https://zhangh12.github.io/TrialSimulator/reference/regimen.md)
  to support both single or multiple switching.

## TrialSimulator 1.14.0

### New Feature

- Support multiple rounds of dynamic treatment switching through
  [`regimen()`](https://zhangh12.github.io/TrialSimulator/reference/regimen.md).

### Updates

- New implementation that is 55% faster without regimen, or 40% faster
  with regimen. This is achieved by avoid computing redundant event
  counts.

## TrialSimulator 1.13.0

### New Feature

- New implementation of regimen that is 60% faster.

### Update

- Add a vignette of crossover design with wash-out periods.

### Bug Fix

- Fix a minor bug displaying incorrect event count when using
  [`enrollment()`](https://zhangh12.github.io/TrialSimulator/reference/enrollment.md)
  to define milestone. This bug does not affect milestone triggering.

## TrialSimulator 1.12.0

### New Feature

- Support editing trajectory of endpoint over time.
- Add new argument `tidy` (default: `TRUE`) in `Controllers$run()` to
  stop computing and saving event count per arm per endpoint for 40%
  shorter run time.

## TrialSimulator 1.11.0

### New Feature

- Add function
  [`solvePiecewiseConstantExponentialDistribution()`](https://zhangh12.github.io/TrialSimulator/reference/solvePiecewiseConstantExponentialDistribution.md)
  to compute event rates in time windows given survival probabilities at
  changepoints.
- Add `qPiecewiseExponent()`, the quantile function of piecewise
  exponential distribution. This function is useful to simulate
  time-to-event endpoint that is correlated to other endpoints using the
  copula method. For example, the `simdata` package needs marginal
  quantile functions.

## TrialSimulator 1.10.0

CRAN release: 2026-02-15

### New Feature

- Support parallelization in `Controllers$run()` through new argument
  `n_workers`. The package `mirai` is used. Although `mirai` advocates
  the use of L’Ecuyer-CMRG streams to maintain independence between
  multiple streams, however, `TrialSimulator` resets it to be
  Mersenne-Twister streams to force identical behavior between
  `n_workers = 1` and `n_workers > 1`. This enables debuggability and
  reproduciability under single-process mode by setting seed that causes
  issues under multi-process mode.

## TrialSimulator 1.9.0

### New Feature

- Support wrapper functions for adaptation, including
  [`remove_arms()`](https://zhangh12.github.io/TrialSimulator/reference/remove_arms.md),
  [`add_arms()`](https://zhangh12.github.io/TrialSimulator/reference/add_arms.md),
  [`update_sample_ratio()`](https://zhangh12.github.io/TrialSimulator/reference/update_sample_ratio.md),
  [`set_duration()`](https://zhangh12.github.io/TrialSimulator/reference/set_duration.md),
  [`resize()`](https://zhangh12.github.io/TrialSimulator/reference/resize.md)
  and
  [`update_generator()`](https://zhangh12.github.io/TrialSimulator/reference/update_generator.md).
- More informative message is prompted when error is throwed from an
  action function; milestone’s name is printed.

## TrialSimulator 1.8.0

### New Feature

- Support new adaptation `Trials$resize()` that resizes an ongoing
  trial.

## TrialSimulator 1.7.0

CRAN release: 2025-12-19

### New Feature

- Columns automatically recorded at milestones can be eliminated from
  `get_output()` by setting new argument `tidy = TRUE`.

### Minor Updates

- Some minor fixes for CRAN submission.

## TrialSimulator 1.6.0

### New Feature

- Generator of an endpoint can be updated during a running trial with
  `Trials$update_generator()`.

## TrialSimulator 1.5.0

### Update

- `enforce = TRUE` is no longer needed when adding new arms to an
  existing trial with at least one arm through `Trials$add_arms()`.
  However, for backward compatibility, legacy codes with
  `enforce = TRUE` still behaves as expected and no need to update.

## TrialSimulator 1.4.0

### Update

- Add vignette of dose-ranging study.

## TrialSimulator 1.3.0

CRAN release: 2025-09-26

### Major Updates

- Action function no longer needs argument `milestone_name`. Now action
  function only requires argument `trial` and supports optional
  arguments.
- [`milestone()`](https://zhangh12.github.io/TrialSimulator/reference/milestone.md)
  now support `...` to pass arguments to action functions.
- Documents and vignettes are updated.

### Minor Updates

- Some minor fixes.

## TrialSimulator 1.2.0

### New Feature

- [`enrollment()`](https://zhangh12.github.io/TrialSimulator/reference/enrollment.md)
  now supports `min_treatment_duration` to ensure minimum treatment
  duration received by patients at a milestone. With its default value
  0, milestone is triggered when a specific number of patients are
  enrolled.
- No longer print the return value of action function, thus
  `invisible(NULL)` is no longer recommended as return value of action
  function.

## TrialSimulator 1.1.0

### Update

- Add notes to R6 class indicating public methods that can be used by
  end users.
- Update help documents.

## TrialSimulator 1.0.0

CRAN release: 2025-09-03

### Update

- Fix issues to meet CRAN submission conditions.

## TrialSimulator 0.97.0

### Bug Fix

- Fix a bug in function `event_plot()` for plot of cumulative events
  number when endpoint name is `"ep"`. This is due to data masking in
  `dplyr`.
- Fix issues in unit tests caused by new dropout mechanisum.
- use Bonferroni method in unit test of `update_sample_ratio`. This test
  is probably broken by randomness (it is okay).

## TrialSimulator 0.96.0

### Update

- Update mechanism of simulating dropout time. Switching from trial
  level to patient level, i.e. dropout time is now the time from a
  patient is enrolled until leaving a trial. This aligns with common
  practice and popular softwares.

## TrialSimulator 0.95.0

### Update

- Add vignette of action function.

## TrialSimulator 0.94.0

### New Feature

- Add function `summarizeMilestoneTime` and its plot method to summarize
  triggering time of milestones.

## TrialSimulator 0.93.0

### New Feature

- [`update_sample_ratio()`](https://zhangh12.github.io/TrialSimulator/reference/update_sample_ratio.md)
  now supports updating multiple arms simultaneously. When ratio is not
  a whole number, [`sample()`](https://rdrr.io/r/base/sample.html) is
  used to replace the permuted block algorithm to randomize patients to
  arms. This enable response-adaptive design.
- Add a vignette of response-adaptive design using
  [`update_sample_ratio()`](https://zhangh12.github.io/TrialSimulator/reference/update_sample_ratio.md).

## TrialSimulator 0.92.0

### Update

-Add vignette of fixed design. -Add vignette of wrapper functions of
commom statistical tests.

## TrialSimulator 0.91.0

### New Feature

- Add function that maps medians of PFS and OS, and their correlation to
  the hazard parameters. The induced hazard parameters can be used with
  PFS-OS generator .

### Update

- Add vignette of simulating PFS and OS.

## TrialSimulator 0.90.0

### New Feature

- Save event counts per arm in simulation output.
- No long stop the program when all planned patients are already
  randomized into the trial when calling the function `enroll_patients`.
  This is useful when a milestone is triggered after all patients are
  recruited.

### Bug Fix

- Fix a bug that affects functions and when patient recruitment is
  completed fast thus no sample increment between some milestones. This
  bug can substantially reduce testing powers.

## TrialSimulator 0.89.0

### New Feature

- Support `...` in `eventNumber` to count event in subset of trial data.
  This is useful in enrichment design when milestone is defined based on
  biomarker.

## TrialSimulator 0.88.0

### New Feature

- Add function `get` as alias of `get_custom_data` in the `Trials`
  class.

### Bug Fix

- Fix a bug that no results is returned to controller when an error is
  triggered.

### Update

- Print informative message when
  `trial$dunnettTest(..., planned_info = "default")` triggers an error.

## TrialSimulator 0.87.0

### Bug Fix

- Revise `Trials$dunnettTest` to be compatible to one-sided logrank
  test.

## TrialSimulator 0.86.0

### Bug Fix

- Fix a bug in the fourth-state model.

## TrialSimulator 0.85.0

### New Feature

- Add data generators of time to response, progression and death.

## TrialSimulator 0.84.0

### New Feature

- The function `fitLogrank` now supports `formula`. `strata(...)` can be
  in `formula`. No covariate is accepted.
- Add unit tests for `fitLogistic`.

## TrialSimulator 0.83.0

### New Feature

- The function `fitLogistic` now supports `scale = "coefficient"` to
  compute regression coefficient as main effect of `arm` in the presence
  of covariates. It is also equivalent to `scale = "log odds ratio"` in
  the absence of covariates.
- Add unit tests for `fitLogistic`.

## TrialSimulator 0.82.0

### New Feature

- The function `fitCoxph` now supports `formula` to compute log hazard
  ratio or hazard ratio as main effect of `arm`. It will detect whether
  arm’s main effect is specified in formula. It allows covariates and
  interaction between covariates and arm. However, only the main effect
  of arm is tested and returned.
- Add unit tests for `fitCoxph`.

## TrialSimulator 0.81.0

### New Feature

- The function `fitLogistic` now supports computing log odds ratio, odds
  ratio, risk ratio, and risk difference using `emmeans` contrast, in
  the presence of covariates.
- Add unit tests for `fitLogistic` and `fitLinear`.

## TrialSimulator 0.80.0

### New Feature

- The function `fitLogistic` now supports `formula` with covariates, and
  uses `emmeans` contrast to compute average treatment effect (ATE) on
  the `logit` scale.

## TrialSimulator 0.79.0

### New Feature

- The function `fitLinear` now supports `formula` with covariates, and
  uses `emmeans` contrast to compute average treatment effect (ATE) on
  the mean scale.

## TrialSimulator 0.78.0

### Updates

- The `trigger_condition` in the function `milestone` is deprecated and
  is replaced with `when`. Note that `trigger_condition` is still
  supported in the `R6` class `Milestones`.

## TrialSimulator 0.77.0

### Updates

- Add case for unit test.

## TrialSimulator 0.76.0

### Bug Fixes

- Fix a bug that `trial$get_custom_data` throws an error when `n > 1` in
  `controller$run(n)` because custom data is wiped out in `trial$reset`.

## TrialSimulator 0.75.0

### Update

- Add vignette of defining arms.

## TrialSimulator 0.74.0

### New Feature

- Support inclusion criteria in `arm` through the `...` argument.

## TrialSimulator 0.73.0

### New Feature

- Print summary report of arms when printing an arm object in console or
  `rmarkdown`.

## TrialSimulator 0.72.0

### New Feature

- Print summary report of endpoints when printing an endpoint object in
  console or `rmarkdown`.

## TrialSimulator 0.70.0

### Updates

- Add vignette for longitudinal endpoints.

## TrialSimulator 0.68.0

### Updates

- Rename trial event as milestone. This a major update. Relevant codes
  and documents are updated accordingly.
- Add executable examples for
  [`controller()`](https://zhangh12.github.io/TrialSimulator/reference/controller.md)
  as per suggestion from CRAN team.

## TrialSimulator 0.67.0

### Updates

- Print event counts at trial events using
  [`message()`](https://rdrr.io/r/base/message.html) so that Shiny app
  can display it properly.

## TrialSimulator 0.66.0

### Updates

- Add cases for unit test.

## TrialSimulator 0.65.0

### New Features

- Add function `solveMixtureExponentialDistribution` to compute median
  of exponential endpoint of subgroup or the overall population.

## TrialSimulator 0.63.0

### New Features

- Add wrapper functions `endpoints`, `arm`, `trial`, `event`, `listener`
  and `controller` for `Endpoint$new`, `Arm$new`, `Trial$new`,
  `Event$new`, `Listener$new` and `Controller$new`.

## TrialSimulator 0.62.0

### New Features

- Allow extending trial duration with `Trial$set_duration`.

## TrialSimulator 0.61.0

### Updates

- Deprecate function `enroll_a_patient`. Use `enroll_patients` only.

## TrialSimulator 0.58.0

### Updates

- Update vignette of adaptive seamless design.

### Bug Fixes

- Fix a bug to use `n > 1` in `Controller$run` when an arm can possibly
  be removed adaptively during a trial.

### New Features

- Allow specifying arms in `enrollment`. This is useful to count
  randomized patients of all arms even if some are removed adaptively.

## TrialSimulator 0.57.0

### Updates

- Move vignette of comparison between `GraphicalTesting` and
  `graphicalMCP` to repository
  [TrialSimulatorDocuments](https://github.com/zhangh12/TrialSimulatorDocuments).

## TrialSimulator 0.56.0

### New Features

- `Controller$run` now can specify number of simulation replicates by
  newly added argument `n`. If `n` is greater than 1, simulation results
  can be accessed in `Controller$get_output()`.

## TrialSimulator 0.55.0

### Bug Fixes

- Fix a bug in `StaggeredRecruiter` to force the enrollment time of the
  first patient is zero. This is an known issue but I was too lazy to
  fix it. Earlier version may have overestimated time of events.

### New Features

- add function `fitFarringtonManning` of Farrington-Manning test for
  rate difference.

## TrialSimulator 0.54.0

### New Features

- Add function `Trial$bind` to row bind data frame in action functions.
  It is useful to prepare inputs of group sequential or graphical test.

## TrialSimulator 0.53.0

### New Features

- Add vignette of condition system.

## TrialSimulator 0.52.0

### Bug Fixes

- Fix a bug that data is not censored correctly at events. This bug does
  not affect a trial without interims.

## TrialSimulator 0.51.0

### New Features

- Add vignette of non-time-to-event endpoints.
- Add function `weibullDropout` to compute parameters of Weibull
  distribution when using it for dropout distribution.

## TrialSimulator 0.49.0

### New Features

- Add vignette of time-to-event endpoints.

## TrialSimulator 0.48.0

### Bug Fixes

- Fix a bug when alpha of a node in graph is set to rounding error bound
  `1e-5` while no alpha should have been propagated.

## TrialSimulator 0.47.0

### New Features

- Support new condition system for event triggering. Built-in functions
  `enrollment`, `eventNumber` and `calendarTime` can be combined with
  `&` and `|`. Nested combination is supported by using parentheses.
- `TriggerByEventNumbers` and `TriggerByCalendarTime` are therefore
  deprecated.

## TrialSimulator 0.44.0

### Bug Fixes

- Fix a bug when adding an arm that is already in the trial.

## TrialSimulator 0.43.0

### New Features

- Add function `fitLogistic` to fit logistic regression model.
- Support model fitting for multiple treatment arms in logistic
  regression, Cox PH model, and logrank test.

## TrialSimulator 0.42.0

### Minor Updates

- Add a logo.

## TrialSimulator 0.40.0

### Bug Fixes

- Revise examples for CRAN submission.

## TrialSimulator 0.39.0

### Bug Fixes

- Remove space in class name to eliminate R CMD check note.

## TrialSimulator 0.38.2

### Bug Fixes

- Throw error message when none of the hypotheses at test has non-zero
  alpha in graphical test.
- Update vignette.

## TrialSimulator 0.38.1

### New Features

- Plot stacked area chart for accumulative event numbers of endpoints.

### Bug Fixes

- Fix a bug when small weight (epsilon) is used in graph in graphical
  testing. Small weight can be conflict with integral tolerance error.

## TrialSimulator 0.37.0

### New Features

- Return more informative error message when custom random number
  generators are used to define endpoints. Specifically, it guides users
  to return columns for time-to-event endpoints properly.
- Update manual for `generator` in `Endpoint`.

## TrialSimulator 0.36.0

### Bug Fixes

- Fix a bug in `GraphicalTesting` when a hypothesis is tested multiple
  times at the same stage because more alpha is passed from other
  rejected hypothesis.

## TrialSimulator 0.35.0

### New Features

- Warn when incremental information is too low that can affect normality
  approximation of combination test.

### Bug Fixes

- Minor bugs fixed.

## TrialSimulator 0.33.0

### New Features

- Add README.

## TrialSimulator 0.33.0

### New Features

- Provide a default action function `do_nothing()` if users have no
  intent to do anything at a triggered event. This function can be
  passed to the argument `action` when creating a new event, e.g.,
  `Event$new(name = 'interim', trigger_condition = TriggerByCalendarTime, action = do_nothing, calendar_time = 64)`.

## TrialSimulator 0.30.0

### New Features

- Capture error inside `Controller$run()` and insert error message into
  output (see `Trial$get_output()$error_message`). It helps to integrate
  `TrialSimulator` with `targets`.

## TrialSimulator 0.29.0

#### New Features

- Support closed test based on inverse normal combination test.
- Seed can be accessed by `Trial$get_seed()`.

## TrialSimulator 0.28.0

#### New Features

- Support inverse normal combination test when multiple treatment arms
  present. Dunnett’s test is used for comparison.
- Specify random seed if user dose not pick one. Seed is saved into
  Trial’s output for reproducibility.

## TrialSimulator 0.27.0

#### New Features

- Support dry run for fixed design.

## TrialSimulator 0.26.0

#### New Features

- Adjust boundary at final analysis for over- or under-running trials.
- Support custom alpha spending function in graphical testing procedure.

## TrialSimulator 0.25.0

#### New Features

- Support inverse normal combination test for logrank statistics.

## TrialSimulator 0.24.0

#### New Features

- Update `GraphicalTesting` based on simplified interface of
  `GroupSequentialTest`.
