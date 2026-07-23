# Updating the Accrual Rate of a Trial at a Milestone

update the accrual rate of the recruitment curve at a milestone. The
enroller of a trial is always `StaggeredRecruiter`; this function
replaces its `accrual_rate` for patients not yet enrolled, while
enrolled patients are left unchanged. This function can be used in
adaptive designs; its application includes, but is not limited to,
revising recruitment after dose selection or enrichment, or pausing
recruitment for a period after an interim decision.

`end_time` in `accrual_rate` is measured from the time this function is
called (i.e., the current milestone), not from the start of the trial. A
milestone is usually event driven, so its calendar time is unknown until
the trial is simulated, and a schedule on the calendar time scale could
not be pre-specified. Measuring `end_time` from the milestone also lets
users state the new plan simply as "from now on": e.g.,
`data.frame(end_time = c(3, Inf), piecewise_rate = c(20, 35))` means 20
patients per month for the 3 months following the milestone and 35 per
month thereafter, whenever the milestone occurs. Following the
convention of `StaggeredRecruiter`, the first re-planned patient is
enrolled `1/piecewise_rate` after the milestone; a leading window with
`piecewise_rate = 0` defers enrollment further. As with other
adaptations, patients not yet enrolled are re-randomized and their data
are regenerated under the new schedule.

Note that this function should only be called within action functions of
milestones. It is users' responsibility to ensure that and
`TrialSimulator` has no way to track it. Calling it before any milestone
has been triggered is an error.

This is a user-friendly wrapper of the member function of trial, i.e.,
`Trials$update_accrual_rate()`, which is used in vignettes. Users who
are not familiar with the concept of classes may consider using this
wrapper directly.

## Usage

``` r
update_accrual_rate(trial, accrual_rate)
```

## Arguments

- trial:

  a trial object returned by
  [`trial()`](https://zhangh12.github.io/TrialSimulator/reference/trial.md).

- accrual_rate:

  a data frame of columns `end_time` and `piecewise_rate` as in
  `StaggeredRecruiter`, with `end_time` measured from the current
  milestone. The last `end_time` must be `Inf` with a positive rate.
