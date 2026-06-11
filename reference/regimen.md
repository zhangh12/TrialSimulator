# Define a Regimen

Define a regimen of a trial. This is a user-friendly wrapper for the
class constructor `Regimens$new()`. Users who are not familiar with the
concept of classes may consider using this wrapper directly.

A regimen defines the rules to select patients who switch treatments, to
determine the time of switching, and to update patients' endpoint data.

## Usage

``` r
regimen(what, when, how, ...)
```

## Arguments

- what:

  a function determining whether patients' data would be updated due to
  switching treatment. It takes `patient_data`, a data frame as
  argument, and returns a data frame of two columns `patient_id` and
  `new_treatment`, with one row per switching patient. The number of
  rows in the returned data frame may be smaller than the number of
  patients in the input data frame; patients that are left out are
  simply not switched. Note that the returned object will be passed into
  function \`how()\`, which is also provide by users. This argument can
  also be a list of functions that will be executed sequentially. No
  default value.

- when:

  a function determining the time at which a patient switches to another
  treatment regimen, measured from the time of enrollment. It takes
  `patient_data`, a data frame as argument, and returns a data frame of
  two columns `patient_id` and `switch_time` (from `enroll_time`). The
  number of rows in the returned data frame must equal the number of
  rows in `patient_data`, i.e., a switching time must be specified for
  every patient (missing values are not allowed). Note that the returned
  object will be passed into function \`how()\`, which is also provided
  by users. This argument can also be a list of functions that will be
  executed sequentially. No default value.

- how:

  a function updating patients' data after treatment switching. Only
  modified columns and `patient_id` are returned. For a cell that should
  not change, return its original value. Only *post-switch* outcomes may
  be changed: returning a value that differs from the original for an
  endpoint whose readout/event is at or before `switch_time` (a
  pre-switch or already-observed outcome) raises an error, so leave such
  cells at their original value (e.g.
  `ifelse(os > switch_time, new_os, os)`). This argument can also be a
  list of functions that will be executed sequentially. No default
  value.

- ...:

  (optional) named arguments to be passed to one or more of `what`,
  `when`, and `how`. Each argument is routed to every function whose
  formal parameter list contains that name. All arguments must be named,
  and every name must match at least one parameter of at least one
  function in `what`, `when`, or `how`.
