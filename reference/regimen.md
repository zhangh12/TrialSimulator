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
  `new_treatment`. Patients with `NA` in `new_treatment` will be
  skipped. The number of rows in the returned data frame may be smaller
  than the number of patients in the input data frame. This indicates
  that some patients' data will not be modifier. Note that the returned
  object will be passed into function \`how()\`, which is also provide
  by users. No default value.

- when:

  a function determining the time at which a patient switches to another
  treatment regimen, measured from the time of enrollment. It takes
  `patient_data`, a data frame as argument, and returns a data frame of
  two columns `patient_id` and `switch_time` (from `enroll_time`). No
  `NA` is allowed in `switch_time` and the number of rows in the
  returned data frame must equal the number of rows in `patient_data`,
  i.e., switching time must be specified to every patients. Note that
  the returned object will be passed into function \`how()\`, which is
  also provided by users. No default value.

- how:

  a function updating patients' data after treatment switching.

- ...:

  optional arguments for the three functions. No default value.
