# Class of Regimens

Create a class of regimen. A regimen defines the rules to select
treatments for patients switch, to determine the time of switching, and
to update patients' endpoint data.

## Methods

### Public methods

- [`Regimens$new()`](#method-Regimens-new)

- [`Regimens$get_number_treatment_allocator()`](#method-Regimens-get_number_treatment_allocator)

- [`Regimens$get_treatment_allocator()`](#method-Regimens-get_treatment_allocator)

- [`Regimens$get_number_time_selector()`](#method-Regimens-get_number_time_selector)

- [`Regimens$get_time_selector()`](#method-Regimens-get_time_selector)

- [`Regimens$get_number_data_modifier()`](#method-Regimens-get_number_data_modifier)

- [`Regimens$get_data_modifier()`](#method-Regimens-get_data_modifier)

- [`Regimens$get_treatment_allocator_args()`](#method-Regimens-get_treatment_allocator_args)

- [`Regimens$get_time_selector_args()`](#method-Regimens-get_time_selector_args)

- [`Regimens$get_data_modifier_args()`](#method-Regimens-get_data_modifier_args)

- [`Regimens$get_earliest_crossover_calendar_time()`](#method-Regimens-get_earliest_crossover_calendar_time)

- [`Regimens$append_triplet()`](#method-Regimens-append_triplet)

- [`Regimens$clone()`](#method-Regimens-clone)

------------------------------------------------------------------------

### Method `new()`

initialize regimen

#### Usage

    Regimens$new(what, when, how, ..., earliest_crossover_calendar_time = 0)

#### Arguments

- `what`:

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

- `when`:

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

- `how`:

  a function updating patients' data after treatment switching. Only
  modified columns and `patient_id` are returned. For a cell that should
  not change, return its original value. Only *post-switch* outcomes may
  be changed: returning a value that differs from the original for an
  endpoint whose readout/event is at or before `switch_time` raises an
  error. This argument can also be a list of functions that will be
  executed sequentially. No default value.

- `...`:

  (optional) named arguments routed to one or more of `what`, `when`,
  and `how`.

- `earliest_crossover_calendar_time`:

  numeric. The earliest calendar time at which the triplet(s) may take
  effect. `0` (default) is the classic enrollment-time regimen, applied
  from the first enrollment. A positive value marks the triplet(s) as a
  milestone-triggered crossover (eligibility filtering, switch-time
  validation and the post-switch data mask). This is set internally by
  `trial$crossover()`; it is not a user argument of
  [`regimen()`](https://zhangh12.github.io/TrialSimulator/reference/regimen.md).

------------------------------------------------------------------------

### Method `get_number_treatment_allocator()`

return number of treatment allocators for regimen

#### Usage

    Regimens$get_number_treatment_allocator()

------------------------------------------------------------------------

### Method `get_treatment_allocator()`

return user-defined new treatment for a patient

#### Usage

    Regimens$get_treatment_allocator(index = NULL)

#### Arguments

- `index`:

  integer. Index of allocator. Return all allocators if `NULL`.

------------------------------------------------------------------------

### Method `get_number_time_selector()`

return number of time selector for regimen

#### Usage

    Regimens$get_number_time_selector()

------------------------------------------------------------------------

### Method `get_time_selector()`

return user-defined time selector

#### Usage

    Regimens$get_time_selector(index = NULL)

#### Arguments

- `index`:

  integer. Index of selector. Return all selectors if `NULL`.

------------------------------------------------------------------------

### Method `get_number_data_modifier()`

return number of data modifier for regimen

#### Usage

    Regimens$get_number_data_modifier()

------------------------------------------------------------------------

### Method `get_data_modifier()`

return user-defined endpoint data modifier

#### Usage

    Regimens$get_data_modifier(index = NULL)

#### Arguments

- `index`:

  integer. Index of selector. Return all modifiers if `NULL`.

------------------------------------------------------------------------

### Method `get_treatment_allocator_args()`

return pre-bound arguments for the i-th treatment allocator

#### Usage

    Regimens$get_treatment_allocator_args(index)

#### Arguments

- `index`:

  integer.

------------------------------------------------------------------------

### Method `get_time_selector_args()`

return pre-bound arguments for the i-th time selector

#### Usage

    Regimens$get_time_selector_args(index)

#### Arguments

- `index`:

  integer.

------------------------------------------------------------------------

### Method `get_data_modifier_args()`

return pre-bound arguments for the i-th data modifier

#### Usage

    Regimens$get_data_modifier_args(index)

#### Arguments

- `index`:

  integer.

------------------------------------------------------------------------

### Method `get_earliest_crossover_calendar_time()`

return the earliest crossover calendar time of triplet(s)

#### Usage

    Regimens$get_earliest_crossover_calendar_time(index = NULL)

#### Arguments

- `index`:

  integer. Index of triplet. Return all if `NULL`.

------------------------------------------------------------------------

### Method `append_triplet()`

append one more triplet to the regimen. Used by milestone-triggered
crossover to stack a new `what`/`when`/`how` (with its own
`earliest_crossover_calendar_time`) onto an existing regimen without
overwriting earlier triplets. Triplets are executed in append order.

#### Usage

    Regimens$append_triplet(
      what,
      when,
      how,
      ...,
      earliest_crossover_calendar_time = 0
    )

#### Arguments

- `what, when, how`:

  see
  [`regimen()`](https://zhangh12.github.io/TrialSimulator/reference/regimen.md).

- `...`:

  (optional) named arguments routed to `what`, `when`, and/or `how`.

- `earliest_crossover_calendar_time`:

  numeric. Earliest calendar time for the appended triplet. A positive
  value marks it as a crossover.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Regimens$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
