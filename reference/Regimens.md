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

- [`Regimens$clone()`](#method-Regimens-clone)

------------------------------------------------------------------------

### Method `new()`

initialize regimen

#### Usage

    Regimens$new(what, when, how)

#### Arguments

- `what`:

  a function determining whether patients' data would be updated due to
  switching treatment. It takes `patient_data`, a data frame as
  argument, and returns a data frame of two columns `patient_id` and
  `new_treatment`. Patients with `NA` in `new_treatment` will be
  skipped. The number of rows in the returned data frame may be smaller
  than the number of patients in the input data frame. This indicates
  that some patients' data will not be modifier. Note that the returned
  object will be passed into function \`how()\`, which is also provide
  by users. This argument can also be a list of functions that will be
  executed sequentially. No default value.

- `when`:

  a function determining the time at which a patient switches to another
  treatment regimen, measured from the time of enrollment. It takes
  `patient_data`, a data frame as argument, and returns a data frame of
  two columns `patient_id` and `switch_time` (from `enroll_time`). No
  `NA` is allowed in `switch_time` and the number of rows in the
  returned data frame must equal the number of rows in `patient_data`,
  i.e., switching time must be specified to every patients. Note that
  the returned object will be passed into function \`how()\`, which is
  also provided by users. This argument can also be a list of functions
  that will be executed sequentially. No default value.

- `how`:

  a function updating patients' data after treatment switching. Only
  modified columns and `patient_id` are returned. A cell will be omitted
  if `NA`, meaning no change to that patient for the endpoint or other
  variables. Equivalently, users can also fill the cell with its
  original value. This argument can also be a list of functions that
  will be executed sequentially. No default value.

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

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Regimens$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
