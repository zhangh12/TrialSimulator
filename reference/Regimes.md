# Class of Regimes

Create a class of regime. A regime defines the rules to select
treatments for patients switch, to determine the time of switching, and
to update patients' endpoint data.

## Methods

### Public methods

- [`Regimes$new()`](#method-Regimes-new)

- [`Regimes$get_treatment_selector()`](#method-Regimes-get_treatment_selector)

- [`Regimes$get_time_selector()`](#method-Regimes-get_time_selector)

- [`Regimes$get_data_modifier()`](#method-Regimes-get_data_modifier)

- [`Regimes$clone()`](#method-Regimes-clone)

------------------------------------------------------------------------

### Method `new()`

initialize regime

#### Usage

    Regimes$new(what, when, how)

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
  by users. No default value.

- `when`:

  a function determining the time at which a patient switches to another
  treatment regimen, measured from the time of enrollment. It takes
  `patient_data`, a data frame as argument, and returns a data frame of
  two columns `patient_id` and `switch_time` (from `enroll_time`). No
  `NA` is allowed in `switch_time` and the number of rows in the
  returned data frame must equal the number of rows in `patient_data`,
  i.e., switching time must be specified to every patients. Note that
  the returned object will be passed into function \`how()\`, which is
  also provided by users. No default value.

- `how`:

  a function updating patients' data after treatment switching.

- `...`:

  optional arguments for the three functions. No default value.

------------------------------------------------------------------------

### Method `get_treatment_selector()`

return user-defined new treatment for a patient

#### Usage

    Regimes$get_treatment_selector()

------------------------------------------------------------------------

### Method `get_time_selector()`

return user-defined time selector

#### Usage

    Regimes$get_time_selector()

------------------------------------------------------------------------

### Method `get_data_modifier()`

return user-defined endpoint data modifier

#### Usage

    Regimes$get_data_modifier()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Regimes$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
