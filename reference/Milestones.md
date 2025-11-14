# Class of Milestones

Create a class of milestone. A milestone means the time point to take an
action, e.g., carrying out (futility, interim, final) analysis for
adding/removing arms, or stopping a trial early. It can also be any more
general time point where trial data is used in decision making or
adaptation. For example, one can define a milestone for changing
randomization scheme, sample size re-assessment, trial duration
extension etc.

Public methods in this R6 class are used in developing this package.
Thus, we have to export the whole R6 class which exposures all public
methods. However, none of the public methods on this page is useful to
end users. Instead, refer to the
[vignette](https://zhangh12.github.io/TrialSimulator/articles/conditionSystem.html)
to learn how to define milestones when performing simulation using
`TrialSimulator`.

## Methods

### Public methods

- [`Milestones$new()`](#method-Milestones-new)

- [`Milestones$get_name()`](#method-Milestones-get_name)

- [`Milestones$get_type()`](#method-Milestones-get_type)

- [`Milestones$get_trigger_condition()`](#method-Milestones-get_trigger_condition)

- [`Milestones$get_action()`](#method-Milestones-get_action)

- [`Milestones$set_dry_run()`](#method-Milestones-set_dry_run)

- [`Milestones$execute_action()`](#method-Milestones-execute_action)

- [`Milestones$get_trigger_status()`](#method-Milestones-get_trigger_status)

- [`Milestones$reset()`](#method-Milestones-reset)

- [`Milestones$trigger_milestone()`](#method-Milestones-trigger_milestone)

- [`Milestones$mute()`](#method-Milestones-mute)

- [`Milestones$clone()`](#method-Milestones-clone)

------------------------------------------------------------------------

### Method `new()`

initialize milestone

#### Usage

    Milestones$new(name, type = name, trigger_condition, action = doNothing, ...)

#### Arguments

- `name`:

  character. Name of milestone.

- `type`:

  character vector. Milestone type(s) (futility, interim, final), a
  milestone can be of multiple types. This is for information purpose so
  can be any string.

- `trigger_condition`:

  function to check if this milestone should trigger. See vignette
  `Condition System for Triggering Milestones in a Trial`.

- `action`:

  function to execute when the milestone triggers.

- `...`:

  (optional) arguments of `action`.

------------------------------------------------------------------------

### Method `get_name()`

return name of milestone

#### Usage

    Milestones$get_name()

------------------------------------------------------------------------

### Method `get_type()`

return type(s) of milestone

#### Usage

    Milestones$get_type()

------------------------------------------------------------------------

### Method `get_trigger_condition()`

return trigger_condition function

#### Usage

    Milestones$get_trigger_condition()

------------------------------------------------------------------------

### Method `get_action()`

return action function

#### Usage

    Milestones$get_action()

------------------------------------------------------------------------

### Method `set_dry_run()`

set if dry run should be carried out for the milestone. For more
details, refer to `Controller::run`.

#### Usage

    Milestones$set_dry_run(dry_run)

#### Arguments

- `dry_run`:

  logical.

------------------------------------------------------------------------

### Method `execute_action()`

execute action function

#### Usage

    Milestones$execute_action(trial)

#### Arguments

- `trial`:

  a `Trial` object.

------------------------------------------------------------------------

### Method `get_trigger_status()`

return trigger status

#### Usage

    Milestones$get_trigger_status()

------------------------------------------------------------------------

### Method `reset()`

reset an milestone so that it can be triggered again. Usually, this is
called before the controller of a trial can run additional replicates of
simulation.

#### Usage

    Milestones$reset()

------------------------------------------------------------------------

### Method `trigger_milestone()`

trigger an milestone (always TRUE) and execute action accordingly. It
calls Trial\$get_data_lock_time() to lock data based on conditions
implemented in Milestones\$trigger_condition. If time that meets the
condition cannot be found, Trial\$get_data_lock_time() will throw an
error and stop the program. This means that user needs to adjust their
trigger_condition (e.g., target number of events (target_n_events) is
impossible to reach).

#### Usage

    Milestones$trigger_milestone(trial)

#### Arguments

- `trial`:

  a `Trial` object.

------------------------------------------------------------------------

### Method `mute()`

mute all messages (not including warnings)

#### Usage

    Milestones$mute(silent)

#### Arguments

- `silent`:

  logical.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Milestones$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
