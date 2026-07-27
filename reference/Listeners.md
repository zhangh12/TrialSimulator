# Class of Listener

Create a class of listener. A listener monitors the trial while checking
condition of pre-defined milestones. Actions are triggered and executed
automatically.

Public methods in this R6 class are used in developing this package.
Thus, we have to export the whole R6 class which exposures all public
methods. However, only the public methods in the list below are useful
to end users.

- `$add_milestones()` register milestone(s) with the listener.

- `$get_milestone_names()` return names of registered milestones.

**Internal machinery.** The remaining public methods (`$monitor()`,
`$mute()` and `$reset()`) are public only because they are invoked on a
listener object by other components of the package (controllers), which
R6 cannot grant through private members. Users should not call them
directly.

## Value

an `R6Class` generator object; use
[`listener()`](https://zhangh12.github.io/TrialSimulator/reference/listener.md)
to create a listener.

## Methods

### Public methods

- [`Listeners$new()`](#method-Listeners-new)

- [`Listeners$add_milestones()`](#method-Listeners-add_milestones)

- [`Listeners$get_milestone_names()`](#method-Listeners-get_milestone_names)

- [`Listeners$monitor()`](#method-Listeners-monitor)

- [`Listeners$mute()`](#method-Listeners-mute)

- [`Listeners$reset()`](#method-Listeners-reset)

- [`Listeners$clone()`](#method-Listeners-clone)

------------------------------------------------------------------------

### Method `new()`

initialize a listener

#### Usage

    Listeners$new(silent = FALSE)

#### Arguments

- `silent`:

  logical. `TRUE` to mute messages.

------------------------------------------------------------------------

### Method `add_milestones()`

register milestones with listener. Order in `...` matter as they are
scanned and triggered in that order. It is users' responsibility to use
reasonable order when calling this function, otherwise, the result of
`Listeners$monitor()` can be problematic.

#### Usage

    Listeners$add_milestones(...)

#### Arguments

- `...`:

  one or more objects returned from
  [`milestone()`](https://zhangh12.github.io/TrialSimulator/reference/milestone.md).

#### Examples

    listener <- listener()
    interim <- milestone(name = 'interim',
                         when = eventNumber('endpoint', n = 100)
                        )
    final <- milestone(name = 'final',
                       when = calendarTime(time = 24)
                      )
    listener$add_milestones(interim, final)

------------------------------------------------------------------------

### Method `get_milestone_names()`

return names of registered milestones

#### Usage

    Listeners$get_milestone_names()

------------------------------------------------------------------------

### Method `monitor()`

**INTERNAL MACHINERY: DO NOT CALL THIS METHOD DIRECTLY.**

scan, check, and trigger registered milestones. Milestones are triggered
in the order when calling `Listener$add_milestones`.

#### Usage

    Listeners$monitor(trial)

#### Arguments

- `trial`:

  a `Trial` object.

------------------------------------------------------------------------

### Method `mute()`

**INTERNAL MACHINERY: DO NOT CALL THIS METHOD DIRECTLY.**

mute all messages (not including warnings)

#### Usage

    Listeners$mute(silent)

#### Arguments

- `silent`:

  logical.

------------------------------------------------------------------------

### Method `reset()`

**INTERNAL MACHINERY: DO NOT CALL THIS METHOD DIRECTLY.**

reset all milestones registered to the listener. Usually, this is called
before a controller can run additional replicates of simulation.

#### Usage

    Listeners$reset()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Listeners$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
##

## ------------------------------------------------
## Method `Listeners$add_milestones`
## ------------------------------------------------

listener <- listener()
interim <- milestone(name = 'interim',
                     when = eventNumber('endpoint', n = 100)
                    )
final <- milestone(name = 'final',
                   when = calendarTime(time = 24)
                  )
listener$add_milestones(interim, final)
#> A milestone <interim> is registered. 
#> A milestone <final> is registered. 
```
