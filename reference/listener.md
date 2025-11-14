# Define a Listener

Define a listener. This is a user-friendly wrapper for the class
constructor `Listener$new()`. Users who are not familiar with the
concept of classes may consider using this wrapper directly.

Listener is an important concept of `TrialSimulator`. Used with a trial
object in a controller, a listener can monitor a running trial to
execute user-defined actions when it determine condition of triggering a
milestone is met. This mechanism allows the package users to focus on
the development of action functions in a simulation.

## Usage

``` r
listener(silent = FALSE)
```

## Arguments

- silent:

  logical. `TRUE` to mute messages.

## Examples

``` r
listener <- listener()
```
