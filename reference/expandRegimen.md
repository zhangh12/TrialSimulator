# Expand regimen trajectory into long format

Expands the compact `regimen_trajectory` column in locked data (returned
by `trial$get_locked_data()`) into a long-format data frame with one row
per regimen segment per patient.

The `regimen_trajectory` column stores each patient's treatment history
as a semicolon-separated string of `"name\@time"` entries, e.g.
`"placebo\@0;low dose\@8.5"`. `expandRegimen` parses this into two
additional columns:

- `regimen` — name of the treatment regimen

- `switch_time_from_enrollment` — time from enrollment at which the
  patient switched to this regimen

The `regimen_trajectory` column is dropped from the result.

## Usage

``` r
expandRegimen(data)
```

## Arguments

- data:

  a data frame returned by `trial$get_locked_data()`.

## Value

a long-format data frame: one row per regimen segment per patient, with
`regimen` and `switch_time_from_enrollment` appended and
`regimen_trajectory` removed.

## Examples

``` r
if (FALSE) { # \dontrun{
locked <- trial$get_locked_data('final')
long   <- expandRegimen(locked)
} # }
```
