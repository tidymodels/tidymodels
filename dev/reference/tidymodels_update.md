# Update tidymodels packages

This will check to see if all tidymodels packages (and optionally, their
dependencies) are up-to-date, and will install after an interactive
confirmation.

## Usage

``` r
tidymodels_update(pkg = "tidymodels", recursive = FALSE, ...)
```

## Arguments

- pkg:

  A character string for the model being updated.

- recursive:

  If `TRUE`, will also check all dependencies of tidymodels packages.

- ...:

  Extra arguments to pass to
  [`utils::install.packages()`](https://rdrr.io/r/utils/install.packages.html)

## Value

Nothing is returned but a message is printed to the console about which
packages (if any) should be installed along with code to do so.

## Examples

``` r
if (FALSE) { # \dontrun{
tidymodels_update()
} # }
```
