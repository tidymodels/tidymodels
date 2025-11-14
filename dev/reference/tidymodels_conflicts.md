# Conflicts between the tidymodels and other packages

This function lists all the conflicts between packages in the tidymodels
and other packages that you have loaded.

## Usage

``` r
tidymodels_conflicts()
```

## Details

There are four conflicts that are deliberately ignored: `intersect`,
`union`, `setequal`, and `setdiff` from dplyr. These functions make the
base equivalents generic, so shouldn't negatively affect any existing
code.

To manage conflicts, you can use the conflicted package. To prefer
tidymodels functions over other functions, use
[`tidymodels_prefer()`](https://tidymodels.tidymodels.org/dev/reference/tidymodels_prefer.md).

## See also

[`tidymodels_prefer()`](https://tidymodels.tidymodels.org/dev/reference/tidymodels_prefer.md)

## Examples

``` r
tidymodels_conflicts()
#> ── Conflicts ──────────────────────────────── tidymodels_conflicts() ──
#> ✖ purrr::discard() masks scales::discard()
#> ✖ dplyr::filter()  masks stats::filter()
#> ✖ dplyr::lag()     masks stats::lag()
#> ✖ recipes::step()  masks stats::step()
```
