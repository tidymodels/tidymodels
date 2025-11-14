# Resolve conflicts between tidymodels packages and others

`tidymodels_prefer()` uses the `conflicted` package to handle common
conflicts with tidymodels and other packages.

## Usage

``` r
tidymodels_prefer(quiet = TRUE)
```

## Arguments

- quiet:

  If `TRUE`, all output will be suppressed

## Details

The list of conflicts includes several dozen that are known issues with
other packages.

Note that the conflicted package is used to manage which packages take
precedence. Using `tidymodels_prefer()` will turn on general conflict
resolution during the R session.

## See also

[`tidymodels_conflicts()`](https://tidymodels.tidymodels.org/dev/reference/tidymodels_conflicts.md)

## Examples

``` r
tidymodels_prefer(quiet = FALSE)
#> [conflicted] Will prefer agua::refit over any other package.
#> [conflicted] Will prefer DALEX::explain over any other package.
#> [conflicted] Will prefer dials::Laplace over any other package.
#> [conflicted] Will prefer dials::max_rules over any other package.
#> [conflicted] Will prefer dials::neighbors over any other package.
#> [conflicted] Will prefer dials::prune over any other package.
#> [conflicted] Will prefer dials::smoothness over any other package.
#> [conflicted] Will prefer dplyr::collapse over any other package.
#> [conflicted] Will prefer dplyr::combine over any other package.
#> [conflicted] Will prefer dplyr::filter over any other package.
#> [conflicted] Will prefer dplyr::rename over any other package.
#> [conflicted] Will prefer dplyr::select over any other package.
#> [conflicted] Will prefer dplyr::slice over any other package.
#> [conflicted] Will prefer ggplot2::`%+%` over any other package.
#> [conflicted] Will prefer ggplot2::margin over any other package.
#> [conflicted] Will prefer parsnip::bart over any other package.
#> [conflicted] Will prefer parsnip::fit over any other package.
#> [conflicted] Will prefer parsnip::mars over any other package.
#> [conflicted] Will prefer parsnip::pls over any other package.
#> [conflicted] Will prefer purrr::cross over any other package.
#> [conflicted] Will prefer purrr::invoke over any other package.
#> [conflicted] Will prefer purrr::map over any other package.
#> [conflicted] Will prefer recipes::discretize over any other package.
#> [conflicted] Will prefer recipes::step over any other package.
#> [conflicted] Will prefer recipes::update over any other package.
#> [conflicted] Will prefer rsample::populate over any other package.
#> [conflicted] Will prefer scales::rescale over any other package.
#> [conflicted] Will prefer themis::step_downsample over any other
#> package.
#> [conflicted] Will prefer themis::step_upsample over any other package.
#> [conflicted] Will prefer tidyr::expand over any other package.
#> [conflicted] Will prefer tidyr::extract over any other package.
#> [conflicted] Will prefer tidyr::pack over any other package.
#> [conflicted] Will prefer tidyr::unpack over any other package.
#> [conflicted] Will prefer tune::parameters over any other package.
#> [conflicted] Will prefer tune::tune over any other package.
#> [conflicted] Will prefer yardstick::get_weights over any other
#> package.
#> [conflicted] Will prefer yardstick::precision over any other package.
#> [conflicted] Will prefer yardstick::recall over any other package.
#> [conflicted] Will prefer yardstick::spec over any other package.
#> [conflicted] Removing existing preference.
#> [conflicted] Will prefer recipes::update over Matrix::update.
#> ── Conflicts ─────────────────────────────────── tidymodels_prefer() ──
#> 
```
