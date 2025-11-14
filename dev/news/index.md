# Changelog

## tidymodels (development version)

## tidymodels 1.4.1

CRAN release: 2025-09-08

- Bug fix for omitted attached packages.

## tidymodels 1.4.0

CRAN release: 2025-09-05

- Updated versions

- Transition from the magrittr pipe to the base R pipe.

- Added the tailor package.

## tidymodels 1.3.0

CRAN release: 2025-02-21

- Updated versions

- Prefer `DALEX::explains()` over `dplyr::explains()` when you use
  tidymodels_prefer()
  [\#113](https://github.com/tidymodels/tidymodels/issues/113)

- Prefer [`recipes::update()`](https://rdrr.io/r/stats/update.html) over
  other [`update()`](https://rdrr.io/r/stats/update.html) functions.

## tidymodels 1.2.0

CRAN release: 2024-03-25

- Updated versions

## tidymodels 1.1.1

CRAN release: 2023-08-24

- Updated versions

## tidymodels 1.1.0

CRAN release: 2023-05-01

- Updated
  [`tidymodels_prefer()`](https://tidymodels.tidymodels.org/dev/reference/tidymodels_prefer.md)
  for new conflicts.

- Updated versions

## tidymodels 1.0.0

CRAN release: 2022-07-13

- Updated versions

## tidymodels 0.2.0

CRAN release: 2022-03-19

- Updated versions

- Updated to testthat 3e

## tidymodels 0.1.4

CRAN release: 2021-10-01

- Updated versions

- Added five package startup messages to point users to helpful
  documentation. These startup messages are chosen via modulo of the
  time, *not* using the RNG, so these messages will not affect
  reproducibility
  ([\#59](https://github.com/tidymodels/tidymodels/issues/59)).

- Added R Markdown template for a tidymodels analysis
  ([\#58](https://github.com/tidymodels/tidymodels/issues/58)).

## tidymodels 0.1.3

CRAN release: 2021-04-19

- Re-licensed package from GPL-3 to MIT. See [consent from copyright
  holders here](https://github.com/tidymodels/tidymodels/issues/51).

- Updated versions

- Added
  [`tidymodels_prefer()`](https://tidymodels.tidymodels.org/dev/reference/tidymodels_prefer.md)
  to help resolve name conflicts with other packages.

- Added `workflowsets` as a core package.

## tidymodels 0.1.2

CRAN release: 2020-11-22

- Updated versions

- Removed `crayon` dependency.

- Updated tags.

## tidymodels 0.1.1

CRAN release: 2020-07-14

- Updated versions

- `tidyposterior`, `pillar`, `tidypredict`, `tidytext` were removed from
  the imports list due to an `R CMD check` warning about the number of
  imports.

- `tidyr` and `modeldata` were added to the imports list.

## tidymodels 0.0.4

- Updated versions

- Added `workflows`, and `tune` to the core package list.

- Moved away from testing via `travis`

## tidymodels 0.0.3

CRAN release: 2019-10-04

- Updated versions

## tidymodels 0.0.2

CRAN release: 2018-11-27

- Added `parsnip` and `dials` to the core package list.

- Package requirements bumped to current versions.

## tidymodels 0.0.1

CRAN release: 2018-07-27

First CRAN version.
