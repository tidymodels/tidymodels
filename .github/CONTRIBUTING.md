# Contributing to tidymodels

For more detailed information about contributing to tidymodels packages, see our [**development contributing guide**](https://www.tidymodels.org/contribute/).

## Documentation

Typos or grammatical errors in documentation may be edited directly using the GitHub web interface, as long as the changes are made in the _source_ file.

*  YES ✅: you edit a roxygen comment in an `.R` file in the `R/` directory.
*  NO 🚫: you edit an `.Rd` file in the `man/` directory.

We use [roxygen2](https://cran.r-project.org/package=roxygen2), with [Markdown syntax](https://cran.r-project.org/web/packages/roxygen2/vignettes/rd-formatting.html), for documentation.

## Code

Before you submit 🎯 a pull request on a tidymodels package, always file an issue and confirm the tidymodels team agrees with your idea and is happy with your basic proposal.

The [tidymodels packages](https://www.tidymodels.org/packages/) work together. Each package contains its own unit tests, while integration tests and other tests using all the packages are contained in [extratests](https://github.com/tidymodels/extratests).

*  For pull requests, we recommend that you [create a fork of this repo](https://usethis.r-lib.org/articles/articles/pr-functions.html) with `usethis::create_from_github()`, and then initiate a new branch with `usethis::pr_init()`.
*  Look at the build status before and after making changes. The `README` contains badges for any continuous integration services used by the package.  
*  New code should follow the tidyverse [style guide](http://style.tidyverse.org). You can use the [styler](https://CRAN.R-project.org/package=styler) package to apply these styles, but please don't restyle code that has nothing to do with your PR.  
*  For user-facing changes, add a bullet to the top of `NEWS.md` below the current development version header describing the changes made followed by your GitHub username, and links to relevant issue(s)/PR(s).
*  We use [testthat](https://cran.r-project.org/package=testthat). Contributions with test cases included are easier to accept.
*  If your contribution spans the use of more than one package, consider building [extratests](https://github.com/tidymodels/extratests) with your changes to check for breakages and/or adding new tests there. Let us know in your PR if you ran these extra tests.

### Code of Conduct

This project is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.
