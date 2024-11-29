# Checklist for adding a new engine

* [ ] Write the initial functions that will populate the parsnip model “database”. For example,	`parsnip::set_fit()` and so on. Suggested file name: `{model_name}-data.R`. [[example]](https://github.com/tidymodels/censored/blob/efbb1cf3b2ba8bca5de65acc3c8b665dec35631c/R/decision_tree-data.R#L13-L83)

* [ ] _[extension pkg]_ Make sure that `parsnip::set_dependency()` has an entry for the package that contains the engine definition as well as the actual dependencies. [[example]](https://github.com/tidymodels/censored/blob/efbb1cf3b2ba8bca5de65acc3c8b665dec35631c/R/decision_tree-data.R#L14-L21)

* [ ] _[extension pkg]_ Put the `set_*()` calls in a wrapper function that will register the model with parsnip. Suggested name: `make_{model name}_{engine name}()`. [[example]](https://github.com/tidymodels/censored/blob/efbb1cf3b2ba8bca5de65acc3c8b665dec35631c/R/decision_tree-data.R#L12) 

* [ ] _[extension pkg]_ Add the registration functions to `.onLoad()` in the `zzz.R` file. [(example)](https://github.com/tidymodels/censored/blob/efbb1cf3b2ba8bca5de65acc3c8b665dec35631c/R/zzz.R#L13) 

* [ ] Engine-specific tuning parameters: Necessary parameter objects [[example]](https://github.com/tidymodels/dials/blob/d47dc47f42ad9c190d7f4a1ec85db0e385345ec0/R/param_num_knots.R) should go into *dials* and the `tunable()` method [[example]](https://github.com/tidymodels/parsnip/blob/bdc28548fd46493b52b408b6c1e04e1f80d0abdf/R/tunable.R#L338-L345) should go into parsnip. This needs a tibble which links engine arguments to dials parameter objects [[example]](https://github.com/tidymodels/parsnip/blob/bdc28548fd46493b52b408b6c1e04e1f80d0abdf/R/tunable.R#L206-L215).

* [ ] Optionally create methods for `translate()` and `check_args()` to handle engine-specific cases. These `.{model name}` methods should go into parsnip, even if the engine is defined in an extension package, as they may contain code for various engines distributed across multiple extension packages. [(example)](https://github.com/tidymodels/parsnip/blob/c54f07b7e1f7ce164aab8f95bc7b1356b68558c8/R/proportional_hazards.R#L85:L98')

* [ ] Write unit tests for the engine-specific features. 

* [ ] Write unit tests for model-specific features which require an engine. If parsnip does not contain _any_ engines for the model, put these in the extension package.

* [ ] Write the engine-specific documentation. This documentation always goes into parsnip, regardless of where the engine is defined. See the [README](https://github.com/tidymodels/parsnip/blob/main/inst/README-DOCS.md) for the parsnip documentation system.

* [ ] If the engine is added in parsnip, update `vignettes/articles/Examples.Rmd`. If the engine is added in an extension package: does it have a corresponding article that needs updating? Does the README list available models/engines and needs updating?

* [ ] Add a documentation entry in `NEWS.md`.
