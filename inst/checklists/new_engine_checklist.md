* [ ] Write the initial functions that will populate the parsnip model “database”. For example, 	`parsnip::set_fit()` and so on. [(example)](https://github.com/tidymodels/poissonreg/blob/main/R/poisson_reg_data.R#L7:L67)

* [ ] Make sure that `parsnip::set_dependency()` has an entry for the package that contains the engine definition as well as the actual dependencies [(example)](https://github.com/tidymodels/poissonreg/blob/main/R/poisson_reg_data.R#L72:L73)

* [ ] _[if not in parsnip]_ Put the `set_*()` calls in a wrapper function that will register the model with parsnip. [(example)](https://github.com/tidymodels/rules/blob/main/R/rule_fit_data.R#L1)

* [ ] _[if not in parsnip]_ Add the registration functions to `.onLoad()` in the `zzz.R` file. [(example)](https://github.com/tidymodels/rules/blob/main/R/zzz.R#L7:L18)

* [ ] Optionally create methods for `translate()`, `check_args()` and `tunable()` to handle engine-specific cases. These `.<model name>` methods should go into parsnip, even if the engine is defined in an extension package. [(example)](https://github.com/tidymodels/parsnip/blob/c54f07b7e1f7ce164aab8f95bc7b1356b68558c8/R/proportional_hazards.R#L85:L98')

* [ ] Write unit tests for the engine-specific features. 

* [ ] Write unit tests for model-specific features which require an engine if parsnip does not contain _any_ engines for the model.

* [ ] Write the engine-specific documentation or open an issue in parsnip for the tidymodels folks to do. 

* [ ] If the engine is added in parsnip, update `vignettes/articles/Examples.Rmd`. If the engine is added in an extension package: is there a corresponding place that needs updating (e.g., if the README lists available models/engines)?

* [ ] Add a documentation entry in NEWS.md
