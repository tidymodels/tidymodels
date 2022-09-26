* [ ] Write the initial functions that will populate the parsnip model “database”. For example, 	`parsnip::set_fit()` and so on. [(example)](https://github.com/tidymodels/poissonreg/blob/main/R/poisson_reg_data.R#L7:L67)

* [ ] Make sure that `parsnip::set_dependency()` has an entry for the package that contains the engine definition as well as the actual dependencies [(example)](https://github.com/tidymodels/poissonreg/blob/main/R/poisson_reg_data.R#L72:L73)

* [ ] _[not in parsnip]_ Put the `set_*()` calls in a wrapper function that will register the model with parsnip. [(example)](https://github.com/tidymodels/rules/blob/main/R/rule_fit_data.R#L1)

* [ ] _[not in parsnip]_ Add the registration functions to `.onLoad()` in the `zzz.R` file. [(example)](https://github.com/tidymodels/rules/blob/main/R/zzz.R#L7:L18)

* [ ] Optionally create a `translate()` method to handle special cases. [(example)](https://github.com/tidymodels/parsnip/blob/c54f07b7e1f7ce164aab8f95bc7b1356b68558c8/R/proportional_hazards.R#L85:L98')

* [ ] Write unit tests for the engine-specific features. 

* [ ] Write the engine-specific documentation or open an issue in parsnip for the tidymodels folks to do. 

* [ ] Add a documentation entry in NEWS.md
