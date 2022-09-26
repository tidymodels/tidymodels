* [ ] Create the main modeling function (e.g. `nearest_neighbors()`). 

* [ ] Write the initial functions that will populate the parsnip model “database”. For example, `parsnip::set_mode()` and so on. [(example)](https://github.com/tidymodels/parsnip/blob/c54f07b7e1f7ce164aab8f95bc7b1356b68558c8/R/proportional_hazards_data.R)

* [ ] _[not in parsnip]_ Put the `set_*()` calls in a wrapper function that will register the model with parsnip. [(example)](https://github.com/tidymodels/rules/blob/main/R/rule_fit_data.R#L1)

* [ ] _[not in parsnip]_ Add the registration functions to `.onLoad()` in the `zzz.R` file. [(example)](https://github.com/tidymodels/rules/blob/main/R/zzz.R#L7:L18)

* [ ] Create secondary methods such as `update()`, `tuneable()` and so on. 

* [ ] _[new tuning parameters]_ Make sure that there are not existing argument names that can be used.

* [ ] _[new tuning parameters]_ Are there possible engine functions that users will tune? If so, document them and plan for their use.

* [ ] _[new tuning parameters]_ Make dials functions for main or engine parameters, if needed. 

* [ ] Optionally create a `translate()` method to handle special cases. [(example)](https://github.com/tidymodels/parsnip/blob/c54f07b7e1f7ce164aab8f95bc7b1356b68558c8/R/proportional_hazards.R#L85:L98')

* [ ] Write unit tests for the basic modeling function (independent of the engines). 

* [ ] _[in parsnip]_ Add a pkgdown entry

* [ ] _[in parsnip]_ Add model description to the internal `model_descs` tibble

* [ ] _[in parsnip]_ Run `purrr::map(parsnip:::extensions, ~ library(.x, character.only = TRUE))`

* [ ] _[in parsnip]_ Run `parsnip:::update_model_info_file()` 

* [ ] _[in parsnip]_ Make sure that no entries are removed from `inst/models.tsv`.

* [ ] Add a documentation entry in NEWS.md
