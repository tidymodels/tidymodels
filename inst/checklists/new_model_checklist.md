* [ ] Create the main modeling function (e.g. `nearest_neighbors()`). 

* [ ] Register the model and a mode in the parsnip model “database” with `parsnip::set_new_model()` and `parsnip::set_mode_mode()`. [(example)](https://github.com/tidymodels/parsnip/blob/c54f07b7e1f7ce164aab8f95bc7b1356b68558c8/R/proportional_hazards_data.R)


* [ ] Create an `update()` method.

* [ ] _[new tuning parameters]_ Make sure that there are not existing argument names that can be used.

* [ ] _[new tuning parameters]_ Are there possible engine arguments that users will tune? If so, document them and plan for their use.

* [ ] _[new tuning parameters]_ Make dials functions for main or engine parameters, if needed. 


* [ ] Write unit tests for the basic modeling function (independent of the engines). 

* [ ] _[in parsnip]_ Add a pkgdown entry

* [ ] _[in parsnip]_ Add model description to the internal `model_descs` tibble

* [ ] _[in parsnip]_ Run `purrr::map(parsnip:::extensions(), ~ library(.x, character.only = TRUE))`

* [ ] _[in parsnip]_ Run `parsnip:::update_model_info_file()` 

* [ ] _[in parsnip]_ Make sure that no entries are removed from `inst/models.tsv`.

* [ ] Add a documentation entry in NEWS.md
