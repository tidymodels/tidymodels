All these items should be added to *parsnip*, unless otherwise specified.

* [ ] Create the main modeling function (e.g. `nearest_neighbors()`). When choosing argument names, check for existing ones across other model types which could be reused, e.g., `trees` for the number of trees is used across different tree-based models like `rand_forest()` and `boost_tree()`.

* [ ] Register the model and a mode in the parsnip model “database” with `parsnip::set_new_model()` and `parsnip::set_mode_mode()`. [(example)](https://github.com/tidymodels/parsnip/blob/c54f07b7e1f7ce164aab8f95bc7b1356b68558c8/R/proportional_hazards_data.R)

* [ ] Create an `update()` method.

* [ ] Add parameter objects for main parameters to *dials*, if needed. See [How to create a tuning parameter function](https://www.tidymodels.org/learn/develop/parameters/) for more details.

* [ ] Write unit tests for the basic modeling function (independent of the engines). 

* [ ] Add a pkgdown entry.

* [ ] Add model description to the internal `model_descs` tibble.

* [ ] Run `purrr::map(parsnip:::extensions(), ~ library(.x, character.only = TRUE))`.

* [ ] Run `parsnip:::update_model_info_file()`.

* [ ] Make sure that no entries are removed from `inst/models.tsv`.

* [ ] Add a documentation entry in NEWS.md
