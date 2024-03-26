For a model denotes as `{model}` with a specific `{engine}`:

* [ ] Make sure that you have the most recent devtools and roxygen2 installed. 

* [ ] Add a new Rmd file to `parsnip/man/rmd` called `{model}_{engine}.Rmd`. [(example)](https://github.com/tidymodels/parsnip/blob/c54f07b7e1f7ce164aab8f95bc7b1356b68558c8/man/rmd/decision_tree_rpart.Rmd)

* [ ] Write the actual documentation inside the `{model}_{engine}.Rmd`.

* [ ] Make use of existing templates for topics like preprocessing requirements and case weights. Add templates if appropriate.

* [ ] Add a new R file to `parsnip/R` called `{model}_{engine}.R`. [(example)](https://github.com/tidymodels/parsnip/blob/c54f07b7e1f7ce164aab8f95bc7b1356b68558c8/R/decision_tree_rpart.R)

* [ ] Make sure to have `@includeRmd man/rmd/{model}_{engine}.md details`

* [ ] Make sure to have `@name details_{model}_{engine}`.

* [ ] Use `@keywords internal`.

* [ ] Make sure that the packages in `parsnip:::extensions()` are installed.

* [ ] Make sure that the `rmd_pkgs` listed in `parsnip/man/rmd/aaa.Rmd` are also installed. [(example)](https://github.com/tidymodels/parsnip/blob/main/man/rmd/aaa.Rmd#L20:L21)

* [ ] Run `purrr::map(parsnip:::extensions(), ~ library(.x, character.only = TRUE))`.

* [ ] Run `parsnip:::update_model_info_file()`.

* [ ] Make sure that no entries are removed from `inst/models.tsv`.

* [ ] Restart your R session (with `Shift + Cmd + 0` on MacOS)

* [ ] Run `parsnip:::knit_engine_docs()`. There is an optional `pattern` argument to filter which documentation files are being processed. Note that this needs to execute `knit_engine_docs()` from an installed version of parsnip as opposed to parsnip being loaded via `devtools::load_all()`.

* [ ] Check output for errors. 

* [ ] `devtools::document()`.

* [ ] Reinstall parsnip and check that the main modeling function has an entry for your engine. If it does not, was the model added to the parsnip model database? 
