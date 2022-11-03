For a model denotes as `{model}` with a specific `{engine}`:

* [ ] Make sure that you have the most recent devtools and roxygen2 installed. 

* [ ] Add a new Rmd file to `parsnip/man/rmd` called `{model}_{engine}.Rmd`. [(example)](https://github.com/tidymodels/parsnip/blob/c54f07b7e1f7ce164aab8f95bc7b1356b68558c8/man/rmd/decision_tree_rpart.Rmd)

* [ ] Document...

* [ ] Add templates if none exist for your topic.

* [ ] Add a new Rmd file to `parsnip/R` called `{model}_{engine}.R`. [(example)](https://github.com/tidymodels/parsnip/blob/c54f07b7e1f7ce164aab8f95bc7b1356b68558c8/R/decision_tree_rpart.R)

* [ ] Make sure to have `@includeRmd man/rmd/{model}_{engine}.md details`

* [ ] Make sure to have `@name details_{model}_{engine}`.

* [ ] Use `@keywords internal`.

* [ ] Make sure that the packages in `parsnip:::extensions()` are installed.

* [ ] Make sure that the `rmd_pkgs` listed in `parsnip/man/rmd/aaa.Rmd` are also installed. [(example)](https://github.com/tidymodels/parsnip/blob/main/man/rmd/aaa.Rmd#L20:L21)


* [ ] Restart your R session (with `Shift + Cmd + 0` on MacOS)

* [ ] Run `parsnip:::knit_engine_docs()`. There is an optional `pattern` argument to filter which documentation files are being processed. 

* [ ] Check output for errors. 

* [ ] `devtools::document()`.

* [ ] Reinstall parsnip and check that the main modeling function has an entry for your engine. If it does not, was the model added to the parsnip model database? 