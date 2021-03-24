.onAttach <- function(...) {
  needed <- core[!is_attached(core)]
  if (length(needed) == 0)
    return()

  tidymodels_attach()
  tidymodels_conflict_management(FALSE)

}

is_attached <- function(x) {
  paste0("package:", x) %in% search()
}
