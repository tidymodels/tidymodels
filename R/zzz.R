.onAttach <- function(...) {
  needed <- core[!is_attached(core)]
  if (length(needed) == 0) {
    return()
  }

  tidymodels_attach()

  if (!"package:conflicted" %in% search()) {
    x <- tidymodels_conflicts()
    msg(tidymodels_conflict_message(x), startup = TRUE)
  }
}

is_attached <- function(x) {
  paste0("package:", x) %in% search()
}
