#' Resolve conflicts between tidymodels packages and others
#'
#' `tidymodels_prefer()` uses the `conflicted` package to handle common
#' conflicts with tidymodels and other packages.
#'
#' The list of conflicts includes about a dozen that are known issues with other
#' packages.
#' @param quiet If `TRUE`, all output will be suppressed
#' @export
#' @examples
#' tidymodels_prefer(quiet = FALSE)
tidymodels_prefer <- function(quiet = TRUE) {
  res <-
    utils::capture.output({
      conflicted::conflict_prefer("filter",     winner = "dplyr",     quiet = quiet)
      conflicted::conflict_prefer("select",     winner = "dplyr",     quiet = quiet)
      conflicted::conflict_prefer("slice",      winner = "dplyr",     quiet = quiet)
      conflicted::conflict_prefer("rename",     winner = "dplyr",     quiet = quiet)
      conflicted::conflict_prefer("neighbors",  winner = "dials",     quiet = quiet)
      conflicted::conflict_prefer("pls",        winner = "plsmod",    quiet = quiet)
      conflicted::conflict_prefer("map",        winner = "purrr",     quiet = quiet)
      conflicted::conflict_prefer("step",       winner = "recipes",   quiet = quiet)
      conflicted::conflict_prefer("tune",       winner = "tune",      quiet = quiet)
      conflicted::conflict_prefer("precision",  winner = "yardstick", quiet = quiet)
      conflicted::conflict_prefer("recall",     winner = "yardstick", quiet = quiet)
    },
    type = "message")
  if (!quiet) {
    header <- cli::rule(
      left = cli::style_bold("Conflicts"),
      right = "tidymodels_prefer()"
    )
    res <- gsub("^\\[conflicted\\] ", "", res)
    res <- paste0(res, collapse = "\n")
    msg(header, startup = TRUE)
    msg(res, startup = TRUE)
  }
  invisible(NULL)
}

