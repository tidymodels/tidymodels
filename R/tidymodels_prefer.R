#' Resolve conflicts between tidymodels packages and others
#'
#' `tidymodels_prefer()` uses the `conflicted` package to handle common
#' conflicts with tidymodels and other packages.
#'
#' The list of conflicts includes several dozen that are known issues with other
#' packages.
#'
#' Note that the \pkg{conflicted} package is used to manage which packages take
#' precedence. Using `tidymodels_prefer()` will turn on general conflict
#' resolution during the R session.
#'
#' @param quiet If `TRUE`, all output will be suppressed
#' @seealso [tidymodels_conflicts()]
#' @export
#' @examples
#' tidymodels_prefer(quiet = FALSE)
tidymodels_prefer <- function(quiet = TRUE) {
  res <-
    utils::capture.output(
      {
        conflicted::conflict_prefer("refit", winner = "agua", quiet = quiet)
        conflicted::conflict_prefer("explain", winner = "DALEX", quiet = quiet)
        conflicted::conflict_prefer("Laplace", winner = "dials", quiet = quiet)
        conflicted::conflict_prefer(
          "max_rules",
          winner = "dials",
          quiet = quiet
        )
        conflicted::conflict_prefer(
          "neighbors",
          winner = "dials",
          quiet = quiet
        )
        conflicted::conflict_prefer("prune", winner = "dials", quiet = quiet)
        conflicted::conflict_prefer(
          "smoothness",
          winner = "dials",
          quiet = quiet
        )
        conflicted::conflict_prefer("collapse", winner = "dplyr", quiet = quiet)
        conflicted::conflict_prefer("combine", winner = "dplyr", quiet = quiet)
        conflicted::conflict_prefer("filter", winner = "dplyr", quiet = quiet)
        conflicted::conflict_prefer("rename", winner = "dplyr", quiet = quiet)
        conflicted::conflict_prefer("select", winner = "dplyr", quiet = quiet)
        conflicted::conflict_prefer("slice", winner = "dplyr", quiet = quiet)
        conflicted::conflict_prefer("%+%", winner = "ggplot2", quiet = quiet)
        conflicted::conflict_prefer("margin", winner = "ggplot2", quiet = quiet)
        conflicted::conflict_prefer("bart", winner = "parsnip", quiet = quiet)
        conflicted::conflict_prefer("fit", winner = "parsnip", quiet = quiet)
        conflicted::conflict_prefer("mars", winner = "parsnip", quiet = quiet)
        conflicted::conflict_prefer("pls", winner = "parsnip", quiet = quiet)
        conflicted::conflict_prefer("cross", winner = "purrr", quiet = quiet)
        conflicted::conflict_prefer("invoke", winner = "purrr", quiet = quiet)
        conflicted::conflict_prefer("map", winner = "purrr", quiet = quiet)
        conflicted::conflict_prefer(
          "discretize",
          winner = "recipes",
          quiet = quiet
        )
        conflicted::conflict_prefer("step", winner = "recipes", quiet = quiet)
        conflicted::conflict_prefer("update", winner = "recipes", quiet = quiet)
        conflicted::conflict_prefer(
          "populate",
          winner = "rsample",
          quiet = quiet
        )
        conflicted::conflict_prefer("rescale", winner = "scales", quiet = quiet)
        conflicted::conflict_prefer(
          "step_downsample",
          winner = "themis",
          quiet = quiet
        )
        conflicted::conflict_prefer(
          "step_upsample",
          winner = "themis",
          quiet = quiet
        )
        conflicted::conflict_prefer("expand", winner = "tidyr", quiet = quiet)
        conflicted::conflict_prefer("extract", winner = "tidyr", quiet = quiet)
        conflicted::conflict_prefer("pack", winner = "tidyr", quiet = quiet)
        conflicted::conflict_prefer("unpack", winner = "tidyr", quiet = quiet)
        conflicted::conflict_prefer(
          "parameters",
          winner = "tune",
          quiet = quiet
        )
        conflicted::conflict_prefer("tune", winner = "tune", quiet = quiet)
        conflicted::conflict_prefer(
          "get_weights",
          winner = "yardstick",
          quiet = quiet
        )
        conflicted::conflict_prefer(
          "precision",
          winner = "yardstick",
          quiet = quiet
        )
        conflicted::conflict_prefer(
          "recall",
          winner = "yardstick",
          quiet = quiet
        )
        conflicted::conflict_prefer("spec", winner = "yardstick", quiet = quiet)

        conflicted::conflict_prefer(
          "update",
          winner = "recipes",
          loser = "Matrix",
          quiet = quiet
        )
      },
      type = "message"
    )
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
