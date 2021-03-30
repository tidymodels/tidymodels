#' Conflicts between the tidymodels and other packages
#'
#' This function lists all the conflicts between packages in the tidymodels
#' and other packages that you have loaded.
#'
#' There are four conflicts that are deliberately ignored: \code{intersect},
#' \code{union}, \code{setequal}, and \code{setdiff} from dplyr. These functions
#' make the base equivalents generic, so shouldn't negatively affect any
#' existing code.
#'
#' @export
#' @examples
#' tidymodels_conflicts()
tidymodels_conflicts <- function() {
  envs <- purrr::set_names(search())
  objs <- invert(lapply(envs, ls_env))

  conflicts <- purrr::keep(objs, ~ length(.x) > 1)

  tidy_names <- paste0("package:", tidymodels_packages())
  conflicts <- purrr::keep(conflicts, ~ any(.x %in% tidy_names))

  conflict_funs <- purrr::imap(conflicts, confirm_conflict)
  conflict_funs <- purrr::compact(conflict_funs)

  structure(conflict_funs, class = "tidymodels_conflicts")
}

tidymodels_conflict_message <- function(x) {
  if (length(x) == 0) return("")

  header <- cli::rule(
    left = cli::style_bold("Conflicts"),
    right = "tidymodels_conflicts()"
  )

  pkgs <- x %>% purrr::map(~ gsub("^package:", "", .))
  others <- pkgs %>% purrr::map(`[`, -1)
  other_calls <- purrr::map2_chr(
    others, names(others),
    ~ paste0(cli::col_blue(.x), "::", .y, "()", collapse = ", ")
  )

  winner <- pkgs %>% purrr::map_chr(1)
  funs <- format(paste0(cli::col_blue(winner), "::", cli::col_green(paste0(names(x), "()"))))
  bullets <- paste0(
    cli::col_red(cli::symbol$cross), " ", funs,
    " masks ", other_calls,
    collapse = "\n"
  )
  prefer_note <-
    paste(
      cli::col_blue(cli::symbol$bullet),
      "Use",
      cli::col_green("tidymodels_prefer()"),
      "to resolve common conflicts."
    )

  paste0(header, "\n", bullets, "\n", prefer_note)
}

#' @export
print.tidymodels_conflicts <- function(x, ..., startup = FALSE) {
  cli::cat_line(tidymodels_conflict_message(x))
  cli::cat_line("Use ", cli::col_green("tidymodels_prefer()"), " to resolve these.")
}

#' @importFrom dplyr %>%
confirm_conflict <- function(packages, name) {
  # Only look at functions
  objs <- packages %>%
    purrr::map(~ get(name, pos = .)) %>%
    purrr::keep(is.function)

  if (length(objs) <= 1)
    return()

  # Remove identical functions
  objs <- objs[!duplicated(objs)]
  packages <- packages[!duplicated(packages)]
  if (length(objs) == 1)
    return()

  packages
}

ls_env <- function(env) {
  x <- ls(pos = env)
  if (identical(env, "package:dplyr")) {
    x <- setdiff(x, c("intersect", "setdiff", "setequal", "union"))
  }
  x
}
