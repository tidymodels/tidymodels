core <- c(
  "broom",
  "dials",
  "dplyr",
  "ggplot2",
  "infer",
  "modeldata",
  "parsnip",
  "purrr",
  "recipes",
  "rsample",
  "tibble",
  "tidyr",
  "tune",
  "workflows",
  "workflowsets",
  "yardstick"
)


pkg_loaded <- function(pkg = NULL) {
  if (is.null(pkg)) {
    pkg <- core
  }
  search <- paste0("package:", pkg)
  pkg[search %in% search()]
}
pkg_unloaded <- function(pkg = NULL) {
  if (is.null(pkg)) {
    pkg <- core
  }
  search <- paste0("package:", pkg)
  pkg[!search %in% search()]
}

tidymodels_attach <- function(pkg = NULL) {
  to_load <- pkg_unloaded(pkg = pkg)
  if (length(to_load) == 0) {
    return(invisible())
  }

  msg(
    cli::rule(
      left = cli::style_bold("Attaching packages"),
      right = paste0("tidymodels ", package_version("tidymodels"))
    ),
    startup = TRUE
  )

  versions <- vapply(to_load, package_version, character(1))
  clean_versions <- gsub(cli::ansi_regex(), "", versions, perl = TRUE)
  packages <- paste0(
    cli::col_green(cli::symbol$tick),
    " ",
    cli::col_blue(format(to_load)),
    " ",
    cli::ansi_align(versions, max(nchar(clean_versions)))
  )

  if (length(packages) %% 2 == 1) {
    packages <- append(packages, "")
  }
  col1 <- seq_len(length(packages) / 2)
  info <- paste0(packages[col1], "     ", packages[-col1])

  msg(paste(info, collapse = "\n"), startup = TRUE)

  suppressPackageStartupMessages(
    lapply(to_load, library, character.only = TRUE, warn.conflicts = FALSE)
  )

  invisible()
}

package_version <- function(x) {
  version <- as.character(unclass(utils::packageVersion(x))[[1]])

  if (length(version) > 3) {
    version[4:length(version)] <- cli::col_red(as.character(version[
      4:length(version)
    ]))
  }
  paste0(version, collapse = ".")
}
