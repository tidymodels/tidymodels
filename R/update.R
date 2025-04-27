#' Update tidymodels packages
#'
#' This will check to see if all tidymodels packages (and optionally, their
#' dependencies) are up-to-date, and will install after an interactive
#' confirmation.
#'
#' @param pkg A character string for the model being updated.
#' @param recursive If `TRUE`, will also check all dependencies of
#'   tidymodels packages.
#' @param ... Extra arguments to pass to [utils::install.packages()]
#' @return Nothing is returned but a message is printed to the
#'  console about which packages (if any) should be installed along
#'  with code to do so.
#' @export
#' @examples
#' \dontrun{
#' tidymodels_update()
#' }
#' @import rlang
#' @importFrom utils install.packages
tidymodels_update <- function(pkg = "tidymodels", recursive = FALSE, ...) {
  deps <- pkg_deps(pkg, recursive)
  behind <- dplyr::filter(deps, behind)

  if (nrow(behind) == 0) {
    cli::cat_line("All packages up-to-date")
    return(invisible())
  }

  cli::cat_line("The following packages are out of date:")
  cli::cat_line()
  cli::cat_bullet(
    format(behind$package),
    " (",
    behind$local,
    " -> ",
    behind$cran,
    ")"
  )

  cli::cat_line()
  cli::cat_line("Start a clean R session then run:")

  install_opt <- quos(...)
  install_pkg <- behind$package
  inst_expr <- quo(install.packages(c(!!!install_pkg), !!!install_opt))
  pkg_str <- deparse(quo_squash(inst_expr))
  cli::cat_line(pkg_str)

  invisible()
}

#' List all dependencies
#'
#' @param x A character string for the packages being evaluated.
#' @param recursive If `TRUE`, will also list all dependencies of
#'   tidymodels packages.
#' @export
pkg_deps <- function(x = "tidymodels", recursive = FALSE) {
  pkgs <- utils::available.packages()
  deps <- tools::package_dependencies(x, pkgs, recursive = recursive)

  # NULL before package is on CRAN
  if ("tidymodels" %in% x && is.null(deps$tidymodels)) {
    deps$tidymodels <-
      c(
        "broom",
        "cli",
        "dials",
        "dplyr",
        "ggplot2",
        "infer",
        "modeldata",
        "parsnip",
        "pillar",
        "purrr",
        "recipes",
        "rlang",
        "rsample",
        "rstudioapi",
        "tibble",
        "tidyr",
        "tune",
        "workflows",
        "workflowsets",
        "yardstick"
      )
  }

  # include self in list
  pkg_deps <- unique(sort(c(names(deps), unlist(deps))))
  pkg_deps <- pkg_deps[pkg_deps %in% pkgs]

  base_pkgs <- c(
    "base",
    "compiler",
    "datasets",
    "graphics",
    "grDevices",
    "grid",
    "methods",
    "parallel",
    "splines",
    "stats",
    "stats4",
    "tools",
    "tcltk",
    "utils"
  )
  pkg_deps <- setdiff(pkg_deps, base_pkgs)

  cran_version <- lapply(pkgs[pkg_deps, "Version"], base::package_version)
  local_version <- lapply(pkg_deps, utils::packageVersion)

  behind <- purrr::map2_lgl(cran_version, local_version, `>`)

  tibble::tibble(
    package = pkg_deps,
    cran = cran_version |> purrr::map_chr(as.character),
    local = local_version |> purrr::map_chr(as.character),
    behind = behind
  )
}
