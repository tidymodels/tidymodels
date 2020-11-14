msg <- function(..., startup = FALSE) {
  if (startup) {
    if (!isTRUE(getOption("tidymodels.quiet"))) {
      packageStartupMessage(text_col(...))
    }
  } else {
    message(text_col(...))
  }
}

text_col <- function(x) {
  # If RStudio not available, messages already printed in black
  if (!rstudioapi::isAvailable()) {
    return(x)
  }

  if (!rstudioapi::hasFun("getThemeInfo")) {
    return(x)
  }

  theme <- rstudioapi::getThemeInfo()

  if (isTRUE(theme$dark)) crayon::white(x) else crayon::black(x)

}

#' List all packages in the tidymodels
#'
#' @param include_self Include tidymodels in the list?
#' @export
#' @examples
#' tidymodels_packages()
tidymodels_packages <- function(include_self = TRUE) {
  raw <- utils::packageDescription("tidymodels")$Imports
  imports <- strsplit(raw, ",")[[1]]
  parsed <- gsub("^\\s+|\\s+$", "", imports)
  names <- vapply(strsplit(parsed, "\\s+"), "[[", 1, FUN.VALUE = character(1))

  if (include_self) {
    names <- c(names, "tidymodels")
  }

  names
}

invert <- function(x) {
  if (length(x) == 0) return()
  stacked <- utils::stack(x)
  tapply(as.character(stacked$ind), stacked$values, list)
}


style_grey <- function(level, ...) {
  crayon::style(
    paste0(...),
    crayon::make_style(grDevices::grey(level), grey = TRUE)
  )
}

release_bullets <- function() {
  c(
    'Check what `usethis::use_latest_dependencies(TRUE, "CRAN")` might update',
    'Use `tidymodels_dependency_dissuade()` to send emails'
  )
}
