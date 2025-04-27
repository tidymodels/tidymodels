tags <- list(
  "Bayesian analysis" = c("tidyposterior", "tidybayes"),
  "deep learning" = c("keras"),
  "ensembles" = c("modeltime.ensemble", "stacks"),
  "feature engineering" = c("keras", "textrecipes"),
  "parallel processing" = c("furrr"),
  "parameter tuning" = c("finetune"),
  "resampling" = c("infer", "tidyposterior"),
  "text analysis" = c("tidytext", "keras", "textrecipes"),
  "time series" = c(
    "timetk",
    "tidyquant",
    "sweep",
    "modeltime",
    "tsibble",
    "fable"
  ),
  "extra recipes" = c("embed", "textrecipes", "themis"),
  "extra models" = c("discrim", "plsmod", "rules", "poissonreg", "baguette"),
  "visualization" = c(
    "dotwhisker",
    "ggforce",
    "patchwork",
    "gganimate",
    "ggrepel"
  ),
  "interpretation" = c("vip", "lime", "DALEXtra")
)
# cat(paste0("'", sort(names(tags)), "'", collapse = ", "))

#' Facilities for loading and updating other packages
#'
#' The tidymodels metapackage contains a set of tags for specific topics in
#'  modeling and analysis. These can be used to load packages
#'  in groups.
#'
#' @param tag A character strong for the tag. Current options are: 'Bayesian
#'  analysis', 'deep learning', 'ensembles', 'extra models', 'extra recipes',
#'  'feature engineering', 'interpretation', 'parallel processing', 'parameter
#'  tuning', 'resampling', 'text analysis', 'time series', and 'visualization'.
#'
#' @examples
#' tag_show()
#' @aliases tags
#' @export
tag_show <- function() {
  all_tags <- vapply(
    tags,
    function(x) quote_pkg(x),
    character(1)
  )
  all_tags <- paste0(names(all_tags), ": ", all_tags, "\n")
  cat("All tags:\n\n")
  cat(all_tags, sep = "")
  invisible(tags)
}

#' @importFrom utils installed.packages
#' @export
#' @rdname tag_show
tag_attach <- function(tag) {
  tag_validate(tag)
  pkgs <- unlist(tags[[tag]])
  installed <- rownames(installed.packages())
  is_installed <- pkgs %in% installed
  if (any(!is_installed)) {
    rlang::abort(
      "Some packages are not installed: ",
      quote_pkg(pkgs[!is_installed])
    )
  }
  tidymodels_attach(unique(pkgs))
}

#' @export
#' @rdname tag_show
tag_update <- function(tag) {
  tag_validate(tag)
  pkgs <- tags[[tag]]
  tidymodels_update(unique(unlist(pkgs)))
}

quote_pkg <- function(x) {
  paste0("'", x, "'", collapse = ", ")
}

tag_validate <- function(tag) {
  if (!is.character(tag) || length(tag) != 1) {
    rlang::abort(
      "`tag` should be one of: ",
      paste0("'", names(tags), "'", collapse = ", ")
    )
  }
}
