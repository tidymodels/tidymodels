tags <- list(
  "Bayesian analysis" =  c("tidyposterior", "tidybayes"),
  "deep learning" = c("keras"),
  "feature engineering" = c("keras"),
  "parallel processing" = c("furrr"),
  "resampling" = c("infer", "tidyposterior"),
  "text analysis" = c("tidytext", "keras"),
  "time series" = c("timetk", "tidyquant", "sweep")
)


#' Facilities for loading and updating other packages
#'
#' `tidymodels` contains a set of tags for specific topics in
#'  modeling and analysis. These can be used to load packages
#'  in groups.
#'
#' @param tag A character strong for the tag. Current options
#'  are 'Bayesian analysis', 'deep learning', 'feature engineering',
#'  'parallel processing', 'resampling', 'text analysis',  and
#'  'time series'.
#'
#' @examples
#' tag_show()
#' @aliases tags
#' @export
tag_show <- function() {
  all_tags <- vapply(
    tags,
    function(x) quote_pkg(x),
    character(1))
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
    stop("Some pacakges are not installed: ",
         quote_pkg(pkgs[!is_installed]),
         call. = FALSE)
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

quote_pkg <- function(x)
  paste0("'", x, "'", collapse = ", ")

tag_validate <- function(tag) {
  if (!is.character(tag) || length(tag) != 1)
    stop("`tag` should be one of: ",
         paste0("'", names(tags), "'", collapse = ", "),
         call. = FALSE)
}
