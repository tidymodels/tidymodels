tags <- list(
  "text analysis" = c("tidytext"),
  "feature engineering" = c("embed"),
  "time series" = c("tibbletime")
)


#' Facilities for loading and updating other packages
#' 
#' `tidymodels` contains a set of tags for specific topics in 
#'  modeling and analysis. These can be used to load packages
#'  in groups. 
#'  
#' @param tag A character strong for the tag. Current options
#'  are "text analysis", "feature engineering", and "time series".
#'  
#' @examples 
#' show_tags()
#' @aliases tags
#' @export
show_tags <- function() {
  all_tags <- vapply(
    tags, 
    function(x) paste0("'", x, "'", collapse = ", "),
    character(1))
  all_tags <- paste0(names(all_tags), ": ", all_tags, "\n")
  cat("All tags:\n\n")
  cat(all_tags, sep = "")
  invisible(tags)
}

#' @export
#' @rdname show_tags
load_tag <- function(tag) {
  
}

#' @export
#' @rdname show_tags
update_tag <- function(tag) {
  
}

