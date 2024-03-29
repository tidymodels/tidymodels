% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/conflicts.R
\name{tidymodels_conflicts}
\alias{tidymodels_conflicts}
\title{Conflicts between the tidymodels and other packages}
\usage{
tidymodels_conflicts()
}
\description{
This function lists all the conflicts between packages in the tidymodels
and other packages that you have loaded.
}
\details{
There are four conflicts that are deliberately ignored: \code{intersect},
\code{union}, \code{setequal}, and \code{setdiff} from dplyr. These functions
make the base equivalents generic, so shouldn't negatively affect any
existing code.

To manage conflicts, you can use the conflicted package. To prefer tidymodels
functions over other functions, use \code{\link[=tidymodels_prefer]{tidymodels_prefer()}}.
}
\examples{
tidymodels_conflicts()
}
\seealso{
\code{\link[=tidymodels_prefer]{tidymodels_prefer()}}
}
