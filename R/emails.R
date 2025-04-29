make_email <- function(to, package) {
  name <- gsub(" <.*>", "", to)

  body <- glue::glue(
    "
  Dear {name},

  Your package, {package}, lists tidymodels in either Depends, Imports, or
  Suggests in the DESCRIPTION file. This is a bad idea because tidymodels is a
  set of packages designed for interactive modeling and it includes a very
  large number of direct and indirect dependencies, most of which your package
  probably doesn't use.

  Instead of depending on all tidymodels packages, please import from, suggest,
  or depend on the tidymodels packages in that you actually use. This will make
  your package faster to install and will head off potential problems down
  the road.

  Thanks,
  Max
  "
  )

  get("gm_mime", asNamespace("gmailr"))(
    from = "max@posit.co",
    to = to,
    subject = glue::glue("{package} and tidymodels"),
    body = body
  )
}

tidymodels_dependency_dissuade <- function() {
  pkgs <- tools::package_dependencies(
    "tidymodels",
    which = c("Depends", "Imports", "Suggests"),
    reverse = TRUE
  )[[1]]

  db <- tools::CRAN_package_db()
  maintainers <- db$Maintainer[match(pkgs, db$Package)]

  emails <- purrr::map2(maintainers, pkgs, make_email)
  purrr::walk(emails, ~ try(get("gm_send_message", asNamespace("gmailr"))(.x)))
}

# gm_auth_configure(path = "path/to/oauth.json")
# tidyverse_dependency_dissuade()
