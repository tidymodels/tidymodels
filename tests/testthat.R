library(testthat)
library(tidymodels)

if (requireNamespace("xml2")) {
  test_check("tidymodels", reporter = MultiReporter$new(reporters = list(JunitReporter$new(file = "test-results.xml"), CheckReporter$new())))
} else {
  test_check("tidymodels")
}
