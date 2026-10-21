library(testthat)

testthat::test_dir(
  path = "testthat",
  reporter = c("progress", "fail")
)
