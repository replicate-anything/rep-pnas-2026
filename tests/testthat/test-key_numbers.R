test_that("key_numbers matches the manuscript", {
  testthat::skip_if_not_installed("replicateEverything")
  fit <- run_step("key_numbers", given = "parents")
  source(file.path(study_root(), "tests", "substantive", "key_numbers.R"), local = TRUE)
  substantive_check_key_numbers(fit)
  shown <- run_step("key_numbers", given = "parents", format = TRUE)
  testthat::expect_true(grepl("<table", paste(as.character(shown), collapse = "\n"), ignore.case = TRUE))
  testthat::expect_true(file.exists(file.path(study_root(), "outputs", "key_numbers.html")))
  testthat::expect_false(file.exists(file.path(study_root(), "outputs", "key_numbers.tex")))
})

