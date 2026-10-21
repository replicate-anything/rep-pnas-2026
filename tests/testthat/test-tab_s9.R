test_that("tab_s9 matches the manuscript", {
  testthat::skip_if_not_installed("replicateEverything")
  fit <- run_step("tab_s9", given = "parents")
  source(file.path(study_root(), "tests", "substantive", "tab_s9.R"), local = TRUE)
  substantive_check_tab_s9(fit)
  shown <- run_step("tab_s9", given = "parents", format = TRUE)
  testthat::expect_true(grepl("<table", paste(as.character(shown), collapse = "\n"), ignore.case = TRUE))
  testthat::expect_true(file.exists(file.path(study_root(), "outputs", "tab_s9.html")))
})

