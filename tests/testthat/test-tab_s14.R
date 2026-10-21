test_that("tab_s14 matches the manuscript", {
  testthat::skip_if_not_installed("replicateEverything")
  fit <- run_step("tab_s14", given = "parents")
  source(file.path(study_root(), "tests", "substantive", "tab_s14.R"), local = TRUE)
  substantive_check_tab_s14(fit)
  shown <- run_step("tab_s14", given = "parents", format = TRUE)
  testthat::expect_true(grepl("<table", paste(as.character(shown), collapse = "\n"), ignore.case = TRUE))
  testthat::expect_true(file.exists(file.path(study_root(), "outputs", "tab_s14.html")))
})

