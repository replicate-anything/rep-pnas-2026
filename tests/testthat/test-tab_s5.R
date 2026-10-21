test_that("tab_s5 matches the manuscript", {
  testthat::skip_if_not_installed("replicateEverything")
  fit <- run_step("tab_s5", given = "nothing")
  source(file.path(study_root(), "tests", "substantive", "tab_s5.R"), local = TRUE)
  substantive_check_tab_s5(fit)
  shown <- run_step("tab_s5", given = "nothing", format = TRUE)
  testthat::expect_true(grepl("<table", paste(as.character(shown), collapse = "\n"), ignore.case = TRUE))
  testthat::expect_true(file.exists(file.path(study_root(), "outputs", "tab_s5.html")))
})

