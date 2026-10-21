test_that("tab_s4_experiment matches the manuscript", {
  testthat::skip_if_not_installed("replicateEverything")
  fit <- run_step("tab_s4_experiment", given = "parents")
  source(file.path(study_root(), "tests", "substantive", "tab_s4_experiment.R"), local = TRUE)
  substantive_check_tab_s4_experiment(fit)
  shown <- run_step("tab_s4_experiment", given = "parents", format = TRUE)
  testthat::expect_true(grepl("<table", paste(as.character(shown), collapse = "\n"), ignore.case = TRUE))
  testthat::expect_true(file.exists(file.path(study_root(), "outputs", "tab_s4_experiment.html")))
})

