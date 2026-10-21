test_that("fig_s5 matches the manuscript", {
  testthat::skip_if_not_installed("replicateEverything")
  fit <- run_step("fig_s5", given = "parents")
  source(file.path(study_root(), "tests", "substantive", "fig_s5.R"), local = TRUE)
  substantive_check_fig_s5(fit)
  shown <- run_step("fig_s5", given = "parents", format = TRUE)
  testthat::expect_s3_class(shown, "ggplot")
  testthat::expect_true(file.exists(file.path(study_root(), "outputs", "fig_s5.png")))
})

