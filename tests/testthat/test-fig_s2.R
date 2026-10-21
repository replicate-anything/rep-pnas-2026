test_that("fig_s2 matches the manuscript", {
  testthat::skip_if_not_installed("replicateEverything")
  fit <- run_step("fig_s2", given = "parents")
  source(file.path(study_root(), "tests", "substantive", "fig_s2.R"), local = TRUE)
  substantive_check_fig_s2(fit)
  shown <- run_step("fig_s2", given = "parents", format = TRUE)
  testthat::expect_s3_class(shown, "ggplot")
  testthat::expect_true(file.exists(file.path(study_root(), "outputs", "fig_s2.png")))
})

