test_that("fig_s11 matches the manuscript", {
  testthat::skip_if_not_installed("replicateEverything")
  fit <- run_step("fig_s11", given = "nothing")
  source(file.path(study_root(), "tests", "substantive", "fig_s11.R"), local = TRUE)
  substantive_check_fig_s11(fit)
  shown <- run_step("fig_s11", given = "nothing", format = TRUE)
  testthat::expect_s3_class(shown, "ggplot")
  testthat::expect_true(file.exists(file.path(study_root(), "outputs", "fig_s11.png")))
})

