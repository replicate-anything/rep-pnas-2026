test_that("type_contrasts matches the manuscript", {
  testthat::skip_if_not_installed("replicateEverything")
  fit <- run_step("type_contrasts", given = "parents")
  source(file.path(study_root(), "tests", "substantive", "type_contrasts.R"), local = TRUE)
  substantive_check_type_contrasts(fit)
  testthat::expect_true(file.exists(file.path(study_root(), "outputs", "type_contrasts.rds")))
})

