test_that("afd_3way matches the manuscript", {
  testthat::skip_if_not_installed("replicateEverything")
  fit <- run_step("afd_3way", given = "parents")
  source(file.path(study_root(), "tests", "substantive", "afd_3way.R"), local = TRUE)
  substantive_check_afd_3way(fit)
  testthat::expect_true(file.exists(file.path(study_root(), "outputs", "afd_3way.rds")))
})

