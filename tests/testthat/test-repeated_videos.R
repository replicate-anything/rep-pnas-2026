test_that("repeated_videos matches the manuscript", {
  testthat::skip_if_not_installed("replicateEverything")
  fit <- run_step("repeated_videos", given = "parents")
  source(file.path(study_root(), "tests", "substantive", "repeated_videos.R"), local = TRUE)
  substantive_check_repeated_videos(fit)
  testthat::expect_true(file.exists(file.path(study_root(), "outputs", "repeat_summary.rds")))
})

