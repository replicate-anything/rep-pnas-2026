test_that("pid_transitions matches the manuscript", {
  testthat::skip_if_not_installed("replicateEverything")
  fit <- run_step("pid_transitions", given = "parents")
  source(file.path(study_root(), "tests", "substantive", "pid_transitions.R"), local = TRUE)
  substantive_check_pid_transitions(fit)
  testthat::expect_true(file.exists(file.path(study_root(), "outputs", "pid_transitions.rds")))
})

