
test_that("list_replications sees the declared steps", {
  testthat::skip_if_not_installed("replicateEverything")
  old <- getwd()
  setwd(study_root())
  on.exit(setwd(old), add = TRUE)
  listed <- replicateEverything::list_replications("here", include = "all")
  ids <- vapply(listed, function(e) as.character(e$id), character(1))
  expected <- c(
    "repeated_videos",
    "type_contrasts",
    "afd_3way",
    "pid_transitions",
    "fig_1",
    "fig_2",
    "fig_s2",
    "fig_s3",
    "fig_s4",
    "fig_s5",
    "fig_s6",
    "fig_s7",
    "fig_s8",
    "fig_s9",
    "fig_s10",
    "fig_s11",
    "fig_s12",
    "fig_s13",
    "fig_s14",
    "fig_s15",
    "tab_s1",
    "tab_s3",
    "tab_s4_experiment",
    "tab_s5",
    "tab_s6",
    "tab_s7",
    "tab_s8",
    "tab_s9",
    "tab_s10",
    "tab_s11",
    "tab_s12",
    "tab_s13",
    "tab_s14",
    "key_numbers"
  )
  missing <- setdiff(expected, ids)
  if (length(missing)) {
    testthat::fail(paste("Missing steps:", paste(missing, collapse = ", ")))
  }
  testthat::expect_true(length(expected) == 34L)
})

