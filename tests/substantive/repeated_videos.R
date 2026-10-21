
substantive_check_repeated_videos <- function(object) {
  expected <- golden_csv("repeated_videos")
  testthat::expect_equal(as.numeric(round(100 * object$median_share)), expected$median_pp)
  testthat::expect_equal(as.numeric(round(100 * object$p90_share)), expected$p90_pp)
  testthat::expect_gt(object$n_respondents, 0)
  testthat::expect_gt(object$n_norepeat, 0)
}

