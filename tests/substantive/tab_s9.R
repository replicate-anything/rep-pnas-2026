
substantive_check_tab_s9 <- function(object) {
  expected <- golden_csv("tab_s9")
  got <- as.data.frame(object$rows, stringsAsFactors = FALSE)
  got$rate <- round(got$rate, 2)
  compare_keyed(
    got,
    expected,
    keys = "arm",
    measures = c("original_n", "retained", "attrited", "rate"),
    tolerance = 1e-8
  )
  contrast <- golden_csv("tab_s9_contrast")
  td <- as.data.frame(object$test, stringsAsFactors = FALSE)
  testthat::expect_equal(round(100 * td$estimate[[1]], 2), contrast$estimate_pp[[1]])
  testthat::expect_equal(round(100 * td$conf.low[[1]], 2), contrast$conf_low_pp[[1]])
  testthat::expect_equal(round(100 * td$conf.high[[1]], 2), contrast$conf_high_pp[[1]])
  testthat::expect_equal(round(td$p.value[[1]], 3), contrast$p_value[[1]])
}

