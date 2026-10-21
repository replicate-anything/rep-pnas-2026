substantive_check_fig_s3 <- function(object, tolerance = 1e-5) {
  compare_keyed(
    object,
    golden_csv("fig_s3"),
    keys = c("horizon", "outcome", "strategy", "treatment"),
    measures = c("estimate", "std.error", "conf.low", "conf.high", "n"),
    tolerance = tolerance
  )
}

