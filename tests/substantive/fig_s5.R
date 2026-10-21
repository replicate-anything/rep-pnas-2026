substantive_check_fig_s5 <- function(object, tolerance = 1e-5) {
  compare_keyed(
    object,
    golden_csv("fig_s5"),
    keys = c("horizon", "contrast", "outcome", "term"),
    measures = c("estimate", "std.error", "conf.low", "conf.high", "p.value"),
    tolerance = tolerance
  )
}

