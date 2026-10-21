substantive_check_fig_s7 <- function(object, tolerance = 1e-5) {
  compare_keyed(
    object,
    golden_csv("fig_s7"),
    keys = c("label", "term"),
    measures = c("estimate", "std.error", "conf.low", "conf.high", "p.value"),
    tolerance = tolerance
  )
}

