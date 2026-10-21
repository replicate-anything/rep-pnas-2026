substantive_check_fig_s4 <- function(object, tolerance = 1e-5) {
  compare_keyed(
    object,
    golden_csv("fig_s4"),
    keys = c("outcome", "strategy", "term"),
    measures = c("estimate", "std.error", "conf.low", "conf.high", "p.value"),
    tolerance = tolerance
  )
}

