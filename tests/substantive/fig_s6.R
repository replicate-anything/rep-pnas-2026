substantive_check_fig_s6 <- function(object, tolerance = 1e-5) {
  compare_keyed(
    object,
    golden_csv("fig_s6"),
    keys = c("label", "term"),
    measures = c("estimate", "std.error", "conf.low", "conf.high", "p.value"),
    tolerance = tolerance
  )
}

