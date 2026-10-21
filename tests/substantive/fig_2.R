substantive_check_fig_2 <- function(object, tolerance = 1e-5) {
  compare_keyed(
    object,
    golden_csv("fig_2"),
    keys = c("sample", "party", "outcome", "term"),
    measures = c("estimate", "std.error", "conf.low", "conf.high", "p.value"),
    tolerance = tolerance
  )
}

