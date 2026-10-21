substantive_check_fig_s14 <- function(object, tolerance = 1e-5) {
  compare_keyed(
    object,
    golden_csv("fig_s14"),
    keys = c("horizon", "party", "group", "term"),
    measures = c("estimate", "std.error", "conf.low", "conf.high", "p.value"),
    tolerance = tolerance
  )
}

