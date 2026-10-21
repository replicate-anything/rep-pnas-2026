substantive_check_tab_s6 <- function(object, tolerance = 1e-5) {
  compare_keyed(
    flatten_models(object),
    golden_csv("tab_s6"),
    keys = c("model", "term"),
    measures = c("estimate", "std.error", "conf.low", "conf.high", "p.value", "nobs"),
    tolerance = tolerance
  )
}

