substantive_check_fig_s2 <- function(object, tolerance = 1e-6) {
  compare_keyed(
    object,
    golden_csv("fig_s2"),
    keys = "scale_label",
    measures = c("n", "pct"),
    tolerance = tolerance
  )
}

