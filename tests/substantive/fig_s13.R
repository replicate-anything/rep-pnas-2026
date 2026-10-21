substantive_check_fig_s13 <- function(object, tolerance = 1e-6) {
  compare_keyed(
    flows_from_lodes(object$lodes),
    golden_csv("fig_s13"),
    keys = c("from", "to"),
    measures = c("Freq", "Share"),
    tolerance = tolerance
  )
}

