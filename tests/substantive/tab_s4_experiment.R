
substantive_check_tab_s4_experiment <- function(object) {
  compare_keyed(
    object,
    golden_csv("tab_s4_experiment"),
    keys = "row_lab",
    measures = c("emotional", "neutral", "entertainment", "all"),
    tolerance = 1e-8
  )
}

