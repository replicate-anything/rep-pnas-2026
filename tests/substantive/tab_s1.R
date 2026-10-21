
substantive_check_tab_s1 <- function(object) {
  expected <- golden_csv("tab_s1")
  got <- as.data.frame(object, stringsAsFactors = FALSE)
  got$videos_per_respondent <- round(got$videos_per_respondent, 2)
  compare_keyed(
    got,
    expected,
    keys = "horizon",
    measures = setdiff(names(expected), "horizon"),
    tolerance = 1e-8
  )
}

