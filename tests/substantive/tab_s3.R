
substantive_check_tab_s3 <- function(object) {
  expected <- golden_csv("tab_s3")
  got <- as.data.frame(object, stringsAsFactors = FALSE)
  got$row <- "kappa"
  expected$row <- "kappa"
  compare_keyed(
    got,
    expected,
    keys = "row",
    measures = c("anger", "fear", "superiority", "any_emotion"),
    tolerance = 1e-8
  )
}

