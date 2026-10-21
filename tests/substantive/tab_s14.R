
substantive_check_tab_s14 <- function(object) {
  expected <- golden_csv("tab_s14")
  got <- as.data.frame(object, stringsAsFactors = FALSE)
  got <- got[!got$blank, c("row", "col", "r_t", "r_c", "delta", "p"), drop = FALSE]
  got$r_t <- round(got$r_t, 2)
  got$r_c <- round(got$r_c, 2)
  got$delta <- round(got$delta, 2)
  got$stars <- star_count(got$p)
  compare_keyed(
    got,
    expected,
    keys = c("row", "col"),
    measures = c("r_t", "r_c", "delta", "stars"),
    tolerance = 1e-8
  )
}

