
substantive_check_tab_s12 <- function(object) {
  expected <- golden_csv("tab_s12")
  flat <- flatten_models(object)
  gl_r2 <- vapply(object, function(m) {
    as.numeric(as.data.frame(m$glance)$r.squared[[1]])
  }, numeric(1))
  flat$r_squared <- round(unname(gl_r2[flat$model]), 3)
  flat$estimate <- round(flat$estimate, 3)
  flat$std.error <- round(flat$std.error, 3)
  compare_keyed(
    flat,
    expected,
    keys = c("model", "term"),
    measures = c("estimate", "std.error", "r_squared", "nobs"),
    tolerance = 1e-8
  )
}

