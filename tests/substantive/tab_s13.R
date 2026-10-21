
substantive_check_tab_s13 <- function(object) {
  expected <- golden_csv("tab_s13")
  flat <- flatten_models(object$models)
  flat <- flat[flat$term == "workshop_Treatment", , drop = FALSE]
  flat$estimate <- round(flat$estimate, 3)
  flat$std.error <- round(flat$std.error, 3)
  bh <- stats::setNames(as.numeric(object$bh), object$bh_names)
  flat$bh <- NA_real_
  hit <- match(flat$model, names(bh))
  flat$bh[!is.na(hit)] <- round(bh[hit[!is.na(hit)]], 3)
  compare_keyed(
    flat,
    expected,
    keys = c("model", "term"),
    measures = c("estimate", "std.error", "bh", "nobs"),
    tolerance = 1e-8
  )
}

