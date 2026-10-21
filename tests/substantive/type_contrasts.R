
substantive_check_type_contrasts <- function(object) {
  saved <- readRDS(file.path(study_root(), "outputs", "type_contrasts.rds"))
  blocks <- c(
    emo_neu_like = "tab_s5",
    emo_neu_report = "tab_s6",
    emo_ent_like = "tab_s7",
    emo_ent_report = "tab_s8"
  )
  for (nm in names(blocks)) {
    compare_keyed(
      flatten_models(saved[[nm]]),
      golden_csv(blocks[[nm]]),
      keys = c("model", "term"),
      measures = c("estimate", "std.error", "conf.low", "conf.high", "p.value", "nobs"),
      tolerance = 1e-5
    )
  }
}

