
substantive_check_afd_3way <- function(object) {
  saved <- readRDS(file.path(study_root(), "outputs", "afd_3way.rds"))
  compare_keyed(
    flatten_models(saved$short),
    golden_csv("tab_s10"),
    keys = c("model", "term"),
    measures = c("estimate", "std.error", "conf.low", "conf.high", "p.value", "nobs"),
    tolerance = 1e-5
  )
  compare_keyed(
    flatten_models(saved$long),
    golden_csv("tab_s11"),
    keys = c("model", "term"),
    measures = c("estimate", "std.error", "conf.low", "conf.high", "p.value", "nobs"),
    tolerance = 1e-5
  )
}

