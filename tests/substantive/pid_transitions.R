
substantive_check_pid_transitions <- function(object) {
  saved <- readRDS(file.path(study_root(), "outputs", "pid_transitions.rds"))
  compare_keyed(
    flows_from_lodes(saved$full$lodes),
    golden_csv("fig_s11"),
    keys = c("from", "to"),
    measures = c("Freq", "Share"),
    tolerance = 1e-6
  )
  compare_keyed(
    flows_from_lodes(saved$treat$lodes),
    golden_csv("fig_s12"),
    keys = c("from", "to"),
    measures = c("Freq", "Share"),
    tolerance = 1e-6
  )
  compare_keyed(
    flows_from_lodes(saved$control$lodes),
    golden_csv("fig_s13"),
    keys = c("from", "to"),
    measures = c("Freq", "Share"),
    tolerance = 1e-6
  )
}

