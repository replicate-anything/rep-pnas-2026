# replicateEverything provenance: connector
# Figure S13. Party-ID transitions, control group.
# Execute with run_replication("here", "fig_s13").

source("./helpers_graphing.R")

make_fig_s13 <- function(data) {
  data$control
}

format_fig_s13 <- function(object) {
  plot <- plot_pid_alluvial(
    object$lodes,
    object$party_cols,
    "Party identification transitions (control)"
  )
  save_ggplot(plot, study_path("outputs", "fig_s13.png"), width = 9, height = 7)
  plot
}
