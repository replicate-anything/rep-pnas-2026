# replicateEverything provenance: connector
# Figure S11. Party-ID transitions, full sample.
# Execute with run_replication("here", "fig_s11").

source("./helpers_graphing.R")

make_fig_s11 <- function(data) {
  data$full
}

format_fig_s11 <- function(object) {
  plot <- plot_pid_alluvial(
    object$lodes,
    object$party_cols,
    "Party identification transitions: Wave 1 → Wave 2"
  )
  save_ggplot(plot, study_path("outputs", "fig_s11.png"), width = 9, height = 7)
  plot
}
