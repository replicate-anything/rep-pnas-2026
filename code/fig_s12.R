# replicateEverything provenance: connector
# Figure S12. Party-ID transitions, treatment group.
# Execute with run_replication("here", "fig_s12").

source("./helpers_graphing.R")

make_fig_s12 <- function(data) {
  data$treat
}

format_fig_s12 <- function(object) {
  plot <- plot_pid_alluvial(
    object$lodes,
    object$party_cols,
    "Party identification transitions (treatment)"
  )
  save_ggplot(plot, study_path("outputs", "fig_s12.png"), width = 9, height = 7)
  plot
}
