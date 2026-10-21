# replicateEverything provenance: connector
# Figure S9. Wave 2 effects after dropping repeated video exposure.
# Execute with run_replication("here", "fig_s9").

source("./helpers_labels.R")
source("./helpers_estimation.R")
source("./helpers_graphing.R")

make_fig_s9 <- function(data) {
  silence_fixest()
  outcomes <- engagement_outcomes()
  estimate_pooled_ates(data$norepeat, outcomes) |>
    mutate(horizon = "Wave 2")
}

format_fig_s9 <- function(object) {
  outcomes <- engagement_outcomes()
  plot <- plot_strategy_ates(
    object,
    outcomes,
    "Wave 2 effects excluding repeated video exposure",
    "Video FE; SEs clustered by respondent",
    facet_horizon = FALSE
  )
  save_ggplot(plot, study_path("outputs", "fig_s9.png"), width = 12, height = 3.5)
  plot
}
