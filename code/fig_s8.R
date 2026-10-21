# replicateEverything provenance: connector
# Figure S8. Figure 1 Wave 1 estimates among Wave 2 non-attriters.
# Execute with run_replication("here", "fig_s8").

source("./helpers_labels.R")
source("./helpers_estimation.R")
source("./helpers_graphing.R")

make_fig_s8 <- function(data) {
  silence_fixest()
  outcomes <- engagement_outcomes()
  nonattrit_ids <- data$df_w9_w10 |>
    filter(attrit == 0) |>
    pull(id_w9) |>
    as.character()
  df_w9 <- data$wave_9 |>
    filter(as.character(ID) %in% nonattrit_ids)
  estimate_pooled_ates(df_w9, outcomes) |>
    mutate(horizon = "Wave 1")
}

format_fig_s8 <- function(object) {
  outcomes <- engagement_outcomes()
  plot <- plot_strategy_ates(
    object,
    outcomes,
    "Pooled Wave 1 treatment effects (non-attriters)",
    "Video FE; SEs clustered by respondent; retained respondents only"
  )
  save_ggplot(plot, study_path("outputs", "fig_s8.png"), width = 12, height = 3.5)
  plot
}
