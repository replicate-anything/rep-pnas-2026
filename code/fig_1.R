# replicateEverything provenance: connector
# Figure 1. Video FE, clustered by respondent.
# Execute with run_replication("here", "fig_1").

library(tidyverse, quietly = TRUE, warn.conflicts = FALSE)
library(fixest, quietly = TRUE, warn.conflicts = FALSE)
library(broom, quietly = TRUE, warn.conflicts = FALSE)
source("./helpers_labels.R")
source("./helpers_graphing.R")

# Outcome ~ workshop_Treatment | video, clustered by respondent.
# Looped over engagement outcomes and content strategies.
run_feols_tidy <- function(df, outcome_var, extra_cols = list()) {
  fixest::setFixest_notes(FALSE)
  feols(
    as.formula(paste(outcome_var, "~ workshop_Treatment | video")),
    data = df,
    cluster = ~ID
  ) |>
    tidy(conf.int = TRUE) |>
    bind_cols(as_tibble(extra_cols))
}

estimate_pooled_ates <- function(df, outcomes) {
  lapply(outcomes, function(Y) {
    lapply(unique(as.character(df$strategy_2)), function(S) {
      run_feols_tidy(
        df = df |> filter(strategy_2 == S),
        outcome_var = v_outcomes[[Y]],
        extra_cols = list(outcome = Y, strategy = S)
      )
    }) |> bind_rows()
  }) |> bind_rows()
}

make_fig_1 <- function(data) {
  outcomes <- engagement_outcomes()
  estimates <- bind_rows(
    estimate_pooled_ates(data$wave_9, outcomes) |> mutate(horizon = "Wave 1"),
    estimate_pooled_ates(data$wave_10, outcomes) |> mutate(horizon = "Wave 2")
  )
  save_rds_atomic(estimates, study_path("outputs", "fig_1_estimates.rds"))
  format_fig_1(estimates)
  estimates
}

format_fig_1 <- function(object) {
  plot <- plot_strategy_ates(
    object,
    engagement_outcomes(),
    "Treatment effects by content type",
    "Video FE; SEs clustered by respondent"
  )
  save_ggplot(plot, study_path("outputs", "fig_1.png"), width = 12, height = 5)
  plot
}
