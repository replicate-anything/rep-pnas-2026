# replicateEverything provenance: connector
# Figure S4. Wave 1 effects by emotional strategy.
# Execute with run_replication("here", "fig_s4").

source("./helpers_labels.R")
source("./helpers_estimation.R")
source("./helpers_graphing.R")

make_fig_s4 <- function(data) {
  silence_fixest()
  outcomes <- engagement_outcomes()
  emotion_strats <- c("anger", "fear", "superiority")
  lapply(outcomes, function(Y) {
    lapply(emotion_strats, function(S) {
      run_feols_tidy(
        df = data |> filter(strategy == S),
        outcome_var = v_outcomes[[Y]],
        extra_cols = list(outcome = Y, strategy = S)
      )
    }) |> bind_rows()
  }) |>
    bind_rows() |>
    mutate(outcome = factor(outcome, levels = outcomes))
}

format_fig_s4 <- function(object) {
  outcomes <- engagement_outcomes()
  emotion_strats <- c("anger", "fear", "superiority")
  plot <- object |>
    mutate(
      strategy = factor(as.character(strategy), levels = rev(emotion_strats)),
      outcome = factor(outcome, levels = outcomes),
      super = outcome_super(outcome)
    ) |>
    ggplot(aes(x = estimate, y = strategy, color = strategy)) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.2) +
    geom_point(size = 1.8) +
    facet_nested(. ~ super + outcome, scales = "free_x") +
    facetted_pos_scales(x = shared_binary_x_scales(object, outcomes)) +
    scale_color_discrete(guide = "none") +
    labs(
      x = "Average treatment effect",
      y = NULL,
      title = "Wave 1 effects by emotional strategy",
      subtitle = "Video FE; SEs clustered by respondent"
    ) +
    theme_bw(base_size = 10) +
    theme(
      axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 9),
      axis.text.y = element_text(size = 9),
      strip.text = element_text(size = 10)
    )
  save_ggplot(plot, study_path("outputs", "fig_s4.png"), width = 12, height = 3.5)
  plot
}
