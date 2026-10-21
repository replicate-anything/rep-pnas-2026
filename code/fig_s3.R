# replicateEverything provenance: connector
# Figure S3. Mean outcome levels by treatment, with cluster-robust CIs.
# Execute with run_replication("here", "fig_s3").

library(estimatr, quietly = TRUE, warn.conflicts = FALSE)
library(broom, quietly = TRUE, warn.conflicts = FALSE)
source("./helpers_labels.R")
source("./helpers_graphing.R")

make_fig_s3 <- function(data) {
  outcome_order_levels <- engagement_outcomes()
  df_levels_row <- bind_rows(
    data$wave_9 |> mutate(horizon = "Wave 1"),
    data$wave_10 |> mutate(horizon = "Wave 2")
  ) |>
    transmute(
      ID = as.character(ID),
      horizon = factor(horizon, levels = horizon_wave_levels),
      strategy = factor(as.character(strategy_2), levels = rev(strategy_order_article)),
      treatment = factor(
        ifelse(workshop_Treatment == 1, "Treatment", "Control"),
        levels = c("Control", "Treatment")
      ),
      Like = as.numeric(videoLikep1r),
      Report = as.numeric(videoCommentTickedp1r),
      Share = as.numeric(videoSharep1r),
      Bookmark = as.numeric(videoBookmarkp1r),
      Skip = as.numeric(skipped_video_),
      `Watch Time` = as.numeric(watch_percentage_)
    ) |>
    pivot_longer(
      cols = all_of(outcome_order_levels),
      names_to = "outcome",
      values_to = "y"
    ) |>
    filter(is.finite(y), !is.na(strategy), !is.na(treatment))

  df_levels_row |>
    mutate(outcome = factor(outcome, levels = outcome_order_levels)) |>
    group_by(horizon, outcome, strategy, treatment) |>
    group_modify(~ {
      m <- estimatr::lm_robust(y ~ 1, data = .x, clusters = ID)
      broom::tidy(m, conf.int = TRUE) |>
        dplyr::filter(term == "(Intercept)") |>
        dplyr::transmute(
          estimate,
          std.error,
          conf.low,
          conf.high,
          n = nrow(.x),
          n_id = dplyr::n_distinct(.x$ID)
        )
    }) |>
    ungroup()
}

format_fig_s3 <- function(object) {
  treat_cols <- c(Control = "#4C78A8", Treatment = "#E45756")
  pd <- position_dodge(width = 0.5)
  plot <- object |>
    mutate(super = outcome_super(outcome)) |>
    ggplot(aes(x = estimate, y = strategy, color = treatment)) +
    geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.2, position = pd) +
    geom_point(size = 1.8, position = pd) +
    facet_nested(horizon ~ super + outcome, scales = "free_x") +
    scale_color_manual(values = treat_cols, name = NULL) +
    labs(
      x = "Mean outcome level",
      y = NULL,
      title = "Outcome levels by content type and treatment",
      subtitle = "Video–respondent rows; cluster-robust 95% CIs by respondent"
    ) +
    theme_bw(base_size = 10) +
    theme(
      axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 9),
      axis.text.y = element_text(size = 9),
      strip.text = element_text(size = 10),
      panel.spacing.y = unit(6, "pt"),
      legend.position = "bottom"
    )
  save_ggplot(plot, study_path("outputs", "fig_s3.png"), width = 12, height = 5.5)
  plot
}
