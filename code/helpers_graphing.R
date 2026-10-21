# replicateEverything provenance: connector
# Plot pieces shared by more than one figure.

source("./helpers_labels.R")

library(tidyverse, quietly = TRUE, warn.conflicts = FALSE)
library(ggh4x, quietly = TRUE, warn.conflicts = FALSE)
library(ggrepel, quietly = TRUE, warn.conflicts = FALSE)
library(ggalluvial, quietly = TRUE, warn.conflicts = FALSE)

strategy_color_values <- c(
  "political\n(emotional)" = "black",
  "political\n(neutral)" = "grey55",
  "entertainment" = "grey55"
)

outcome_super <- function(outcome) {
  factor(
    if_else(as.character(outcome) %in% l_outcomegroups$main, "Engagement", "Attention"),
    levels = c("Engagement", "Attention")
  )
}

shared_binary_x_scales <- function(df, outcomes) {
  watch <- "Watch Time"
  shared <- setdiff(as.character(outcomes), watch)
  rng <- function(ok) {
    dat <- df |> filter(as.character(outcome) %in% ok)
    vals <- dat$estimate
    if ("conf.low" %in% names(dat)) {
      vals <- c(vals, dat$conf.low, dat$conf.high)
    }
    range(vals, na.rm = TRUE)
  }
  shared_lim <- rng(shared)
  watch_lim <- rng(watch)
  lapply(as.character(outcomes), function(y) {
    scale_x_continuous(limits = if (identical(y, watch)) watch_lim else shared_lim)
  })
}

plot_strategy_ates <- function(estimates, outcomes, title, subtitle, facet_horizon = TRUE) {
  df <- estimates |>
    mutate(
      strategy = factor(as.character(strategy), levels = rev(strategy_order_article)),
      outcome = factor(outcome, levels = outcomes),
      super = outcome_super(outcome),
      strat_color = as.character(strategy)
    )
  if (isTRUE(facet_horizon) && "horizon" %in% names(df)) {
    df$horizon <- factor(df$horizon, levels = horizon_wave_levels)
    facet <- facet_nested(horizon ~ super + outcome, scales = "free_x")
  } else {
    facet <- facet_nested(. ~ super + outcome, scales = "free_x")
  }
  df |>
    ggplot(aes(x = estimate, y = strategy, color = strat_color)) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.2) +
    geom_point(size = 1.8) +
    facet +
    facetted_pos_scales(x = shared_binary_x_scales(estimates, outcomes)) +
    scale_color_manual(values = strategy_color_values, guide = "none") +
    labs(x = "Average treatment effect", y = NULL, title = title, subtitle = subtitle) +
    theme_bw(base_size = 10) +
    theme(
      axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 9),
      axis.text.y = element_text(size = 9),
      strip.text = element_text(size = 10),
      panel.spacing.y = unit(6, "pt")
    )
}

plot_pid_alluvial <- function(lodes, party_cols, title_text) {
  label_layer <- function(side, nudge, hjust) {
    ggrepel::geom_text_repel(
      data = lodes |> filter(x == side),
      stat = "stratum",
      aes(label = stratum_lab),
      size = 3,
      lineheight = 0.9,
      direction = "y",
      nudge_x = nudge,
      hjust = hjust,
      segment.size = 0.25,
      segment.color = "grey40",
      box.padding = 0.2,
      point.padding = 0.05,
      min.segment.length = 0,
      max.overlaps = Inf,
      seed = 1,
      show.legend = FALSE
    )
  }
  ggplot(
    lodes,
    aes(x = x, stratum = stratum, alluvium = alluvium, y = Share, fill = stratum)
  ) +
    geom_alluvium(alpha = 0.65, color = NA, knot.pos = 0.35, width = 0.22) +
    geom_stratum(width = 0.22, fill = "white", color = "black", linewidth = 0.3) +
    label_layer("Wave 1", nudge = -0.42, hjust = 1) +
    label_layer("Wave 2", nudge = 0.42, hjust = 0) +
    scale_fill_manual(values = party_cols, guide = "none") +
    scale_x_discrete(labels = c("Wave 1", "Wave 2"), expand = expansion(add = c(0.85, 1.05))) +
    labs(x = NULL, y = NULL, title = title_text) +
    coord_cartesian(clip = "off") +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.margin = margin(10, 36, 10, 28),
      panel.grid = element_blank(),
      axis.text.y = element_blank()
    )
}

plot_attrition_coefs <- function(results, title, x_lab) {
  results |>
    ggplot(aes(x = estimate, y = forcats::fct_reorder(label, estimate))) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_errorbar(aes(xmin = conf.low, xmax = conf.high), orientation = "y", width = 0) +
    geom_point() +
    labs(title = title, x = x_lab, y = NULL) +
    theme_bw()
}
