# replicateEverything provenance: connector
# Figure S5. Treatment × content-type interactions.
# Execute with run_replication("here", "fig_s5").

library(haven, quietly = TRUE, warn.conflicts = FALSE)
library(fixest, quietly = TRUE, warn.conflicts = FALSE)
source("./helpers_labels.R")
source("./helpers_graphing.R")

make_fig_s5 <- function(data) {
  fixest::setFixest_notes(FALSE)
  outcome_order_did <- engagement_outcomes()
  contrast_specs <- list(
    "Emotional vs neutral\n× treatment" = strategies,
    "Emotional vs entertainment\n× treatment" = c(strategies[1], "entertainment")
  )
  horizon_data <- list("Wave 1" = data$wave_9, "Wave 2" = data$wave_10)
  bind_rows(lapply(names(horizon_data), function(h) {
    bind_rows(lapply(names(contrast_specs), function(c_lab) {
      df <- horizon_data[[h]] |>
        filter(strategy_2 %in% contrast_specs[[c_lab]]) |>
        mutate(
          type_dummy = as.integer(strategy_2 == strategies[1]),
          across(all_of(unname(v_outcomes)), \(z) as.numeric(haven::zap_labels(z)))
        )
      bind_rows(lapply(outcome_order_did, function(Y) {
        feols(
          as.formula(paste0(v_outcomes[[Y]], " ~ workshop_Treatment * type_dummy | video")),
          data = df,
          cluster = ~ID
        ) |>
          broom::tidy(conf.int = TRUE) |>
          filter(term == "workshop_Treatment:type_dummy") |>
          mutate(horizon = h, contrast = c_lab, outcome = Y)
      }))
    }))
  }))
}

format_fig_s5 <- function(object) {
  outcomes <- engagement_outcomes()
  contrast_levels <- rev(c(
    "Emotional vs neutral\n× treatment",
    "Emotional vs entertainment\n× treatment"
  ))
  plot <- object |>
    mutate(
      horizon = factor(horizon, levels = horizon_wave_levels),
      outcome = factor(outcome, levels = outcomes),
      contrast = factor(contrast, levels = contrast_levels),
      super = outcome_super(outcome)
    ) |>
    ggplot(aes(x = estimate, y = contrast)) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.2) +
    geom_point(size = 1.8) +
    facet_nested(horizon ~ super + outcome, scales = "free_x") +
    facetted_pos_scales(x = shared_binary_x_scales(object, outcomes)) +
    labs(
      x = "Treatment × type interaction",
      y = NULL,
      title = "Treatment effect on the emotional-minus-other gap",
      subtitle = "Pairwise samples; video FE; cluster-robust 95% CIs by respondent"
    ) +
    theme_bw(base_size = 10) +
    theme(
      axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 9),
      axis.text.y = element_text(size = 9),
      strip.text = element_text(size = 10),
      panel.spacing.y = unit(6, "pt")
    )
  save_ggplot(plot, study_path("outputs", "fig_s5.png"), width = 12, height = 5.5)
  plot
}
