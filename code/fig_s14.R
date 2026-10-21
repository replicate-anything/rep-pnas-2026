# replicateEverything provenance: connector
# Figure S14. Post-treatment PTV, with pre-treatment PTV demeaned
# inside each estimation group.
# Execute with run_replication("here", "fig_s14").

library(estimatr, quietly = TRUE, warn.conflicts = FALSE)
library(broom, quietly = TRUE, warn.conflicts = FALSE)
source("./helpers_labels.R")
source("./helpers_graphing.R")

make_fig_s14 <- function(data) {
  df_w9_w10 <- data |>
    mutate(citysize = as.numeric(citysize))

  fit_ptv_group <- function(data, post_var, pre_var) {
    d <- data |>
      filter(is.finite(.data[[post_var]]), is.finite(.data[[pre_var]]), !is.na(workshop)) |>
      mutate(pre_within = .data[[pre_var]] - mean(.data[[pre_var]]))
    lm_robust(
      stats::reformulate("workshop * pre_within", response = post_var),
      data = d
    )
  }

  estimate_ptv_by_id <- function(post_prefix) {
    party_codes |>
      lapply(function(party) {
        pre_var <- paste0("ptv_w9_pre_", party)
        post_var <- paste0(post_prefix, party)
        id_var <- paste0("party_id_w9_", party)
        m_all <- fit_ptv_group(df_w9_w10, post_var, pre_var)
        m_id <- fit_ptv_group(df_w9_w10 |> filter(.data[[id_var]] == 1), post_var, pre_var)
        m_non <- fit_ptv_group(df_w9_w10 |> filter(.data[[id_var]] == 0), post_var, pre_var)
        bind_rows(
          tidy(m_all, conf.int = TRUE) |> filter(term == "workshop") |> mutate(group = "All respondents"),
          tidy(m_id, conf.int = TRUE) |> filter(term == "workshop") |> mutate(group = "Identifiers"),
          tidy(m_non, conf.int = TRUE) |> filter(term == "workshop") |> mutate(group = "Non-identifiers")
        ) |>
          mutate(party = party_names[[party]])
      }) |>
      bind_rows()
  }

  ptv_short <- estimate_ptv_by_id("ptv_w9_post_")
  ptv_long <- estimate_ptv_by_id("ptv_w10_")
  group_levels <- c("All respondents", "Identifiers", "Non-identifiers")
  party_order_ptv <- ptv_short |>
    filter(group == "All respondents") |>
    arrange(estimate) |>
    pull(party) |>
    as.character()
  estimates <- bind_rows(
    ptv_short |> mutate(horizon = "Wave 1"),
    ptv_long |> mutate(horizon = "Wave 2")
  ) |>
    mutate(
      horizon = factor(horizon, levels = horizon_wave_levels),
      party = factor(as.character(party), levels = party_order_ptv),
      group = factor(as.character(group), levels = group_levels)
    )

  save_rds_atomic(estimates, study_path("outputs", "fig_s14_estimates.rds"))
  format_fig_s14(estimates)
  estimates
}

format_fig_s14 <- function(object) {
  dodge_ptv <- position_dodge(width = 0.6)
  plot <- object |>
    ggplot(aes(x = estimate, y = party, color = group)) +
    geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
    geom_point(position = dodge_ptv, size = 2.5) +
    geom_errorbar(
      aes(xmin = conf.low, xmax = conf.high),
      orientation = "y",
      position = dodge_ptv,
      width = 0.2
    ) +
    geom_text(
      aes(label = sprintf("%.3f (p = %.3f)", estimate, p.value), group = group),
      position = dodge_ptv, hjust = 0.5, vjust = -0.7, size = 2.2, show.legend = FALSE
    ) +
    facet_grid(cols = vars(horizon)) +
    labs(
      x = "Effect on post-treatment PTV at the group's mean pre-treatment PTV",
      y = "Party",
      color = NULL,
      title = "PTV effects of digital literacy workshop"
    ) +
    theme_bw(base_size = 11) +
    theme(
      legend.position = "bottom",
      strip.background = element_rect(fill = "grey95", color = "grey80")
    )
  save_ggplot(plot, study_path("outputs", "fig_s14.png"), width = 10, height = 8)
  plot
}
