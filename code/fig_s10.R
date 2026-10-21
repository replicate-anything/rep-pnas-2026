# replicateEverything provenance: connector
# Figure S10. Workshop effect on party ID, by prior identification.
# Uses party_id_w9_* and party_id_w10_*.
# Execute with run_replication("here", "fig_s10").

library(estimatr, quietly = TRUE, warn.conflicts = FALSE)
library(broom, quietly = TRUE, warn.conflicts = FALSE)
source("./helpers_labels.R")
source("./helpers_graphing.R")

make_fig_s10 <- function(data) {
  df <- data |>
    mutate(citysize = as.numeric(citysize))
  maintained <- party_codes |>
    lapply(function(party) {
      w9 <- paste0("party_id_w9_", party)
      w10 <- paste0("party_id_w10_", party)
      df_tmp <- df |>
        mutate(
          id_w9 = .data[[w9]],
          id_w9_demeaned = id_w9 - mean(id_w9, na.rm = TRUE),
          id_w10 = .data[[w10]]
        )
      m_all <- lm_robust(id_w10 ~ workshop * id_w9_demeaned, data = df_tmp)
      m_id <- lm_robust(id_w10 ~ workshop, data = df_tmp |> filter(id_w9 == 1))
      m_non <- lm_robust(id_w10 ~ workshop, data = df_tmp |> filter(id_w9 == 0))
      bind_rows(
        tidy(m_all, conf.int = TRUE) |> filter(term == "workshop") |> mutate(group = "All respondents"),
        tidy(m_id, conf.int = TRUE) |> filter(term == "workshop") |> mutate(group = "Identifiers"),
        tidy(m_non, conf.int = TRUE) |> filter(term == "workshop") |> mutate(group = "Non-identifiers")
      ) |>
        mutate(party = party_names[[party]])
    }) |>
    bind_rows()
  party_order <- maintained |>
    filter(group == "All respondents") |>
    arrange(estimate) |>
    pull(party)
  maintained |>
    mutate(
      party = factor(party, levels = party_order),
      group = factor(group, levels = c("All respondents", "Identifiers", "Non-identifiers"))
    )
}

format_fig_s10 <- function(object) {
  plot <- object |>
    ggplot(aes(x = estimate, y = party, color = group)) +
    geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
    geom_point(position = position_dodge(width = 0.6), size = 2.5) +
    geom_errorbar(
      aes(xmin = conf.low, xmax = conf.high),
      orientation = "y",
      position = position_dodge(width = 0.6),
      width = 0.2
    ) +
    geom_text(
      aes(label = sprintf("%.3f (p = %.3f)", estimate, p.value), group = group),
      position = position_dodge(width = 0.6),
      hjust = 0.5, vjust = -0.7, size = 2.5, show.legend = FALSE
    ) +
    labs(
      x = "Effect of workshop on party identification (Wave 2)",
      y = "Party",
      color = NULL,
      title = "Effects of digital literacy workshop on party identification"
    ) +
    theme_bw()
  save_ggplot(plot, study_path("outputs", "fig_s10.png"), width = 7, height = 9)
  plot
}
