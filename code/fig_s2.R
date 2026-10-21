# replicateEverything provenance: connector
# Figure S2. Comment classification shares.
# Execute with run_replication("here", "fig_s2").

library(tidyverse, quietly = TRUE, warn.conflicts = FALSE)
library(scales, quietly = TRUE, warn.conflicts = FALSE)
source("./helpers_labels.R")

make_fig_s2 <- function(data) {
  levels_vec <- c(
    "0 = setup skepticism",
    "1 = platform-focused",
    "2 = content-focused",
    "3 = other/unclear"
  )
  data |>
    mutate(
      scale_label = fct_explicit_na(factor(scale_label), na_level = "NA"),
      scale_label = fct_relevel(scale_label, levels_vec, after = 0)
    ) |>
    count(scale_label, name = "n") |>
    mutate(pct = n / sum(n))
}

format_fig_s2 <- function(object) {
  plot <- object |>
    ggplot(aes(x = scale_label, y = pct)) +
    geom_col(width = 0.7) +
    geom_text(aes(label = percent(pct, accuracy = 0.1)), vjust = -0.25, size = 4) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(x = NULL, y = "Share of comments", title = "Comment classification") +
    theme_bw(base_size = 13) +
    theme(panel.grid.major.x = element_blank())
  save_ggplot(plot, study_path("outputs", "fig_s2.png"), width = 8, height = 4)
  plot
}
