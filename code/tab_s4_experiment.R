# replicateEverything provenance: connector
# Table S4 experiment-sample video counts. From TikTok_gh/2_analysis.qmd.
# Execute with run_replication("here", "tab_s4_experiment").

library(tidyverse, quietly = TRUE, warn.conflicts = FALSE)
library(knitr, quietly = TRUE, warn.conflicts = FALSE)
source("./helpers_labels.R")

make_tab_s4_experiment <- function(data) {
  party_levels_s4 <- c("AfD", "BSW", "CDU/CSU", "FDP", "Grüne", "Linke", "SPD")
  s4_vids <- data |>
    distinct(video, .keep_all = TRUE) |>
    mutate(
      row_lab = ifelse(strategy_2 == "entertainment", "Entertainment", as.character(party)),
      col_lab = case_when(
        strategy_2 == strategies[1] ~ "emotional",
        strategy_2 == strategies[2] ~ "neutral",
        strategy_2 == "entertainment" ~ "entertainment"
      )
    )
  stopifnot(nrow(s4_vids) == 400L)
  s4_tab <- as.data.frame.matrix(with(s4_vids, table(row_lab, col_lab))) |>
    tibble::rownames_to_column("row_lab") |>
    transmute(
      row_lab = factor(row_lab, levels = c(party_levels_s4, "Entertainment")),
      emotional,
      neutral,
      entertainment,
      all = emotional + neutral + entertainment
    ) |>
    arrange(row_lab)
  bind_rows(
    s4_tab,
    summarise(s4_tab, row_lab = "Total", across(-row_lab, sum))
  )
}

format_tab_s4_experiment <- function(object) {
  show <- object |>
    transmute(
      Party = as.character(row_lab),
      `Emotional political` = emotional,
      `Neutral political` = neutral,
      Entertainment = entertainment,
      All = all
    )
  html <- knitr::kable(
    show,
    format = "html",
    caption = "Table S4. Strategy distribution across the 400 experiment videos"
  )
  write_text_atomic(as.character(html), study_path("outputs", "tab_s4_experiment.html"))
  html
}
