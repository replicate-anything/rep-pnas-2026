# replicateEverything provenance: connector
# Table S9. Attrition by treatment condition.
# Execute with run_replication("here", "tab_s9").

library(tidyverse, quietly = TRUE, warn.conflicts = FALSE)
library(estimatr, quietly = TRUE, warn.conflicts = FALSE)
library(broom, quietly = TRUE, warn.conflicts = FALSE)
library(knitr, quietly = TRUE, warn.conflicts = FALSE)
source("./helpers_labels.R")

make_tab_s9 <- function(data) {
  df <- data |>
    mutate(citysize = as.numeric(citysize))
  attrit_counts <- df |>
    mutate(arm = ifelse(workshop == 1, "Treatment", "Control")) |>
    count(arm, attrit) |>
    tidyr::complete(arm, attrit = c(0, 1), fill = list(n = 0))
  attrit_summary <- attrit_counts |>
    group_by(arm) |>
    summarise(
      original_n = sum(n),
      retained = sum(n[attrit == 0]),
      attrited = sum(n[attrit == 1]),
      .groups = "drop"
    ) |>
    mutate(rate = 100 * attrited / original_n)
  attrit_df <- df |>
    mutate(treat = as.integer(workshop == 1))
  attrit_test <- lm_robust(attrit ~ treat, data = attrit_df)
  attrit_test_td <- tidy(attrit_test, conf.int = TRUE) |> filter(term == "treat")
  overall <- attrit_summary |>
    summarise(
      arm = "Overall",
      original_n = sum(original_n),
      retained = sum(retained),
      attrited = sum(attrited)
    ) |>
    mutate(rate = 100 * attrited / original_n)
  rows <- bind_rows(
    attrit_summary |> arrange(arm != "Control", arm),
    overall
  )
  out <- list(rows = rows, test = attrit_test_td)
  save_rds_atomic(rows, study_path("outputs", "tab_s9_estimates.rds"))
  format_tab_s9(out)
  out
}

format_tab_s9 <- function(object) {
  html <- knitr::kable(
    object$rows,
    format = "html",
    digits = 2,
    caption = "Table S9. Attrition by treatment condition"
  )
  write_text_atomic(as.character(html), study_path("outputs", "tab_s9.html"))
  html
}
