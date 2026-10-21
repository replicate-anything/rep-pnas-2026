# replicateEverything provenance: connector
# Person-level repeat shares and the Wave 2 rows with no repeat exposure.
# Execute with run_replication("here", "repeated_videos").

library(tidyverse, quietly = TRUE, warn.conflicts = FALSE)
source("./helpers_labels.R")

make_repeated_videos <- function(data) {
  repeats <- data |>
    group_by(ID) |>
    summarize(
      n_seen = n(),
      n_repeats = sum(shown_both_waves, na.rm = TRUE),
      share = n_repeats / n_seen,
      .groups = "drop"
    )
  summary_df <- data.frame(
    median_share = as.numeric(stats::quantile(repeats$share, 0.5, names = FALSE)),
    p90_share = as.numeric(stats::quantile(repeats$share, 0.9, names = FALSE)),
    n_respondents = nrow(repeats),
    n_wave = nrow(data),
    n_norepeat = sum(data$shown_both_waves == 0, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
  norepeat <- data |>
    filter(shown_both_waves == 0)
  save_rds_atomic(summary_df, study_path("outputs", "repeat_summary.rds"))
  save_rds_atomic(
    list(norepeat = norepeat, summary = summary_df),
    study_path("outputs", "repeated_videos.rds")
  )
  summary_df
}
