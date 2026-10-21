# replicateEverything provenance: connector
# Figure S15. Figure 1 outcomes without video fixed effects.
# Execute with run_replication("here", "fig_s15").

library(fixest, quietly = TRUE, warn.conflicts = FALSE)
library(broom, quietly = TRUE, warn.conflicts = FALSE)
source("./helpers_labels.R")
source("./helpers_graphing.R")

make_fig_s15 <- function(data) {
  fixest::setFixest_notes(FALSE)
  s15_outcomes <- engagement_outcomes()
  lapply(c("wave_9", "wave_10"), function(D) {
    df <- data[[D]]
    lapply(s15_outcomes, function(Y) {
      lapply(unique(as.character(df$strategy_2)), function(S) {
        feols(
          as.formula(paste(v_outcomes[[Y]], "~ workshop_Treatment")),
          data = df |> filter(strategy_2 == S),
          cluster = ~ID
        ) |>
          tidy(conf.int = TRUE) |>
          filter(term == "workshop_Treatment") |>
          mutate(
            outcome = Y,
            strategy = S,
            horizon = if (D == "wave_9") "Wave 1" else "Wave 2"
          )
      }) |> bind_rows()
    }) |> bind_rows()
  }) |>
    bind_rows()
}

format_fig_s15 <- function(object) {
  plot <- plot_strategy_ates(
    object,
    engagement_outcomes(),
    "PAP reconciliation: pooled effects without video FE",
    "Cluster ID only (no video FE). Compare to primary Figure 1."
  )
  save_ggplot(plot, study_path("outputs", "fig_s15.png"), width = 12, height = 5)
  plot
}
