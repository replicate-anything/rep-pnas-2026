# replicateEverything provenance: connector
# Figure S7. Treatment × covariate interactions on attrition.
# Execute with run_replication("here", "fig_s7").

library(estimatr, quietly = TRUE, warn.conflicts = FALSE)
library(broom, quietly = TRUE, warn.conflicts = FALSE)
source("./helpers_labels.R")
source("./helpers_graphing.R")

make_fig_s7 <- function(data) {
  df <- data |>
    mutate(citysize = as.numeric(citysize))
  covs <- names(labels_en)
  names(covs) <- labels_en
  lapply(covs, function(v) {
    as.formula(paste0("attrit ~ workshop * ", v)) |>
      lm_robust(data = df |> mutate(across(all_of(v), \(x) as.numeric(scale(x))))) |>
      tidy(conf.int = TRUE) |>
      filter(grepl(":", term))
  }) |>
    bind_rows(.id = "label")
}

format_fig_s7 <- function(object) {
  plot <- plot_attrition_coefs(
    object,
    "Treatment × covariate interactions on attrition (LPM)",
    "Interaction estimate (Δ probability of attrition)"
  )
  save_ggplot(plot, study_path("outputs", "fig_s7.png"), width = 7, height = 12)
  plot
}
