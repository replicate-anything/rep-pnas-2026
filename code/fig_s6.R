# replicateEverything provenance: connector
# Figure S6. Attrition on standardized pre-treatment covariates.
# Execute with run_replication("here", "fig_s6").

library(estimatr, quietly = TRUE, warn.conflicts = FALSE)
library(broom, quietly = TRUE, warn.conflicts = FALSE)
source("./helpers_labels.R")
source("./helpers_graphing.R")

make_fig_s6 <- function(data) {
  df <- data |>
    mutate(citysize = as.numeric(citysize))
  stopifnot(n_distinct(df$id_w9) == nrow(df))
  covs <- names(labels_en)
  names(covs) <- labels_en
  lapply(covs, function(v) {
    as.formula(paste0("attrit ~ ", v)) |>
      lm_robust(data = df |> mutate(across(all_of(v), \(x) as.numeric(scale(x))))) |>
      tidy(conf.int = TRUE) |>
      filter(term == v)
  }) |>
    bind_rows(.id = "label")
}

format_fig_s6 <- function(object) {
  plot <- plot_attrition_coefs(
    object,
    "Covariate main effects on attrition (LPM)",
    "Covariate estimate (Δ probability of attrition)"
  )
  save_ggplot(plot, study_path("outputs", "fig_s6.png"), width = 7, height = 12)
  plot
}
