# replicateEverything provenance: connector
# Table S12. AfD identification. Uses party_id_w9_afd and party_id_w10_afd.
# Execute with run_replication("here", "tab_s12").

library(estimatr, quietly = TRUE, warn.conflicts = FALSE)
source("./helpers_labels.R")
source("./helpers_estimation.R")

make_tab_s12 <- function(data) {
  df <- data |>
    mutate(citysize = as.numeric(citysize)) |>
    mutate(
      party_id_w9_afd_demeaned = party_id_w9_afd - mean(party_id_w9_afd, na.rm = TRUE)
    )
  models <- list(
    identification = lm_robust(
      party_id_w10_afd ~ workshop * party_id_w9_afd_demeaned,
      data = df
    ),
    conditional_afd = lm_robust(
      party_id_w10_afd ~ workshop,
      data = df |> filter(party_id_w9_afd == 1)
    ),
    conditional_non_afd = lm_robust(
      party_id_w10_afd ~ workshop,
      data = df |> filter(party_id_w9_afd == 0)
    )
  )
  packed <- lapply(models, function(m) {
    list(tidy = broom::tidy(m, conf.int = TRUE), glance = broom::glance(m))
  })
  save_rds_atomic(packed, study_path("outputs", "tab_s12_estimates.rds"))
  format_tab_s12(packed)
  packed
}

format_tab_s12 <- function(object) {
  html <- format_model_html(
    object,
    c(
      "(Intercept)" = "Intercept",
      "workshop" = "Treatment (workshop)",
      "party_id_w9_afd_demeaned" = "AfD ID (Wave 1, demeaned)",
      "workshop:party_id_w9_afd_demeaned" = "Treatment × AfD ID (demeaned)"
    ),
    "Table S12. Effect of the workshop on AfD identification (Wave 2).",
    model_names = c("All respondents", "AfD identifiers", "Non-AfD respondents")
  )
  write_text_atomic(as.character(html), study_path("outputs", "tab_s12.html"))
  html
}
