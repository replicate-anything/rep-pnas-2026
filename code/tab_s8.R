# replicateEverything provenance: connector
# Table S8. Emotional vs entertainment, Report. Reads type_contrasts.
# Execute with run_replication("here", "tab_s8").

source("./helpers_labels.R")
source("./helpers_estimation.R")

make_tab_s8 <- function(data) {
  data$emo_ent_report
}

format_tab_s8 <- function(object) {
  html <- format_model_html(
    object,
    type_contrast_coef_map,
    "Table S8. Emotional vs entertainment: Report (Wave 1)."
  )
  write_text_atomic(as.character(html), study_path("outputs", "tab_s8.html"))
  html
}
