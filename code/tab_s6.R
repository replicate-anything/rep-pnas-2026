# replicateEverything provenance: connector
# Table S6. Emotional vs neutral, Report. Reads type_contrasts.
# Execute with run_replication("here", "tab_s6").

source("./helpers_labels.R")
source("./helpers_estimation.R")

make_tab_s6 <- function(data) {
  data$emo_neu_report
}

format_tab_s6 <- function(object) {
  html <- format_model_html(
    object,
    type_contrast_coef_map,
    "Table S6. Emotional vs politically neutral: Report (Wave 1)."
  )
  write_text_atomic(as.character(html), study_path("outputs", "tab_s6.html"))
  html
}
