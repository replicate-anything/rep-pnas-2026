# replicateEverything provenance: connector
# Table S7. Emotional vs entertainment, Like. Reads type_contrasts.
# Execute with run_replication("here", "tab_s7").

source("./helpers_labels.R")
source("./helpers_estimation.R")

make_tab_s7 <- function(data) {
  data$emo_ent_like
}

format_tab_s7 <- function(object) {
  html <- format_model_html(
    object,
    type_contrast_coef_map,
    "Table S7. Emotional vs entertainment: Like (Wave 1)."
  )
  write_text_atomic(as.character(html), study_path("outputs", "tab_s7.html"))
  html
}
