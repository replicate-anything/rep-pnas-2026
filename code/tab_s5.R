# replicateEverything provenance: connector
# Table S5. Emotional vs neutral, Like. Reads type_contrasts.
# Execute with run_replication("here", "tab_s5").

source("./helpers_labels.R")
source("./helpers_estimation.R")

make_tab_s5 <- function(data) {
  data$emo_neu_like
}

format_tab_s5 <- function(object) {
  html <- format_model_html(
    object,
    type_contrast_coef_map,
    "Table S5. Emotional vs politically neutral: Like (Wave 1)."
  )
  write_text_atomic(as.character(html), study_path("outputs", "tab_s5.html"))
  html
}
