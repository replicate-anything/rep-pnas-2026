# replicateEverything provenance: connector
# Table S10. Short-term AfD triple interaction. Reads afd_3way.
# Execute with run_replication("here", "tab_s10").

source("./helpers_labels.R")
source("./helpers_estimation.R")

make_tab_s10 <- function(data) {
  data$short
}

format_tab_s10 <- function(object) {
  html <- format_model_html(
    object,
    afd_3way_coef_map,
    "Table S10. Treatment × AfD supporter × AfD video (Wave 1)."
  )
  write_text_atomic(as.character(html), study_path("outputs", "tab_s10.html"))
  html
}
