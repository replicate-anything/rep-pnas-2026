# replicateEverything provenance: connector
# Table S11. Long-term AfD triple interaction. Reads afd_3way.
# Execute with run_replication("here", "tab_s11").

source("./helpers_labels.R")
source("./helpers_estimation.R")

make_tab_s11 <- function(data) {
  data$long
}

format_tab_s11 <- function(object) {
  html <- format_model_html(
    object,
    afd_3way_coef_map,
    "Table S11. Treatment × AfD supporter × AfD video (Wave 2)."
  )
  write_text_atomic(as.character(html), study_path("outputs", "tab_s11.html"))
  html
}
