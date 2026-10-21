# replicateEverything provenance: connector
# Table S3. Published mean pairwise Cohen's kappa. coder_ratings.rds is absent.
# Execute with run_replication("here", "tab_s3").

library(knitr, quietly = TRUE, warn.conflicts = FALSE)
source("./helpers_labels.R")

make_tab_s3 <- function(data = NULL) {
  data.frame(
    anger = 0.492,
    fear = 0.453,
    superiority = 0.371,
    any_emotion = 0.404,
    stringsAsFactors = FALSE
  )
}

format_tab_s3 <- function(object) {
  html <- knitr::kable(
    object,
    format = "html",
    digits = 3,
    caption = "Table S3. Average Cohen's Kappa between coder pairings"
  )
  write_text_atomic(as.character(html), study_path("outputs", "tab_s3.html"))
  html
}
