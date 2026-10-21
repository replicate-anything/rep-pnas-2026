# replicateEverything provenance: connector
# Table S13. PAP H1a on emotional political videos, Wave 1.
# Execute with run_replication("here", "tab_s13").

source("./helpers_labels.R")
source("./helpers_estimation.R")

make_tab_s13 <- function(data) {
  silence_fixest()
  df_h1_emo <- data |>
    filter(strategy_2 == strategies[1]) |>
    mutate(
      engage = rowMeans(cbind(videoSharep1r, videoLikep1r, videoBookmarkp1r), na.rm = FALSE)
    )
  h1_outcomes <- c(
    Like = v_outcomes[["Like"]],
    Share = v_outcomes[["Share"]],
    Bookmark = v_outcomes[["Bookmark"]],
    Engage = "engage"
  )
  models <- lapply(h1_outcomes, function(y) {
    feols(as.formula(paste0(y, " ~ workshop_Treatment | video")), data = df_h1_emo, cluster = ~ID)
  })
  h1_comp <- c("Like", "Share", "Bookmark")
  p_h1_raw <- sapply(models[h1_comp], \(m) coeftable(m)["workshop_Treatment", "Pr(>|t|)"])
  p_h1_bh <- p.adjust(p_h1_raw, method = "BH")
  packed <- lapply(models, tidy_fixest_one)
  list(models = packed, bh = as.numeric(p_h1_bh), bh_names = names(p_h1_bh))
}

format_tab_s13 <- function(object) {
  html <- format_model_html(
    object$models,
    c("workshop_Treatment" = "Treatment"),
    "Table S13. PAP H1a reconciliation on emotional political videos (Wave 1)."
  )
  write_text_atomic(as.character(html), study_path("outputs", "tab_s13.html"))
  html
}
