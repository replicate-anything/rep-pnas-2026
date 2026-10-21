# replicateEverything provenance: connector
# Table S1. Video and behavior counts by horizon.
# Execute with run_replication("here", "tab_s1").

library(tidyverse, quietly = TRUE, warn.conflicts = FALSE)
library(knitr, quietly = TRUE, warn.conflicts = FALSE)
source("./helpers_labels.R")

make_tab_s1 <- function(data) {
  one <- function(d, horizon) {
    respondents <- dplyr::n_distinct(d$ID)
    tibble(
      horizon = horizon,
      respondents = respondents,
      videos_per_respondent = nrow(d) / respondents,
      watched = nrow(d),
      anger = sum(d$content == "political" & d$strategy == "anger", na.rm = TRUE),
      fear = sum(d$content == "political" & d$strategy == "fear", na.rm = TRUE),
      superiority = sum(d$content == "political" & d$strategy == "superiority", na.rm = TRUE),
      neutral = sum(d$content == "political" & d$strategy == "neutral", na.rm = TRUE),
      entertainment = sum(d$content == "entertainment", na.rm = TRUE),
      like = sum(d$videoLikep1r == 1, na.rm = TRUE),
      share = sum(d$videoSharep1r == 1, na.rm = TRUE),
      bookmark = sum(d$videoBookmarkp1r == 1, na.rm = TRUE),
      report = sum(d$videoCommentTickedp1r == 1, na.rm = TRUE)
    )
  }
  bind_rows(one(data$wave_9, "Short-term"), one(data$wave_10, "Long-term"))
}

format_tab_s1 <- function(object) {
  wide <- object |>
    tidyr::pivot_longer(-horizon, names_to = "statistic", values_to = "value") |>
    tidyr::pivot_wider(names_from = horizon, values_from = value)
  html <- knitr::kable(
    wide,
    format = "html",
    digits = 2,
    caption = "Table S1. Descriptives on videos and video behavior"
  )
  write_text_atomic(as.character(html), study_path("outputs", "tab_s1.html"))
  html
}
