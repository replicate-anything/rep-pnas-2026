# replicateEverything provenance: connector
# Numbers quoted in the manuscript. Reads baked parent objects only.
# The returned table has columns description and value.
# Execute with run_replication("here", "key_numbers").

library(knitr, quietly = TRUE, warn.conflicts = FALSE)
source("./helpers_labels.R")

# Save key numbers used in the paper for display in key numbers table.
key_numbers_fig_1 <- function(fig1, wave_9) {
  wave1_effect <- function(outcome_name) {
    hit <- fig1[
      as.character(fig1$horizon) == "Wave 1" &
        as.character(fig1$strategy) == strategies[1] &
        as.character(fig1$outcome) == outcome_name &
        as.character(fig1$term) == "workshop_Treatment",
      ,
      drop = FALSE
    ]
    hit$estimate[[1]]
  }
  controls <- wave_9[
    wave_9$strategy_2 == strategies[1] & wave_9$workshop_Treatment == 0,
    ,
    drop = FALSE
  ]
  like <- wave1_effect("Like")
  report <- wave1_effect("Report")
  bookmark <- wave1_effect("Bookmark")
  like_baseline <- mean(controls$videoLikep1r, na.rm = TRUE)
  report_baseline <- mean(controls$videoCommentTickedp1r, na.rm = TRUE)
  data.frame(
    description = c(
      "Bookmark effect, percentage points",
      "Like effect, percentage points",
      "Like baseline, percent",
      "Report effect, percentage points",
      "Report baseline, percent"
    ),
    value = c(
      sprintf("%.1f", 100 * bookmark),
      sprintf("%.0f", -100 * like),
      sprintf("%.0f", 100 * like_baseline),
      sprintf("%.1f", 100 * report),
      sprintf("%.0f", 100 * report_baseline)
    ),
    stringsAsFactors = FALSE
  )
}

# Save key numbers used in the paper for display in key numbers table.
key_numbers_fig_2 <- function(wald) {
  wald_p <- function(outcome_name) {
    hit <- wald[
      as.character(wald$outcome) == outcome_name & as.character(wald$sample) == "All",
      ,
      drop = FALSE
    ]
    hit$p.value[[1]]
  }
  data.frame(
    description = c(
      "Wald test for likes, p-value",
      "Wald test for reports, p-value"
    ),
    value = c(
      sprintf("%.3f", wald_p("Like")),
      sprintf("%.3f", wald_p("Report"))
    ),
    stringsAsFactors = FALSE
  )
}

# Save key numbers used in the paper for display in key numbers table.
key_numbers_repeated_videos <- function(rep) {
  data.frame(
    description = c(
      "Repeated-video share, median percent",
      "Repeated-video share, 90th percentile percent"
    ),
    value = c(
      sprintf("%.0f", 100 * rep$median_share),
      sprintf("%.0f", 100 * rep$p90_share)
    ),
    stringsAsFactors = FALSE
  )
}

# Save key numbers used in the paper for display in key numbers table.
key_numbers_tab_s9 <- function(attrit) {
  overall <- attrit$rate[attrit$arm == "Overall"]
  data.frame(
    description = "Overall attrition, percent",
    value = sprintf("%.1f", overall),
    stringsAsFactors = FALSE
  )
}

# Save key numbers used in the paper for display in key numbers table.
key_numbers_fig_s14 <- function(ptv) {
  ptv_row <- function(horizon_name) {
    ptv[
      as.character(ptv$party) == "AfD" &
        as.character(ptv$group) == "Identifiers" &
        as.character(ptv$horizon) == horizon_name,
      ,
      drop = FALSE
    ]
  }
  p1 <- ptv_row("Wave 1")
  p2 <- ptv_row("Wave 2")
  data.frame(
    description = c(
      "AfD PTV among identifiers, wave 1",
      "AfD PTV among identifiers, wave 1, p-value",
      "AfD PTV among identifiers, wave 2",
      "AfD PTV among identifiers, wave 2, p-value"
    ),
    value = c(
      sprintf("%+.3f", p1$estimate),
      sprintf("%.3f", p1$p.value),
      sprintf("%+.3f", p2$estimate),
      sprintf("%.3f", p2$p.value)
    ),
    stringsAsFactors = FALSE
  )
}

# Save key numbers used in the paper for display in key numbers table.
key_numbers_tab_s12 <- function(models) {
  id_row <- models$conditional_afd$tidy
  id_row <- id_row[id_row$term == "workshop", , drop = FALSE]
  data.frame(
    description = c(
      "AfD identification effect, percentage points",
      "AfD identification effect, p-value"
    ),
    value = c(
      sprintf("%.1f", -100 * id_row$estimate[[1]]),
      sprintf("%.3f", id_row$p.value[[1]])
    ),
    stringsAsFactors = FALSE
  )
}

make_key_numbers <- function(data) {
  parts <- rbind(
    key_numbers_fig_1(data$fig_1_estimates, data$wave_9),
    key_numbers_fig_2(data$fig_2_wald),
    key_numbers_repeated_videos(data$repeat_summary),
    key_numbers_tab_s9(data$tab_s9_estimates),
    key_numbers_fig_s14(data$fig_s14_estimates),
    key_numbers_tab_s12(data$tab_s12_estimates)
  )
  order <- c(
    "Bookmark effect, percentage points",
    "AfD PTV among identifiers, wave 1",
    "AfD PTV among identifiers, wave 1, p-value",
    "AfD PTV among identifiers, wave 2",
    "AfD PTV among identifiers, wave 2, p-value",
    "AfD identification effect, percentage points",
    "AfD identification effect, p-value",
    "Like effect, percentage points",
    "Like baseline, percent",
    "Report effect, percentage points",
    "Report baseline, percent",
    "Repeated-video share, median percent",
    "Repeated-video share, 90th percentile percent",
    "Overall attrition, percent",
    "Wald test for likes, p-value",
    "Wald test for reports, p-value"
  )
  parts <- parts[match(order, parts$description), , drop = FALSE]
  rownames(parts) <- NULL
  parts
}

format_key_numbers <- function(object) {
  html <- knitr::kable(
    object,
    format = "html",
    caption = "Numbers quoted in the manuscript"
  )
  write_text_atomic(as.character(html), study_path("outputs", "key_numbers.html"))
  html
}
