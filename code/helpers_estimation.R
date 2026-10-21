# replicateEverything provenance: connector
# Estimation helpers shared by more than one step.
# Figure 1 writes this workshop model again in fig_1.R so that script stands alone.

source("./helpers_labels.R")

library(tidyverse, quietly = TRUE, warn.conflicts = FALSE)
library(fixest, quietly = TRUE, warn.conflicts = FALSE)
library(broom, quietly = TRUE, warn.conflicts = FALSE)
library(knitr, quietly = TRUE, warn.conflicts = FALSE)

silence_fixest <- function() {
  fixest::setFixest_notes(FALSE)
}

run_feols_tidy <- function(df, outcome_var, extra_cols = list()) {
  silence_fixest()
  feols(
    as.formula(paste(outcome_var, "~ workshop_Treatment | video")),
    data = df,
    cluster = ~ID
  ) |>
    tidy(conf.int = TRUE) |>
    bind_cols(as_tibble(extra_cols))
}

estimate_pooled_ates <- function(df, outcomes) {
  lapply(outcomes, function(Y) {
    lapply(unique(as.character(df$strategy_2)), function(S) {
      run_feols_tidy(
        df = df |> filter(strategy_2 == S),
        outcome_var = v_outcomes[[Y]],
        extra_cols = list(outcome = Y, strategy = S)
      )
    }) |> bind_rows()
  }) |> bind_rows()
}

tidy_fixest_one <- function(fit) {
  list(
    tidy = broom::tidy(fit, conf.int = TRUE),
    glance = broom::glance(fit)
  )
}

format_model_html <- function(models, coef_map, caption, model_names = names(models)) {
  terms <- names(coef_map)
  rows <- lapply(terms, function(term) {
    vals <- vapply(models, function(m) {
      hit <- m$tidy[m$tidy$term == term, , drop = FALSE]
      if (!nrow(hit)) {
        return(NA_real_)
      }
      hit$estimate[[1]]
    }, numeric(1))
    ses <- vapply(models, function(m) {
      hit <- m$tidy[m$tidy$term == term, , drop = FALSE]
      if (!nrow(hit)) {
        return(NA_real_)
      }
      hit$std.error[[1]]
    }, numeric(1))
    data.frame(
      term = unname(coef_map[[term]]),
      model = model_names,
      estimate = vals,
      std.error = ses,
      stringsAsFactors = FALSE
    )
  })
  long <- bind_rows(rows)
  wide_est <- tidyr::pivot_wider(
    long,
    id_cols = term,
    names_from = model,
    values_from = estimate
  )
  se_wide <- tidyr::pivot_wider(
    long,
    id_cols = term,
    names_from = model,
    values_from = std.error
  )
  se_wide$term <- paste0(se_wide$term, " (SE)")
  show <- bind_rows(wide_est, se_wide)
  nobs <- vapply(models, function(m) {
    g <- m$glance
    if ("nobs" %in% names(g)) as.numeric(g$nobs[[1]]) else NA_real_
  }, numeric(1))
  names(nobs) <- model_names
  nobs_row <- as.data.frame(as.list(nobs), check.names = FALSE, stringsAsFactors = FALSE)
  nobs_row$term <- "Num. obs."
  show <- bind_rows(show, nobs_row[, names(show), drop = FALSE])
  knitr::kable(show, format = "html", digits = 3, caption = caption)
}
