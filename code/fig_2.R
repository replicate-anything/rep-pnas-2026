# replicateEverything provenance: connector
# Figure 2. Co-partisan split uses partisan (from party_preference_*).
# Execute with run_replication("here", "fig_2").

library(fixest, quietly = TRUE, warn.conflicts = FALSE)
source("./helpers_labels.R")
source("./helpers_graphing.R")

party_labels_az <- c("AfD", "BSW", "CDU/CSU", "FDP", "Grüne", "Linke", "SPD")

fit_lin_party_feols <- function(df, outcome_var, party_levels, party_code_map,
                                ref_party = party_levels[length(party_levels)]) {
  fixest::setFixest_notes(FALSE)
  interact_parties <- setdiff(party_levels, ref_party)
  df_m <- df |>
    filter(!is.na(.data[[outcome_var]]), !is.na(party), party %in% party_levels) |>
    mutate(party = factor(party, levels = party_levels))

  pi <- setNames(numeric(length(interact_parties)), interact_parties)
  for (p in interact_parties) {
    raw <- as.integer(df_m$party == p)
    pi[p] <- mean(raw, na.rm = TRUE)
    df_m[[paste0("pdm_", party_code_map[[p]])]] <- raw - pi[p]
  }

  pdm_terms <- paste0("pdm_", party_code_map[interact_parties])
  rhs <- paste0("workshop_Treatment * (", paste(pdm_terms, collapse = " + "), ")")
  fit <- feols(
    as.formula(paste(outcome_var, "~", rhs, "| video")),
    data = df_m,
    cluster = ~ID
  )

  b <- coef(fit)
  V <- vcov(fit)
  treat <- "workshop_Treatment"
  lincom <- function(cvec) {
    cvec <- cvec[names(b)]
    cvec[is.na(cvec)] <- 0
    est <- sum(cvec * b)
    se <- sqrt(as.numeric(t(cvec) %*% V %*% cvec))
    tibble(
      estimate = est,
      std.error = se,
      conf.low = est - 1.96 * se,
      conf.high = est + 1.96 * se,
      p.value = 2 * pnorm(-abs(est / se))
    )
  }

  c_ate <- setNames(rep(0, length(b)), names(b))
  c_ate[treat] <- 1
  out_ate <- lincom(c_ate) |> mutate(term = treat, party = "All parties")

  out_parties <- lapply(party_levels, function(p) {
    cvec <- setNames(rep(0, length(b)), names(b))
    cvec[treat] <- 1
    for (j in interact_parties) {
      nm <- paste0(treat, ":pdm_", party_code_map[[j]])
      if (!nm %in% names(b)) {
        next
      }
      cvec[nm] <- if (identical(p, j)) 1 - pi[j] else -pi[j]
    }
    lincom(cvec) |> mutate(term = paste0("TE_", party_code_map[[p]]), party = p)
  }) |> bind_rows()

  bind_rows(out_ate, out_parties) |> mutate(outcome_var = outcome_var)
}

wald_party_te_equal <- function(df, outcome_var, party_levels, party_code_map,
                                ref_party = party_levels[length(party_levels)]) {
  fixest::setFixest_notes(FALSE)
  interact_parties <- setdiff(party_levels, ref_party)
  df_m <- df |>
    filter(!is.na(.data[[outcome_var]]), !is.na(party), party %in% party_levels) |>
    mutate(party = factor(party, levels = party_levels))

  for (p in interact_parties) {
    raw <- as.integer(df_m$party == p)
    df_m[[paste0("pdm_", party_code_map[[p]])]] <- raw - mean(raw, na.rm = TRUE)
  }

  pdm_terms <- paste0("pdm_", party_code_map[interact_parties])
  rhs <- paste0("workshop_Treatment * (", paste(pdm_terms, collapse = " + "), ")")
  fit <- feols(
    as.formula(paste(outcome_var, "~", rhs, "| video")),
    data = df_m,
    cluster = ~ID
  )

  expected_names <- paste0("workshop_Treatment:pdm_", party_code_map[interact_parties])
  interact_names <- grep("^workshop_Treatment:pdm_", names(coef(fit)), value = TRUE)
  stopifnot(setequal(interact_names, expected_names))

  w <- wald(fit, keep = "^workshop_Treatment:pdm_", print = FALSE)
  stat_is_chi2 <- isTRUE(is.na(w$df2)) || is.null(w$df2)
  tibble(
    stat = as.numeric(w$stat),
    stat_name = if (stat_is_chi2) "chi2" else "F",
    df1 = as.integer(w$df1),
    df2 = if (stat_is_chi2) NA_real_ else as.numeric(w$df2),
    p.value = as.numeric(w$p),
    n_terms = length(interact_names)
  )
}

plot_fig_2 <- function(estimates) {
  fig2_outcomes <- engagement_outcomes()
  fig2_sample_order <- c("All", "Co-partisan", "Not co-partisan")
  fig2_sample_dodge_levels <- rev(fig2_sample_order)
  party_row_levels <- c("All parties", party_labels_az)
  dodge_fig2 <- position_dodge(width = 0.55)
  fig2_x_lims <- estimates |>
    summarise(
      lo = min(estimate, na.rm = TRUE),
      hi = max(estimate, na.rm = TRUE),
      .by = outcome
    ) |>
    mutate(
      pad = pmax(0.15 * (hi - lo), 0.005),
      xmin = lo - pad,
      xmax = hi + pad
    )
  fig2_x_breaks <- function(lims) {
    candidates <- pretty(lims, n = 5)
    span <- diff(range(lims))
    inset <- 0.14 * span
    inside <- candidates > (lims[1] + inset) & candidates < (lims[2] - inset)
    is_zero <- abs(candidates) < 1e-8 & candidates >= lims[1] & candidates <= lims[2]
    kept <- sort(unique(candidates[inside | is_zero]))
    if (length(kept) < 2) {
      candidates <- pretty(lims, n = 3)
      kept <- sort(unique(candidates[candidates >= lims[1] & candidates <= lims[2]]))
    }
    kept
  }
  fig2_x_scales <- lapply(fig2_outcomes, function(y) {
    r <- fig2_x_lims[as.character(fig2_x_lims$outcome) == y, ]
    scale_x_continuous(
      limits = c(r$xmin, r$xmax),
      expand = expansion(mult = 0),
      breaks = fig2_x_breaks
    )
  })
  estimates |>
    mutate(super = outcome_super(outcome)) |>
    ggplot(aes(x = estimate, y = party, color = sample)) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_errorbar(
      aes(xmin = conf.low, xmax = conf.high),
      width = 0.2,
      position = dodge_fig2
    ) +
    geom_point(size = 1.6, position = dodge_fig2) +
    facet_nested(. ~ super + outcome, scales = "free_x") +
    facetted_pos_scales(x = fig2_x_scales) +
    scale_y_discrete(limits = rev(party_row_levels)) +
    scale_color_discrete(
      limits = fig2_sample_dodge_levels,
      breaks = fig2_sample_order,
      guide = guide_legend(reverse = TRUE)
    ) +
    labs(
      x = "Average treatment effect",
      y = NULL,
      color = NULL,
      title = "Conditional effects by party and co-partisanship",
      subtitle = "Co-partisan: respondent supports the sending party. Video FE; SEs clustered by respondent"
    ) +
    theme_bw(base_size = 10) +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 8),
      axis.text.y = element_text(size = 9),
      strip.text = element_text(size = 10),
      panel.spacing.x = unit(14, "pt")
    )
}

make_fig_2 <- function(data) {
  fixest::setFixest_notes(FALSE)
  fig2_samples <- list("All" = NULL, "Co-partisan" = 1L, "Not co-partisan" = 0L)
  fig2_sample_order <- names(fig2_samples)
  fig2_sample_dodge_levels <- rev(fig2_sample_order)
  party_row_levels <- c("All parties", party_labels_az)
  fig2_outcomes <- engagement_outcomes()

  df_fig2 <- data |>
    filter(strategy_2 == strategies[1], !is.na(party), party %in% names(party_codes))

  estimates <- lapply(fig2_sample_order, function(samp) {
    df_s <- df_fig2
    if (!is.null(fig2_samples[[samp]])) {
      df_s <- df_s |> filter(partisan == fig2_samples[[samp]])
    }
    lapply(fig2_outcomes, function(Y) {
      fit_lin_party_feols(
        df = df_s,
        outcome_var = v_outcomes[[Y]],
        party_levels = party_labels_az,
        party_code_map = party_codes
      ) |>
        mutate(outcome = Y, sample = samp)
    }) |>
      bind_rows()
  }) |>
    bind_rows() |>
    mutate(
      outcome = factor(outcome, levels = fig2_outcomes),
      sample = factor(sample, levels = fig2_sample_dodge_levels),
      party = factor(party, levels = party_row_levels)
    )

  wald_tbl <- lapply(fig2_sample_order, function(samp) {
    df_s <- df_fig2
    if (!is.null(fig2_samples[[samp]])) {
      df_s <- df_s |> filter(partisan == fig2_samples[[samp]])
    }
    lapply(fig2_outcomes, function(Y) {
      wald_party_te_equal(
        df = df_s,
        outcome_var = v_outcomes[[Y]],
        party_levels = party_labels_az,
        party_code_map = party_codes
      ) |>
        mutate(outcome = Y, sample = samp)
    }) |>
      bind_rows()
  }) |>
    bind_rows()
  save_rds_atomic(wald_tbl, study_path("outputs", "fig_2_wald.rds"))
  save_rds_atomic(estimates, study_path("outputs", "fig_2_estimates.rds"))
  format_fig_2(estimates)
  estimates
}

format_fig_2 <- function(object) {
  plot <- plot_fig_2(object)
  save_ggplot(plot, study_path("outputs", "fig_2.png"), width = 12, height = 8)
  plot
}
