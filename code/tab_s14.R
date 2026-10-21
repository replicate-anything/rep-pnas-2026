# replicateEverything provenance: connector
# Table S14. Pairwise outcome correlations on emotional political videos.
# Execute with run_replication("here", "tab_s14").

library(tidyverse, quietly = TRUE, warn.conflicts = FALSE)
library(haven, quietly = TRUE, warn.conflicts = FALSE)
library(fixest, quietly = TRUE, warn.conflicts = FALSE)
source("./helpers_labels.R")

make_tab_s14 <- function(data) {
  fixest::setFixest_notes(FALSE)
  oc_labs <- c("Like", "Share", "Bookmark", "Report", "Watch time", "Skip")
  oc_vars <- c(
    Like = "videoLikep1r",
    Share = "videoSharep1r",
    Bookmark = "videoBookmarkp1r",
    Report = "videoCommentTickedp1r",
    `Watch time` = "watch_percentage_",
    Skip = "skipped_video_"
  )
  pair_cor <- function(x, y) {
    ok <- is.finite(x) & is.finite(y)
    n <- sum(ok)
    if (n < 3) {
      return(list(r = NA_real_, n = n))
    }
    list(r = suppressWarnings(cor(x[ok], y[ok])), n = n)
  }
  cluster_cor_diff_p <- function(yi, yj, mod, id) {
    ok <- is.finite(yi) & is.finite(yj) & is.finite(mod) & !is.na(id)
    if (sum(ok) < 8L) {
      return(NA_real_)
    }
    mod_ok <- as.numeric(mod[ok])
    if (length(unique(mod_ok)) < 2L) {
      return(NA_real_)
    }
    yi_ok <- as.numeric(yi[ok])
    yj_ok <- as.numeric(yj[ok])
    s_i <- stats::sd(yi_ok)
    s_j <- stats::sd(yj_ok)
    if (!is.finite(s_i) || !is.finite(s_j) || s_i <= 0 || s_j <= 0) {
      return(NA_real_)
    }
    d <- data.frame(
      yi_z = as.numeric(scale(yi_ok)),
      yj_z = as.numeric(scale(yj_ok)),
      mod = mod_ok,
      ID = as.character(id[ok]),
      stringsAsFactors = FALSE
    )
    fit <- tryCatch(feols(yi_z ~ yj_z * mod, data = d, cluster = ~ID), error = function(e) NULL)
    if (is.null(fit)) {
      return(NA_real_)
    }
    td <- broom::tidy(fit)
    hit <- td$term[grepl(":", td$term, fixed = TRUE)]
    if (length(hit) != 1L) {
      return(NA_real_)
    }
    as.numeric(td$p.value[td$term == hit])
  }
  oc_num <- function(x) {
    if (inherits(x, "haven_labelled")) {
      x <- haven::zap_labels(x)
    }
    as.numeric(x)
  }
  df_oc <- data |>
    filter(strategy_2 == strategies[1]) |>
    transmute(
      ID = as.character(ID),
      mod_treat = as.integer(workshop_Treatment),
      across(all_of(unname(oc_vars)), oc_num)
    )
  names(df_oc)[match(unname(oc_vars), names(df_oc))] <- names(oc_vars)

  dat_a <- df_oc[df_oc$mod_treat == 1, , drop = FALSE]
  dat_b <- df_oc[df_oc$mod_treat == 0, , drop = FALSE]
  out <- vector("list", length(oc_labs) * length(oc_labs))
  k <- 0L
  for (i in seq_along(oc_labs)) {
    for (j in seq_along(oc_labs)) {
      k <- k + 1L
      rn <- oc_labs[i]
      cn <- oc_labs[j]
      if (identical(rn, cn)) {
        out[[k]] <- tibble(
          row = rn, col = cn, r_t = NA_real_, r_c = NA_real_,
          delta = NA_real_, p = NA_real_, blank = TRUE
        )
        next
      }
      pa <- pair_cor(dat_a[[rn]], dat_a[[cn]])
      pb <- pair_cor(dat_b[[rn]], dat_b[[cn]])
      p <- cluster_cor_diff_p(df_oc[[rn]], df_oc[[cn]], df_oc$mod_treat, df_oc$ID)
      out[[k]] <- tibble(
        row = rn, col = cn, r_t = pa$r, r_c = pb$r,
        delta = pa$r - pb$r, p = p, blank = FALSE
      )
    }
  }
  bind_rows(out)
}

format_tab_s14 <- function(object) {
  html <- knitr::kable(
    object,
    format = "html",
    digits = 2,
    caption = "Table S14. Pairwise outcome correlations (emotional political videos, Wave 1)."
  )
  write_text_atomic(as.character(html), study_path("outputs", "tab_s14.html"))
  html
}
