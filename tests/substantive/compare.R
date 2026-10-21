# Numeric comparisons against manuscript goldens.
# Failure messages report a count and the largest gap, not the table.

compare_keyed <- function(got, expected, keys, measures, tolerance = 1e-5) {
  got <- as.data.frame(got, stringsAsFactors = FALSE)
  expected <- as.data.frame(expected, stringsAsFactors = FALSE)
  missing_got <- setdiff(c(keys, measures), names(got))
  missing_exp <- setdiff(c(keys, measures), names(expected))
  if (length(missing_got) || length(missing_exp)) {
    testthat::fail(sprintf(
      "Missing columns (got: %s; expected: %s).",
      paste(missing_got, collapse = ", "),
      paste(missing_exp, collapse = ", ")
    ))
  }
  for (k in keys) {
    got[[k]] <- as.character(got[[k]])
    expected[[k]] <- as.character(expected[[k]])
  }
  if (anyDuplicated(got[keys]) > 0L || anyDuplicated(expected[keys]) > 0L) {
    testthat::fail("Key columns do not identify a single row.")
  }
  merged <- merge(
    expected[c(keys, measures)],
    got[c(keys, measures)],
    by = keys,
    suffixes = c(".exp", ".got"),
    all = TRUE
  )
  got_key <- interaction(got[keys], drop = TRUE, lex.order = TRUE, sep = "\r")
  exp_key <- interaction(expected[keys], drop = TRUE, lex.order = TRUE, sep = "\r")
  n_only <- length(setdiff(got_key, exp_key)) + length(setdiff(exp_key, got_key))
  if (n_only > 0L) {
    testthat::fail(sprintf("%d keyed rows are present on only one side.", n_only))
  }
  n_bad <- 0L
  worst <- 0
  worst_col <- measures[[1]]
  for (col in measures) {
    a <- suppressWarnings(as.numeric(merged[[paste0(col, ".exp")]]))
    b <- suppressWarnings(as.numeric(merged[[paste0(col, ".got")]]))
    both_na <- is.na(a) & is.na(b)
    gap <- ifelse(both_na, 0, abs(a - b))
    gap[!is.finite(gap)] <- Inf
    n_bad <- n_bad + sum(gap > tolerance)
    if (length(gap) && max(gap) > worst) {
      worst <- max(gap)
      worst_col <- col
    }
  }
  if (n_bad > 0L) {
    testthat::fail(sprintf(
      "%d numeric cells differ (tolerance %g). Largest gap is %g on %s.",
      n_bad, tolerance, worst, worst_col
    ))
  }
  testthat::succeed()
  invisible(TRUE)
}

flows_from_lodes <- function(lodes) {
  d <- as.data.frame(lodes, stringsAsFactors = FALSE)
  d$x <- as.character(d$x)
  d$stratum <- as.character(d$stratum)
  w1 <- d[d$x == "Wave 1", c("alluvium", "stratum", "Freq", "Share")]
  w2 <- d[d$x == "Wave 2", c("alluvium", "stratum")]
  names(w1)[2] <- "from"
  names(w2)[2] <- "to"
  m <- merge(w1, w2, by = "alluvium")
  m$alluvium <- NULL
  m
}

flatten_models <- function(models) {
  rows <- lapply(names(models), function(nm) {
    td <- as.data.frame(models[[nm]]$tidy, stringsAsFactors = FALSE)
    gl <- as.data.frame(models[[nm]]$glance, stringsAsFactors = FALSE)
    nobs <- NA_real_
    if ("nobs" %in% names(gl)) {
      nobs <- as.numeric(gl$nobs[[1]])
    } else if ("N" %in% names(gl)) {
      nobs <- as.numeric(gl$N[[1]])
    }
    td$model <- nm
    td$nobs <- nobs
    td
  })
  do.call(rbind, rows)
}

star_count <- function(p) {
  ifelse(
    is.na(p),
    NA_real_,
    ifelse(p < 0.01, 3, ifelse(p < 0.05, 2, ifelse(p < 0.10, 1, 0)))
  )
}
