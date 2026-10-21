local_pkg <- "C:/WZB Dropbox/Macartan Humphreys/5_github/replicate_everything/replicateEverything"
here_ok <- tryCatch(
  replicateEverything:::is_local_doi_query("here"),
  error = function(e) FALSE
)
if (!isTRUE(here_ok)) {
  pkgload::load_all(local_pkg, quiet = TRUE)
}
options(replicateEverything.quiet_run = TRUE)

study_root <- function() {
  normalizePath(testthat::test_path("..", ".."), winslash = "/", mustWork = TRUE)
}

golden_csv <- function(id) {
  utils::read.csv(
    file.path(study_root(), "tests", "goldens", paste0(id, ".csv")),
    stringsAsFactors = FALSE,
    check.names = FALSE,
    fileEncoding = "UTF-8"
  )
}

run_step <- function(id, given = "parents", format = FALSE) {
  root <- study_root()
  old <- getwd()
  setwd(root)
  on.exit(setwd(old), add = TRUE)
  replicateEverything::run_replication("here", id, given = given, format = format)
}

source(file.path(
  normalizePath(testthat::test_path(".."), winslash = "/", mustWork = TRUE),
  "substantive",
  "compare.R"
))
