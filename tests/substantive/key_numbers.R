# Compares formatted manuscript numbers. The study object has no command names.
substantive_check_key_numbers <- function(object) {
  expected <- utils::read.csv(
    file.path(study_root(), "tests", "goldens", "key_numbers.csv"),
    stringsAsFactors = FALSE,
    colClasses = "character",
    fileEncoding = "UTF-8"
  )
  got <- as.data.frame(object, stringsAsFactors = FALSE)
  if (!identical(names(got), c("description", "value"))) {
    testthat::fail("key_numbers must have columns description and value only.")
  }
  expected$description <- as.character(expected$description)
  expected$value <- as.character(expected$value)
  got$description <- as.character(got$description)
  got$value <- as.character(got$value)
  merged <- merge(got, expected, by = "description", all = TRUE, suffixes = c(".got", ".exp"))
  if (any(is.na(merged$value.got) | is.na(merged$value.exp))) {
    testthat::fail("Key-number rows do not match the manuscript set.")
  }
  bad <- merged$value.got != merged$value.exp
  if (any(bad)) {
    testthat::fail(paste(
      sprintf("%s expected %s got %s", merged$description[bad], merged$value.exp[bad], merged$value.got[bad]),
      collapse = "; "
    ))
  }
  testthat::expect_equal(nrow(merged), 16L)
}
