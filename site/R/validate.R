# validate.R — pre-publish sanity checks
# Call validate_artifacts(joined) before writing any output.
# Stops with an informative message if any check fails.

validate_joined <- function(joined) {
  msgs <- character(0)

  n_countries <- length(unique(joined$location_id))
  if (n_countries < 100) {
    msgs <- c(msgs, sprintf("Expected >= 100 countries, got %d", n_countries))
  }

  n_conditions <- length(unique(joined$cause_id))
  if (n_conditions < 100) {
    msgs <- c(msgs, sprintf("Expected >= 100 conditions, got %d", n_conditions))
  }

  years_present <- sort(unique(joined$year))
  if (!all(c(1990L, 2023L) %in% years_present)) {
    msgs <- c(msgs, sprintf(
      "Expected years 1990 and 2023, found: %s",
      paste(years_present, collapse = ", ")
    ))
  }

  required_cols <- c("location_id", "location_name", "cause_id", "cause_name",
                     "year", "dalys", "attention_score")
  missing_cols <- setdiff(required_cols, names(joined))
  if (length(missing_cols) > 0) {
    msgs <- c(msgs, sprintf("Missing required columns: %s", paste(missing_cols, collapse = ", ")))
  } else {
    na_dalys <- sum(is.na(joined$dalys))
    if (na_dalys > 0) {
      msgs <- c(msgs, sprintf("%d rows have NA dalys", na_dalys))
    }
  }

  if (length(msgs) > 0) {
    stop(paste("Artifact validation failed:\n", paste(msgs, collapse = "\n  ")))
  }

  invisible(TRUE)
}

validate_opportunities <- function(opps) {
  if (nrow(opps) == 0) stop("No eligible opportunities found — check eligibility threshold")
  invisible(TRUE)
}

validate_site_artifact_files <- function(data_dir,
                                         expected_years = c(1990L, 2023L),
                                         min_countries = 100L,
                                         min_conditions = 100L) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required to validate site artifacts.", call. = FALSE)
  }

  required_files <- file.path(
    data_dir,
    c(
      "metadata.json",
      "countries.json",
      "conditions.json",
      "country_summaries.json",
      "opportunities.json"
    )
  )
  missing_files <- required_files[!file.exists(required_files)]
  if (length(missing_files) > 0) {
    stop("Missing site artifact(s): ", paste(missing_files, collapse = ", "), call. = FALSE)
  }

  metadata <- jsonlite::fromJSON(file.path(data_dir, "metadata.json"))
  countries <- jsonlite::fromJSON(file.path(data_dir, "countries.json"))
  conditions <- jsonlite::fromJSON(file.path(data_dir, "conditions.json"))
  summaries <- jsonlite::fromJSON(file.path(data_dir, "country_summaries.json"))
  opportunities <- jsonlite::fromJSON(file.path(data_dir, "opportunities.json"))

  msgs <- character(0)

  missing_meta <- setdiff(
    c("build_time", "years", "n_countries", "n_conditions", "n_eligible_opps"),
    names(metadata)
  )
  if (length(missing_meta) > 0) {
    msgs <- c(msgs, paste("metadata missing keys:", paste(missing_meta, collapse = ", ")))
  }

  years <- sort(as.integer(unlist(metadata$years)))
  if (!all(expected_years %in% years)) {
    msgs <- c(msgs, sprintf(
      "metadata years missing expected value(s): expected %s, found %s",
      paste(expected_years, collapse = ", "),
      paste(years, collapse = ", ")
    ))
  }

  for (col in c("location_id", "location_name")) {
    if (!col %in% names(countries)) msgs <- c(msgs, paste("countries missing column:", col))
  }
  for (col in c("cause_id", "cause_name", "attention_score")) {
    if (!col %in% names(conditions)) msgs <- c(msgs, paste("conditions missing column:", col))
  }
  for (col in c("location_id", "location_name", "year")) {
    if (!col %in% names(summaries)) msgs <- c(msgs, paste("country_summaries missing column:", col))
  }
  for (col in c("location_id", "cause_id", "year", "mismatch_share")) {
    if (!col %in% names(opportunities)) msgs <- c(msgs, paste("opportunities missing column:", col))
  }

  if (nrow(countries) < min_countries) {
    msgs <- c(msgs, sprintf("expected at least %d countries, found %d", min_countries, nrow(countries)))
  }
  if (nrow(conditions) < min_conditions) {
    msgs <- c(msgs, sprintf("expected at least %d conditions, found %d", min_conditions, nrow(conditions)))
  }

  country_dir <- file.path(data_dir, "country")
  country_files <- list.files(country_dir, pattern = "^[0-9]+[.]json$", full.names = TRUE)
  if (length(country_files) < nrow(countries)) {
    msgs <- c(msgs, sprintf(
      "expected at least one per-country JSON for each country (%d), found %d",
      nrow(countries),
      length(country_files)
    ))
  }

  if (length(country_files) > 0) {
    sample_country <- jsonlite::fromJSON(country_files[[1]])
    missing_country_keys <- setdiff(
      c("location_id", "location_name", paste0("y", expected_years)),
      names(sample_country)
    )
    if (length(missing_country_keys) > 0) {
      msgs <- c(msgs, paste("sample country artifact missing keys:", paste(missing_country_keys, collapse = ", ")))
    }
  }

  if (length(msgs) > 0) {
    stop(paste("Site artifact contract failed:\n ", paste(msgs, collapse = "\n  ")), call. = FALSE)
  }

  invisible(TRUE)
}
