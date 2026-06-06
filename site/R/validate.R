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
