library(testthat)
library(jsonlite)

write_json_file <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(x, path, auto_unbox = TRUE, pretty = TRUE)
}

make_site_artifact_fixture <- function() {
  root <- tempfile("site-artifacts-")
  dir.create(file.path(root, "country"), recursive = TRUE)

  countries <- data.frame(location_id = 1:2, location_name = c("A", "B"))
  conditions <- data.frame(
    cause_id = 1:2,
    cause_name = c("Condition A", "Condition B"),
    attention_score = c(1, 0)
  )
  summaries <- data.frame(
    location_id = c(1, 1, 2, 2),
    location_name = c("A", "A", "B", "B"),
    year = c(1990, 2023, 1990, 2023)
  )
  opportunities <- data.frame(
    location_id = 1,
    cause_id = 2,
    year = 2023,
    mismatch_share = 0.1
  )
  metadata <- list(
    build_time = "2026-06-15T00:00:00Z",
    years = c(1990, 2023),
    n_countries = 2,
    n_conditions = 2,
    n_eligible_opps = 1
  )

  write_json_file(metadata, file.path(root, "metadata.json"))
  write_json_file(countries, file.path(root, "countries.json"))
  write_json_file(conditions, file.path(root, "conditions.json"))
  write_json_file(summaries, file.path(root, "country_summaries.json"))
  write_json_file(opportunities, file.path(root, "opportunities.json"))
  write_json_file(
    list(
      location_id = 1,
      location_name = "A",
      y1990 = list(list(cause_id = 1, cause_name = "Condition A")),
      y2023 = list(list(cause_id = 1, cause_name = "Condition A"))
    ),
    file.path(root, "country", "1.json")
  )
  write_json_file(
    list(
      location_id = 2,
      location_name = "B",
      y1990 = list(list(cause_id = 1, cause_name = "Condition A")),
      y2023 = list(list(cause_id = 1, cause_name = "Condition A"))
    ),
    file.path(root, "country", "2.json")
  )

  root
}

test_that("site artifact contract validates required JSON shape", {
  root <- make_site_artifact_fixture()
  expect_true(validate_site_artifact_files(
    data_dir = root,
    expected_years = c(1990L, 2023L),
    min_countries = 2L,
    min_conditions = 2L
  ))
})

test_that("site artifact contract fails when metadata years are missing", {
  root <- make_site_artifact_fixture()
  write_json_file(
    list(
      build_time = "2026-06-15T00:00:00Z",
      years = c(2023),
      n_countries = 2,
      n_conditions = 2,
      n_eligible_opps = 1
    ),
    file.path(root, "metadata.json")
  )

  expect_error(
    validate_site_artifact_files(
      data_dir = root,
      expected_years = c(1990L, 2023L),
      min_countries = 2L,
      min_conditions = 2L
    ),
    "metadata years"
  )
})
