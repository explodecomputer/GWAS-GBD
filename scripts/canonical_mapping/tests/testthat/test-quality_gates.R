library(testthat)
library(dplyr)

make_obs_universe <- function(ids = c("EFO:0000001", "EFO:0000002")) {
  list(
    terms = data.frame(
      ontology_id = ids,
      stringsAsFactors = FALSE
    )
  )
}

make_gbd_context <- function() {
  data.frame(
    condition_name = c("Condition A", "Condition B", "Other A NEC"),
    gbd_level      = c(3L, 3L, 3L),
    parent_name    = c("Group X", "Group X", "Group X"),
    siblings       = c("Condition B|Other A NEC", "Condition A|Other A NEC",
                       "Condition A|Condition B"),
    n_siblings     = c(2L, 2L, 2L),
    is_residual    = c(FALSE, FALSE, TRUE),
    alias          = NA_character_,
    excluded_alias = NA_character_,
    scope_note     = NA_character_,
    stringsAsFactors = FALSE
  )
}

make_canonical <- function() {
  data.frame(
    gbd_condition = c("Condition A", "Condition B"),
    ontology_id   = c("EFO:0000001", "EFO:0000002"),
    stringsAsFactors = FALSE
  )
}

# ── Tests ──────────────────────────────────────────────────────────────────

test_that("all gates pass for valid canonical mapping", {
  result <- run_mapping_quality_gates(
    canonical              = make_canonical(),
    observed_term_universe = make_obs_universe(),
    gbd_context            = make_gbd_context()
  )
  expect_true(result$passed)
})

test_that("observed_only gate fails for unobserved accepted term", {
  bad <- data.frame(
    gbd_condition = c("Condition A", "Condition B"),
    ontology_id   = c("EFO:0000001", "EFO:9999999"),  # 9999999 not observed
    stringsAsFactors = FALSE
  )
  result <- run_mapping_quality_gates(
    canonical              = bad,
    observed_term_universe = make_obs_universe(),
    gbd_context            = make_gbd_context()
  )
  expect_false(result$passed)
  expect_equal(result$results$observed_only$gate, "observed_only")
})

test_that("no_duplicate_edges gate fails for duplicate decisions", {
  dup <- data.frame(
    gbd_condition = c("Condition A", "Condition A"),
    ontology_id   = c("EFO:0000001", "EFO:0000001"),
    stringsAsFactors = FALSE
  )
  result <- run_mapping_quality_gates(
    canonical              = dup,
    observed_term_universe = make_obs_universe(),
    gbd_context            = make_gbd_context()
  )
  expect_false(result$passed)
  expect_equal(result$results$no_duplicate_edges$gate, "no_duplicate_edges")
})

test_that("residual_exclusivity gate fails when residual overlaps specific sibling", {
  # Condition A accepted EFO:0000001; Other A NEC also accepts EFO:0000001
  bad <- data.frame(
    gbd_condition = c("Condition A", "Other A NEC"),
    ontology_id   = c("EFO:0000001", "EFO:0000001"),
    stringsAsFactors = FALSE
  )
  result <- run_mapping_quality_gates(
    canonical              = bad,
    observed_term_universe = make_obs_universe(),
    gbd_context            = make_gbd_context()
  )
  expect_false(result$passed)
  expect_equal(result$results$residual_exclusivity$gate, "residual_exclusivity")
})

test_that("sentinel_non_zero gate fails when sentinel has no mappings", {
  result <- run_mapping_quality_gates(
    canonical              = make_canonical(),
    observed_term_universe = make_obs_universe(),
    gbd_context            = make_gbd_context(),
    sentinel_conditions    = c("Condition A", "Missing Condition")
  )
  expect_false(result$passed)
  expect_equal(result$results$sentinel_non_zero$gate, "sentinel_non_zero")
  expect_true(grepl("Missing Condition", result$results$sentinel_non_zero$message))
})

test_that("specificity_conflicts produces a warning (not a FAIL) when present", {
  # Condition A and its child both accept same term (if parent also mapped)
  gbd_ctx <- data.frame(
    condition_name = c("Parent P", "Child P1"),
    gbd_level      = c(3L, 4L),
    parent_name    = c("Top", "Parent P"),
    siblings       = c("", ""),
    n_siblings     = c(0L, 0L),
    is_residual    = c(FALSE, FALSE),
    alias = NA_character_, excluded_alias = NA_character_, scope_note = NA_character_,
    stringsAsFactors = FALSE
  )
  dup_parent <- data.frame(
    gbd_condition = c("Parent P", "Child P1"),
    ontology_id   = c("EFO:0000001", "EFO:0000001"),
    stringsAsFactors = FALSE
  )
  result <- run_mapping_quality_gates(
    canonical              = dup_parent,
    observed_term_universe = make_obs_universe(),
    gbd_context            = gbd_ctx
  )
  expect_equal(result$results$specificity_conflicts$status, "WARN")
  # WARN does not cause overall failure
  expect_true(result$passed)
})

test_that("required_fields gate fails for missing columns", {
  bad <- data.frame(gbd_condition = "X", stringsAsFactors = FALSE)  # missing ontology_id
  result <- run_mapping_quality_gates(
    canonical              = bad,
    observed_term_universe = make_obs_universe(),
    gbd_context            = make_gbd_context()
  )
  expect_false(result$passed)
})

test_that("report is a non-empty character string", {
  result <- run_mapping_quality_gates(
    canonical              = make_canonical(),
    observed_term_universe = make_obs_universe(),
    gbd_context            = make_gbd_context()
  )
  expect_true(is.character(result$report))
  expect_true(nchar(result$report) > 0)
})
