library(testthat)
library(dplyr)

make_obs_universe <- function() {
  list(terms = data.frame(
    ontology_id = c("EFO:0000001", "EFO:0000002", "EFO:0000003", "EFO:0000004",
                    "EFO:0000005"),
    stringsAsFactors = FALSE
  ))
}

make_gbd_context <- function() {
  data.frame(
    condition_name = c("Condition A", "Condition B", "Other NEC"),
    gbd_level      = c(3L, 3L, 3L),
    parent_name    = c("Group X", "Group X", "Group X"),
    siblings       = c("Condition B|Other NEC", "Condition A|Other NEC",
                       "Condition A|Condition B"),
    n_siblings     = c(2L, 2L, 2L),
    is_residual    = c(FALSE, FALSE, TRUE),
    alias          = NA_character_,
    excluded_alias = NA_character_,
    scope_note     = NA_character_,
    stringsAsFactors = FALSE
  )
}

make_reviewed_pkg <- function() {
  data.frame(
    row_id        = c("A||EFO:0000001", "A||EFO:0000002", "B||EFO:0000003",
                      "B||EFO:0000001", "Other NEC||EFO:0000003",
                      "B||EFO:0000004", "B||EFO:0000005"),
    gbd_condition = c("Condition A", "Condition A", "Condition B",
                      "Condition B", "Other NEC", "Condition B", "Condition B"),
    ontology_id   = c("EFO:0000001", "EFO:0000002", "EFO:0000003",
                      "EFO:0000001", "EFO:0000003", "EFO:0000004", "EFO:0000005"),
    channels      = "lexical",
    channel_details = "test",
    # Row 6: model=accept, no human  → model_accepted (enters canonical)
    # Row 7: model=reject, no human  → model_reviewed (excluded from canonical)
    model_recommendation = c("accept", "reject", "accept", "unsure", NA, "accept", "reject"),
    model_relationship   = NA_character_,
    model_rationale      = NA_character_,
    model_confidence     = NA_real_,
    human_decision       = c("accept", "reject", "accept", "unsure", NA, NA, NA),
    human_relationship   = NA_character_,
    human_notes          = NA_character_,
    reviewer_id          = NA_character_,
    review_date          = NA_character_,
    stringsAsFactors     = FALSE
  )
}

# ── Tests ──────────────────────────────────────────────────────────────────

test_that("only accepted rows enter the canonical mapping", {
  compiled <- compile_human_review(
    make_reviewed_pkg(), make_obs_universe(), make_gbd_context()
  )
  expect_true(all(c("Condition A", "Condition B") %in%
                    compiled$canonical$gbd_condition))
  # Rejected row (Condition A × EFO:0000002) must not be in canonical
  rej <- compiled$canonical %>%
    filter(gbd_condition == "Condition A", ontology_id == "EFO:0000002")
  expect_equal(nrow(rej), 0L)
})

test_that("rejected and unsure rows are preserved in audit_trail", {
  compiled <- compile_human_review(
    make_reviewed_pkg(), make_obs_universe(), make_gbd_context()
  )
  statuses <- compiled$audit_trail$decision_status
  expect_true("rejected" %in% statuses)
  expect_true("unsure"   %in% statuses)
})

test_that("model-accepted rows enter canonical; model-rejected without human decision do not", {
  compiled <- compile_human_review(
    make_reviewed_pkg(), make_obs_universe(), make_gbd_context()
  )
  # Row 6 (model=accept, no human) → model_accepted → in canonical
  expect_true(any(compiled$canonical$ontology_id == "EFO:0000004"))
  expect_true(any(compiled$audit_trail$decision_status == "model_accepted"))
  # Row 7 (model=reject, no human) → model_reviewed → not in canonical
  model_reviewed <- compiled$audit_trail %>% filter(decision_status == "model_reviewed")
  expect_equal(nrow(model_reviewed), 1L)
  expect_false(any(compiled$canonical$ontology_id == "EFO:0000005"))
})

test_that("summary counts match expected values", {
  compiled <- compile_human_review(
    make_reviewed_pkg(), make_obs_universe(), make_gbd_context()
  )
  expect_equal(compiled$summary$n_accepted,       2L)  # human accept
  expect_equal(compiled$summary$n_model_accepted, 1L)  # model accept fallback
  expect_equal(compiled$summary$n_canonical_total, 3L) # human + model accepted
  expect_equal(compiled$summary$n_rejected,        1L)
  expect_equal(compiled$summary$n_unsure,          1L)
  expect_equal(compiled$summary$n_model_reviewed,  1L) # model reject, no human
})

test_that("canonical_mapping_for_scoring errors when gates fail", {
  # Create a mapping that fails gates (Condition B accepts same term as Other NEC)
  # The fixture already has this setup
  compiled <- compile_human_review(
    make_reviewed_pkg(), make_obs_universe(), make_gbd_context()
  )
  # Check residual exclusivity: Condition B accepted EFO:0000003,
  # Other NEC also has it... but Other NEC is unreviewed so not in canonical
  # Therefore gates should pass in this fixture
  if (compiled$summary$gates_passed) {
    expect_no_error(canonical_mapping_for_scoring(compiled))
  } else {
    expect_error(canonical_mapping_for_scoring(compiled))
  }
})

test_that("compile_human_review errors if human_decision column missing", {
  pkg_no_decision <- make_reviewed_pkg() %>% select(-human_decision)
  expect_error(
    compile_human_review(pkg_no_decision, make_obs_universe(), make_gbd_context()),
    "human_decision"
  )
})

test_that("duplicate accepted decisions fail at the compiler boundary", {
  pkg <- bind_rows(
    make_reviewed_pkg(),
    make_reviewed_pkg() %>%
      filter(gbd_condition == "Condition A", ontology_id == "EFO:0000001") %>%
      mutate(row_id = "duplicate")
  )

  compiled <- compile_human_review(pkg, make_obs_universe(), make_gbd_context())
  expect_false(compiled$summary$gates_passed)
  expect_equal(compiled$gate_result$results$no_duplicate_edges$status, "FAIL")
  expect_error(canonical_mapping_for_scoring(compiled), "quality gates")
})

test_that("residual overlap is a hard compiler failure", {
  pkg <- make_reviewed_pkg()
  pkg$human_decision[pkg$gbd_condition == "Other NEC" & pkg$ontology_id == "EFO:0000003"] <- "accept"

  compiled <- compile_human_review(pkg, make_obs_universe(), make_gbd_context())
  expect_false(compiled$summary$gates_passed)
  expect_equal(compiled$gate_result$results$residual_exclusivity$status, "FAIL")
})

test_that("canonical export table has scoring-compatible schema", {
  compiled <- compile_human_review(
    make_reviewed_pkg(), make_obs_universe(), make_gbd_context()
  )
  exported <- canonical_export_table(compiled, evidence_source = "fixture.csv")

  expect_equal(
    names(exported),
    c(
      "gbd_term",
      "mapped_trait_uri",
      "mapping_strategy",
      "mapping_source",
      "include_in_pipeline",
      "hierarchy_distance",
      "hierarchy_weight",
      "root_mapped_trait_uri",
      "lookup_trait_uri",
      "replacement_type",
      "source_file",
      "source_column",
      "source_value",
      "search_term",
      "matched_gwas_trait_examples",
      "n_matching_pubmeds"
    )
  )
  # Human-accepted rows carry "canonical_human_review"; model-accepted fallback rows carry
  # "canonical_model_review"
  expect_true("canonical_human_review" %in% exported$mapping_source)
  expect_true("canonical_model_review" %in% exported$mapping_source)
  # Model-accepted row (EFO:0000004) is now included; model-reviewed row (EFO:0000005) is not
  expect_true("EFO:0000004" %in% exported$mapped_trait_uri)
  expect_false("EFO:0000005" %in% exported$mapped_trait_uri)
})
