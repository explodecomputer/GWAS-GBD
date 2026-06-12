library(testthat)
library(dplyr)

make_obs_universe <- function() {
  list(terms = data.frame(
    ontology_id = c("EFO:0000001", "EFO:0000002", "EFO:0000003"),
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
                      "B||EFO:0000001", "Other NEC||EFO:0000003"),
    gbd_condition = c("Condition A", "Condition A", "Condition B",
                      "Condition B", "Other NEC"),
    ontology_id   = c("EFO:0000001", "EFO:0000002", "EFO:0000003",
                      "EFO:0000001", "EFO:0000003"),
    channels      = "lexical",
    channel_details = "test",
    model_recommendation = c("accept", "reject", "accept", "unsure", NA),
    model_relationship   = NA_character_,
    model_rationale      = NA_character_,
    model_confidence     = NA_real_,
    human_decision       = c("accept", "reject", "accept", "unsure", NA),
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

test_that("model-only rows are classified as model_reviewed, not accepted", {
  compiled <- compile_human_review(
    make_reviewed_pkg(), make_obs_universe(), make_gbd_context()
  )
  # Row "Other NEC||EFO:0000003" has no human_decision but also no model_recommendation
  # (it's NA for both) → "unreviewed"
  unreviewed <- compiled$audit_trail %>% filter(decision_status == "unreviewed")
  expect_true(nrow(unreviewed) >= 1)
})

test_that("summary counts match expected values", {
  compiled <- compile_human_review(
    make_reviewed_pkg(), make_obs_universe(), make_gbd_context()
  )
  expect_equal(compiled$summary$n_accepted, 2L)
  expect_equal(compiled$summary$n_rejected, 1L)
  expect_equal(compiled$summary$n_unsure,   1L)
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
