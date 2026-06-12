library(testthat)
library(dplyr)

make_evidence_row <- function() {
  data.frame(
    row_id              = "Type 1 diabetes||EFO:0000001",
    gbd_condition       = "Type 1 diabetes",
    gbd_level           = 3L,
    parent_name         = "Metabolic disorders",
    siblings            = "Type 2 diabetes | Obesity",
    n_siblings          = 2L,
    is_residual         = FALSE,
    alias               = NA_character_,
    scope_note          = NA_character_,
    ontology_id         = "EFO:0000001",
    label               = "type 1 diabetes mellitus",
    prefix              = "EFO",
    definition          = "An autoimmune form of diabetes.",
    synonyms            = "T1D | insulin-dependent diabetes",
    is_obsolete         = FALSE,
    catalog_release     = "2024Q1",
    pubmed_count        = 150L,
    association_count   = 2000L,
    example_trait_labels = "Type 1 diabetes | T1D",
    example_pubmed_ids  = "123 | 456",
    channels            = "lexical|gwas_trait",
    channel_details     = "search='Type 1 diabetes'",
    model_recommendation = NA_character_,
    model_relationship   = NA_character_,
    model_rationale      = NA_character_,
    model_confidence     = NA_real_,
    human_decision       = NA_character_,
    human_relationship   = NA_character_,
    human_notes          = NA_character_,
    reviewer_id          = NA_character_,
    review_date          = NA_character_,
    stringsAsFactors     = FALSE
  )
}

# ── Tests ──────────────────────────────────────────────────────────────────

test_that("prompt has system and user fields", {
  prompt <- format_llm_review_prompt(make_evidence_row())
  expect_true(all(c("system", "user") %in% names(prompt)))
})

test_that("system prompt contains accept/reject/unsure decision labels", {
  prompt <- format_llm_review_prompt(make_evidence_row())
  expect_true(grepl("accept", prompt$system, ignore.case = TRUE))
  expect_true(grepl("reject", prompt$system, ignore.case = TRUE))
  expect_true(grepl("unsure", prompt$system, ignore.case = TRUE))
})

test_that("user prompt contains GBD condition name and ontology ID", {
  prompt <- format_llm_review_prompt(make_evidence_row())
  expect_true(grepl("Type 1 diabetes", prompt$user))
  expect_true(grepl("EFO:0000001", prompt$user))
})

test_that("valid JSON response parses correctly", {
  response <- '{"recommendation": "accept", "relationship_label": "exact",
                "rationale": "The term exactly denotes T1D.", "confidence": 0.95}'
  result <- parse_llm_review_response(response)
  expect_equal(result$recommendation,     "accept")
  expect_equal(result$relationship_label, "exact")
  expect_true(grepl("T1D", result$rationale))
  expect_equal(result$confidence, 0.95)
  expect_true(is.na(result$parse_error))
})

test_that("JSON in markdown code fence is parsed", {
  response <- '```json\n{"recommendation": "reject", "relationship_label": "unrelated",\n"rationale": "Not related.", "confidence": 0.8}\n```'
  result <- parse_llm_review_response(response)
  expect_equal(result$recommendation, "reject")
})

test_that("empty response returns parse_error", {
  result <- parse_llm_review_response("")
  expect_true(!is.na(result$parse_error))
  expect_true(is.na(result$recommendation))
})

test_that("invalid JSON returns parse_error", {
  result <- parse_llm_review_response("{not valid json}")
  expect_true(!is.na(result$parse_error))
})

test_that("invalid recommendation value returns parse_error but does not crash", {
  response <- '{"recommendation": "maybe", "relationship_label": "exact",
                "rationale": "Test.", "confidence": 0.5}'
  result <- parse_llm_review_response(response)
  expect_true(!is.na(result$parse_error))
  expect_true(grepl("invalid_recommendation", result$parse_error))
})

test_that("apply_llm_recommendations fills model fields in evidence package", {
  pkg <- make_evidence_row()
  recommendations <- list(
    "Type 1 diabetes||EFO:0000001" = list(
      recommendation     = "accept",
      relationship_label = "exact",
      rationale          = "Exact match.",
      confidence         = 0.9,
      parse_error        = NA_character_
    )
  )
  updated <- apply_llm_recommendations(pkg, recommendations)
  expect_equal(updated$model_recommendation, "accept")
  expect_equal(updated$model_confidence, 0.9)
})
