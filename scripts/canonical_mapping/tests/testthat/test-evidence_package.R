library(testthat)
library(dplyr)

make_candidates <- function() {
  data.frame(
    gbd_condition   = c("Type 1 diabetes", "Type 1 diabetes", "Obesity"),
    ontology_id     = c("EFO:0000001", "EFO:0000003", "EFO:0000002"),
    channels        = c("lexical|gwas_trait", "legacy_first_part", "lexical"),
    channel_details = c("search='Type 1 diabetes'", "col=EFO 1", "search='Obesity'"),
    stringsAsFactors = FALSE
  )
}

make_obs_universe <- function() {
  list(
    terms = data.frame(
      catalog_release      = "test",
      ontology_id          = c("EFO:0000001", "EFO:0000002", "EFO:0000003"),
      n_uri_rows           = c(5L, 3L, 2L),
      pubmed_count         = c(10L, 5L, 2L),
      association_count    = c(100L, 50L, 20L),
      example_trait_labels = c("T1D", "BMI", "T1D variant"),
      example_pubmed_ids   = c("111|222", "333", "444"),
      example_uris         = c("EFO_0000001", "EFO_0000002", "EFO_0000003"),
      stringsAsFactors     = FALSE
    )
  )
}

make_gbd_context <- function() {
  data.frame(
    condition_name = c("Type 1 diabetes", "Obesity"),
    cause_id       = c("1", "2"),
    gbd_level      = c(3L, 3L),
    parent_name    = c("Metabolic", "Metabolic"),
    siblings       = c("Obesity", "Type 1 diabetes"),
    n_siblings     = c(1L, 1L),
    is_residual    = c(FALSE, FALSE),
    alias          = c(NA_character_, NA_character_),
    excluded_alias = c(NA_character_, NA_character_),
    scope_note     = c(NA_character_, NA_character_),
    stringsAsFactors = FALSE
  )
}

make_ontology_metadata <- function() {
  data.frame(
    ontology_id = c("EFO:0000001", "EFO:0000002", "EFO:0000003"),
    label       = c("type 1 diabetes", "obesity", "type 1 DM variant"),
    is_obsolete = c(FALSE, FALSE, TRUE),
    stringsAsFactors = FALSE
  )
}

# ── Tests ──────────────────────────────────────────────────────────────────

test_that("one row per candidate decision", {
  pkg <- build_evidence_package(
    make_candidates(), make_obs_universe(), make_gbd_context()
  )
  expect_equal(nrow(pkg), nrow(make_candidates()))
})

test_that("required columns are present", {
  pkg <- build_evidence_package(
    make_candidates(), make_obs_universe(), make_gbd_context()
  )
  required <- c("row_id", "gbd_condition", "ontology_id",
                "channels", "human_decision", "model_recommendation")
  expect_true(all(required %in% names(pkg)))
})

test_that("row_id is unique per row", {
  pkg <- build_evidence_package(
    make_candidates(), make_obs_universe(), make_gbd_context()
  )
  expect_equal(length(unique(pkg$row_id)), nrow(pkg))
})

test_that("human and model review fields are NA by default", {
  pkg <- build_evidence_package(
    make_candidates(), make_obs_universe(), make_gbd_context()
  )
  review_fields <- c("human_decision", "human_relationship", "human_notes",
                     "model_recommendation", "model_relationship",
                     "model_rationale", "model_confidence")
  for (f in review_fields) {
    expect_true(all(is.na(pkg[[f]])), info = paste(f, "should be NA"))
  }
})

test_that("GWAS evidence columns are joined from observed universe", {
  pkg <- build_evidence_package(
    make_candidates(), make_obs_universe(), make_gbd_context()
  )
  t1d <- pkg %>% filter(gbd_condition == "Type 1 diabetes",
                        ontology_id == "EFO:0000001")
  expect_equal(t1d$pubmed_count, 10L)
  expect_equal(t1d$association_count, 100L)
})

test_that("ontology metadata is joined when provided", {
  pkg <- build_evidence_package(
    make_candidates(), make_obs_universe(), make_gbd_context(),
    ontology_metadata = make_ontology_metadata()
  )
  expect_true("label" %in% names(pkg))
  t1d <- pkg %>% filter(ontology_id == "EFO:0000001")
  expect_equal(t1d$label, "type 1 diabetes")
})

test_that("grouped review by condition preserves all rows", {
  pkg <- build_evidence_package(
    make_candidates(), make_obs_universe(), make_gbd_context()
  )
  t1d_rows <- filter_by_condition(pkg, "Type 1 diabetes")
  expect_equal(nrow(t1d_rows), 2L)
})
