library(testthat)
library(dplyr)

# ── Fixtures ───────────────────────────────────────────────────────────────

make_obs_universe <- function(ids = c("EFO:0000001", "EFO:0000002", "MONDO:0005015")) {
  list(
    terms = data.frame(
      catalog_release = "test",
      ontology_id     = ids,
      n_uri_rows      = 5L,
      pubmed_count    = 3L,
      association_count = 10L,
      example_trait_labels = "Example trait",
      example_pubmed_ids   = "111",
      example_uris         = paste(ids, collapse = " | "),
      stringsAsFactors = FALSE
    ),
    metadata = list(n_observed_terms = length(ids))
  )
}

make_gbd_context <- function() {
  data.frame(
    condition_name = c("Type 1 diabetes", "Type 2 diabetes",
                       "Obesity", "Other metabolic NEC"),
    cause_id       = c("1", "2", "3", "4"),
    gbd_level      = c(3L, 3L, 3L, 3L),
    parent_name    = c("Metabolic disorders", "Metabolic disorders",
                       "Metabolic disorders", "Metabolic disorders"),
    siblings       = c("Type 2 diabetes | Obesity | Other metabolic NEC",
                       "Type 1 diabetes | Obesity | Other metabolic NEC",
                       "Type 1 diabetes | Type 2 diabetes | Other metabolic NEC",
                       "Type 1 diabetes | Type 2 diabetes | Obesity"),
    n_siblings     = c(3L, 3L, 3L, 3L),
    is_residual    = c(FALSE, FALSE, FALSE, TRUE),
    alias          = c(NA_character_, NA_character_, NA_character_, NA_character_),
    excluded_alias = c(NA_character_, NA_character_, NA_character_, NA_character_),
    scope_note     = c(NA_character_, NA_character_, NA_character_, NA_character_),
    stringsAsFactors = FALSE
  )
}

make_ontology_labels <- function() {
  data.frame(
    ontology_id = c("EFO:0000001", "EFO:0000002", "MONDO:0005015"),
    label       = c("type 1 diabetes mellitus", "obesity",
                    "type 2 diabetes mellitus"),
    stringsAsFactors = FALSE
  )
}

make_gwas_raw <- function() {
  data.frame(
    PUBMEDID         = c("111", "222"),
    MAPPED_TRAIT_URI = c("EFO:0000001", "EFO:0000002"),
    `DISEASE/TRAIT`  = c("Type 1 diabetes", "Obesity"),
    check.names      = FALSE,
    stringsAsFactors = FALSE
  )
}

# ── Tests ──────────────────────────────────────────────────────────────────

test_that("lexical channel finds GBD name substring in ontology label", {
  obs   <- make_obs_universe()
  ctx   <- make_gbd_context()
  lbls  <- make_ontology_labels()

  result <- generate_deterministic_candidates(
    observed_term_universe = obs,
    gbd_context            = ctx,
    ontology_labels        = lbls
  )

  # "Type 1 diabetes" should match "type 1 diabetes mellitus" (EFO:0000001)
  t1d <- result %>%
    filter(gbd_condition == "Type 1 diabetes", ontology_id == "EFO:0000001")
  expect_true(nrow(t1d) == 1)
  expect_true(grepl("lexical", t1d$channels))
})

test_that("gwas_trait channel finds conditions via DISEASE/TRAIT string", {
  obs  <- make_obs_universe()
  ctx  <- make_gbd_context()
  gwas <- make_gwas_raw()

  result <- generate_deterministic_candidates(
    observed_term_universe = obs,
    gbd_context            = ctx,
    gwas_catalog_raw       = gwas
  )

  # "Obesity" in GWAS trait should propose EFO:0000002 for Obesity condition
  obes <- result %>%
    filter(gbd_condition == "Obesity", ontology_id == "EFO:0000002")
  expect_true(nrow(obes) >= 1)
  expect_true(grepl("gwas_trait", obes$channels))
})

test_that("provenance is preserved when multiple channels find same candidate", {
  obs  <- make_obs_universe()
  ctx  <- make_gbd_context()
  lbls <- make_ontology_labels()
  gwas <- make_gwas_raw()

  result <- generate_deterministic_candidates(
    observed_term_universe = obs,
    gbd_context            = ctx,
    ontology_labels        = lbls,
    gwas_catalog_raw       = gwas
  )

  # "Type 1 diabetes" × "EFO:0000001" via both lexical and gwas_trait
  t1d <- result %>%
    filter(gbd_condition == "Type 1 diabetes", ontology_id == "EFO:0000001")
  expect_true(grepl("lexical", t1d$channels))
  expect_true(grepl("gwas_trait", t1d$channels))
})

test_that("output contains only observed term IDs", {
  obs  <- make_obs_universe(ids = c("EFO:0000001"))
  ctx  <- make_gbd_context()
  lbls <- data.frame(
    ontology_id = c("EFO:0000001", "EFO:9999999"),
    label       = c("type 1 diabetes mellitus", "unrelated term"),
    stringsAsFactors = FALSE
  )

  result <- generate_deterministic_candidates(
    observed_term_universe = obs,
    gbd_context            = ctx,
    ontology_labels        = lbls
  )

  expect_true(all(result$ontology_id %in% obs$terms$ontology_id))
  expect_false("EFO:9999999" %in% result$ontology_id)
})

test_that("one row per (gbd_condition × ontology_id) in output", {
  obs  <- make_obs_universe()
  ctx  <- make_gbd_context()
  lbls <- make_ontology_labels()
  gwas <- make_gwas_raw()

  result <- generate_deterministic_candidates(
    observed_term_universe = obs,
    gbd_context            = ctx,
    ontology_labels        = lbls,
    gwas_catalog_raw       = gwas
  )

  deduped <- result %>% distinct(gbd_condition, ontology_id)
  expect_equal(nrow(result), nrow(deduped))
})

test_that("legacy_first_part channel proposes only observed terms", {
  obs <- make_obs_universe(ids = c("EFO:0000001"))

  # Create temp First_part with a non-observed and an observed EFO
  first_part_df <- data.frame(
    `GBD term` = c("Type 1 diabetes", "Some other condition"),
    `EFO 1`    = c("EFO_0000001", "EFO_9999"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  first_path <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(first_part_df, first_path)

  ctx <- make_gbd_context()

  result <- generate_deterministic_candidates(
    observed_term_universe = obs,
    gbd_context            = ctx,
    first_part_path        = first_path,
    exclude_causes         = character(0)
  )

  expect_true("EFO:0000001" %in% result$ontology_id)
  expect_false("EFO:9999" %in% result$ontology_id)
  expect_true(any(grepl("legacy_first_part", result$channels)))
})
