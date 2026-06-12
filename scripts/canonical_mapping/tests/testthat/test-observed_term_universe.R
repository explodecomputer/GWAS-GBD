library(testthat)
library(dplyr)
library(data.table)

# ── Fixtures ───────────────────────────────────────────────────────────────

make_gwas_fixture <- function() {
  data.frame(
    PUBMEDID            = c("111", "111", "222", "333", "444"),
    DATE                = c("2020-01-01", "2020-01-01", "2021-06-01",
                            "2022-03-01", "2023-01-01"),
    MAPPED_TRAIT_URI    = c(
      "http://www.ebi.ac.uk/efo/EFO_0000001",
      "http://www.ebi.ac.uk/efo/EFO_0000001, http://www.ebi.ac.uk/efo/EFO_0000002",
      "http://purl.obolibrary.org/obo/MONDO_0005015",
      NA_character_,
      "  "
    ),
    `ASSOCIATION COUNT` = c("10", "20", "5", "30", "15"),
    `DISEASE/TRAIT`     = c("Type 1 diabetes", "Obesity", "Type 2 diabetes",
                            "Asthma", "Hypertension"),
    stringsAsFactors    = FALSE,
    check.names         = FALSE
  )
}

write_fixture_tsv <- function(df = make_gwas_fixture()) {
  p <- tempfile(fileext = ".tsv")
  data.table::fwrite(df, p, sep = "\t")
  p
}

# ── Tests ──────────────────────────────────────────────────────────────────

test_that("observed terms include all valid normalized IDs", {
  path <- write_fixture_tsv()
  result <- build_observed_term_universe(path, catalog_release = "test_2020")
  ids <- result$terms$ontology_id
  expect_true("EFO:0000001" %in% ids)
  expect_true("EFO:0000002" %in% ids)
  expect_true("MONDO:0005015" %in% ids)
})

test_that("blank and NA URIs are excluded and counted", {
  path <- write_fixture_tsv()
  result <- build_observed_term_universe(path)
  expect_equal(result$metadata$n_blank_uri_rows, 2L)
  ids <- result$terms$ontology_id
  expect_false(any(ids == ""))
  expect_false(any(is.na(ids)))
})

test_that("multi-term rows are expanded and counted", {
  path <- write_fixture_tsv()
  result <- build_observed_term_universe(path)
  # Row 2 has two terms separated by comma
  expect_true(result$metadata$n_multi_term_rows >= 1)
  expect_equal(result$metadata$n_observed_terms, 3L)
})

test_that("repeated PubMed IDs are counted per term correctly", {
  path <- write_fixture_tsv()
  result <- build_observed_term_universe(path)
  efo1 <- result$terms %>% filter(ontology_id == "EFO:0000001")
  # PUBMEDID 111 appears in both rows with EFO_0000001
  expect_equal(efo1$pubmed_count, 1L)
})

test_that("catalog_release label is preserved in output", {
  path <- write_fixture_tsv()
  result <- build_observed_term_universe(path, catalog_release = "2024Q1")
  expect_true(all(result$terms$catalog_release == "2024Q1"))
})

test_that("n_total_rows matches the fixture row count", {
  df <- make_gwas_fixture()
  path <- write_fixture_tsv(df)
  result <- build_observed_term_universe(path)
  expect_equal(result$metadata$n_total_rows, nrow(df))
})

test_that("malformed IDs without prefix separator are excluded", {
  df <- data.frame(
    PUBMEDID            = "999",
    DATE                = "2023-01-01",
    MAPPED_TRAIT_URI    = c("NOCOLON", "EFO:0000001"),
    `ASSOCIATION COUNT` = "5",
    `DISEASE/TRAIT`     = "Test trait",
    stringsAsFactors    = FALSE,
    check.names         = FALSE
  )
  path <- tempfile(fileext = ".tsv")
  data.table::fwrite(df, path, sep = "\t")
  result <- build_observed_term_universe(path)
  expect_false("NOCOLON" %in% result$terms$ontology_id)
  expect_equal(result$metadata$n_malformed_uri_rows, 1L)
})
