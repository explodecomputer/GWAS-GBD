library(testthat)
library(dplyr)
library(data.table)
library(writexl)

make_catalog_tsv <- function(path = tempfile(fileext = ".tsv")) {
  df <- data.frame(
    PUBMEDID         = c("111", "111", "222", "333", "444"),
    DATE             = c("2020-01-01", "2020-01-01", "2021-01-01",
                         "2022-01-01", "2022-06-01"),
    MAPPED_TRAIT_URI = c("EFO:0000001", "EFO:0000002",
                         "EFO:0000001", "EFO:0000003", "EFO:0000001"),
    `ASSOCIATION COUNT` = c("10", "20", "15", "5", "8"),
    `DISEASE/TRAIT`  = c("T1D", "Obesity", "T1D", "T2D", "T1D"),
    stringsAsFactors = FALSE,
    check.names      = FALSE
  )
  data.table::fwrite(df, path, sep = "\t")
  path
}

make_canonical <- function() {
  data.frame(
    gbd_condition = c("Type 1 diabetes", "Obesity"),
    ontology_id   = c("EFO:0000001", "EFO:0000002"),
    stringsAsFactors = FALSE
  )
}

make_hierarchy_xlsx <- function(path = tempfile(fileext = ".xlsx")) {
  df <- data.frame(
    CauseId      = c("10", "11", "12"),
    CauseName    = c("Metabolic disorders", "Type 1 diabetes", "Obesity"),
    ParentName   = c("NCDs", "Metabolic disorders", "Metabolic disorders"),
    Level        = c("3", "4", "4"),
    CauseOutline = c("B1", "B1.1", "B1.2"),
    stringsAsFactors = FALSE
  )
  writexl::write_xlsx(list(`Cause Hierarchy` = df), path)
  path
}

# ── Tests ──────────────────────────────────────────────────────────────────

test_that("accepted mappings are counted once per (condition × ontology_id)", {
  path <- make_catalog_tsv()
  scores <- score_canonical_attention(make_canonical(), path)
  t1d <- scores %>% filter(gbd_condition == "Type 1 diabetes")
  # PUBMEDs 111(n_efo=2), 222(n_efo=1), 444(n_efo=1) each map EFO:0000001→T1D
  # 111: 1/2=0.5; 222: 1/1=1.0; 444: 1/1=1.0 → total 2.5
  expect_equal(t1d$total_attention_score, 2.5)
})

test_that("rejected mappings are not counted (not in canonical)", {
  path  <- make_catalog_tsv()
  # EFO:0000003 is in catalog (PUBMEDID 333) but not in canonical
  canonical <- make_canonical()  # Only EFO:0000001 and EFO:0000002
  scores <- score_canonical_attention(canonical, path)
  # No "Type 2 diabetes" row since EFO:0000003 is not mapped
  expect_false("Type 2 diabetes" %in% scores$gbd_condition)
})

test_that("zero-attention conditions are preserved", {
  path <- make_catalog_tsv()
  canonical_with_zero <- bind_rows(
    make_canonical(),
    data.frame(gbd_condition = "Rare disease", ontology_id = "ORPHANET:12345",
               stringsAsFactors = FALSE)
  )
  scores <- score_canonical_attention(canonical_with_zero, path)
  rare <- scores %>% filter(gbd_condition == "Rare disease")
  expect_equal(nrow(rare), 1L)
  expect_equal(rare$total_attention_score, 0)
  expect_true(rare$zero_attention)
})

test_that("parent rollup adds child scores to parent", {
  hier_path <- make_hierarchy_xlsx()
  leaf_scores <- data.frame(
    gbd_condition         = c("Type 1 diabetes", "Obesity"),
    total_attention_score = c(10, 5),
    zero_attention        = c(FALSE, FALSE),
    stringsAsFactors      = FALSE
  )
  rolled <- rollup_canonical_hierarchy(leaf_scores, hier_path)
  parent <- rolled %>% filter(cause_name == "Metabolic disorders")
  expect_equal(parent$total_attention_score, 15)
})

test_that("duplicate edges in canonical are deduplicated before scoring", {
  path <- make_catalog_tsv()
  dup_canonical <- data.frame(
    gbd_condition = c("Type 1 diabetes", "Type 1 diabetes"),
    ontology_id   = c("EFO:0000001", "EFO:0000001"),  # duplicate
    stringsAsFactors = FALSE
  )
  scores <- score_canonical_attention(dup_canonical, path)
  t1d <- scores %>% filter(gbd_condition == "Type 1 diabetes")
  # Should be same as without duplicate: 2.5 (deduped to one edge)
  expect_equal(t1d$total_attention_score, 2.5)
})

test_that("temporal scores produce year and sliding_3yr rows", {
  path    <- make_catalog_tsv()
  temporal <- build_canonical_temporal_scores(make_canonical(), path)
  expect_true("year"        %in% temporal$analysis_type)
  expect_true("sliding_3yr" %in% temporal$analysis_type)
})
