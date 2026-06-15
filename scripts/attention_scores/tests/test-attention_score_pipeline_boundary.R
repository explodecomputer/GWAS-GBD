library(testthat)
library(dplyr)
library(data.table)

make_boundary_attention <- function() {
  data.frame(
    MAPPED_TRAIT_URI = c("EFO_0000001", "EFO_0000002", "EFO_0000001"),
    PUBMEDID = c("1", "2", "3"),
    pub_year = c(2020L, 2021L, 2022L),
    n_efo = c(1L, 1L, 1L),
    ASSOCIATION_COUNT = c(1L, 1L, 1L),
    Impact_factor = c(0, 0, 0),
    DISEASE_TRAIT = c("A trait", "B trait", "A trait"),
    NCASE = c(10, 5, 20),
    stringsAsFactors = FALSE
  )
}

make_boundary_master <- function() {
  data.frame(
    gbd_term = "Condition A",
    mapped_trait_uri = "EFO:0000001",
    mapping_strategy = "canonical",
    mapping_source = "canonical_human_review",
    include_in_pipeline = TRUE,
    hierarchy_distance = 0L,
    hierarchy_weight = 1,
    root_mapped_trait_uri = "EFO:0000001",
    lookup_trait_uri = NA_character_,
    replacement_type = "original",
    source_file = "fixture.tsv",
    source_column = "ontology_id",
    source_value = "EFO:0000001",
    search_term = "Condition A",
    matched_gwas_trait_examples = NA_character_,
    n_matching_pubmeds = NA_integer_,
    stringsAsFactors = FALSE
  ) %>%
    .standardise_master_mapping()
}

fake_rollup <- function(leaf_scores, hierarchy_path) {
  leaf_scores %>%
    transmute(
      GBD.term = `GBD term`,
      cause_id = if_else(`GBD term` == "Condition A", 1L, 2L),
      total_attention_score,
      weighted_nhits,
      weighted_attention_score_impact_factor,
      weighted_n,
      nhit = nhits
    )
}

fake_temporal <- function(attention, master_map, exclude_causes, hierarchy_path) {
  data.frame(
    cause_name = c("Condition A", "Condition A", "Condition A"),
    cause_id = c(1L, 1L, 1L),
    total_attention_score = c(10, 20, 30),
    analysis_type = c("year", "year", "sliding_3yr"),
    time_strata = c(2020L, 2022L, 2020L)
  )
}

test_that("manual zero-attention mappings are explicit material inputs", {
  manual_path <- tempfile(fileext = ".csv")
  fwrite(
    data.frame(
      cause_name = "Condition B",
      cause_id = 2L,
      mapped_trait_uri = "EFO_0000002",
      notes = "fixture"
    ),
    manual_path
  )

  manual_rows <- load_manual_zero_mapping(manual_path)
  master <- append_manual_zero_mappings(make_boundary_master(), manual_rows)

  expect_true(any(master$gbd_term == "Condition B"))
  expect_true(any(master$mapping_source == "manual_zero_attention_mapping"))
  expect_true(any(master$mapped_trait_uri == "efo0000002"))
})

test_that("attention pipeline boundary produces all-time, temporal, and schema outputs", {
  manual_path <- tempfile(fileext = ".csv")
  fwrite(
    data.frame(
      cause_name = "Condition B",
      cause_id = 2L,
      mapped_trait_uri = "EFO_0000002",
      notes = "fixture"
    ),
    manual_path
  )
  master <- append_manual_zero_mappings(
    make_boundary_master(),
    load_manual_zero_mapping(manual_path)
  )

  result <- compute_attention_score_outputs(
    attention = make_boundary_attention(),
    master_map = master,
    gbd_hierarchy_path = "unused.xlsx",
    gbd_universe = data.frame(
      cause_name = c("Condition A", "Condition B"),
      cause_id = c(1L, 2L)
    ),
    rollup_fn = fake_rollup,
    temporal_fn = fake_temporal
  )

  expect_setequal(result$combined$GBD.term, c("Condition A", "Condition B"))
  expect_true(all(c("all", "year", "sliding_3yr") %in% result$temporal$analysis_type))
  expect_true(all(c("cause_name", "cause_id", "total_attention_score", "analysis_type", "time_strata") %in%
                    names(result$temporal)))
  expect_true(result$combined$total_attention_score[result$combined$GBD.term == "Condition A"] > 0)
  expect_true(result$combined$total_attention_score[result$combined$GBD.term == "Condition B"] > 0)
})
