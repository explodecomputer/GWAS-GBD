library(testthat)
library(dplyr)

test_that("fake embedding port returns deterministic embeddings", {
  port <- embedding_fake_port(dim = 6L)
  first <- embedding_port_embed(port, c("alpha beta", "gamma"))
  second <- embedding_port_embed(port, c("alpha beta", "gamma"))

  expect_equal(first, second)
  expect_equal(nrow(first), 2L)
  expect_equal(ncol(first), 6L)
})

test_that("embedding candidate generation works without GPU server", {
  existing <- empty_candidate_rows()
  gbd_context <- data.frame(
    condition_name = "Type 1 diabetes",
    alias = NA_character_,
    scope_note = NA_character_,
    stringsAsFactors = FALSE
  )
  observed <- list(terms = data.frame(
    ontology_id = c("EFO:0000001", "EFO:0000002"),
    stringsAsFactors = FALSE
  ))
  ontology_metadata <- data.frame(
    ontology_id = c("EFO:0000001", "EFO:0000002"),
    label = c("type 1 diabetes mellitus", "unrelated phenotype"),
    synonyms = c("juvenile diabetes", ""),
    definition = c("diabetes autoimmune", "other"),
    stringsAsFactors = FALSE
  )

  result <- add_embedding_candidates(
    existing_candidates = existing,
    gbd_context = gbd_context,
    observed_term_universe = observed,
    ontology_metadata = ontology_metadata,
    top_k = 1L,
    min_similarity = 0,
    embedding_port = embedding_fake_port(dim = 12L)
  )

  expect_equal(nrow(result), 1L)
  expect_equal(result$gbd_condition, "Type 1 diabetes")
  expect_true(result$ontology_id %in% observed$terms$ontology_id)
  expect_true(grepl("embedding", result$channels))
})
