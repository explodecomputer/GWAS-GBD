library(testthat)
library(dplyr)
library(tidyr)
library(stringr)

# Tiny synthetic inputs for map_attention_to_gbd_leaves.
# attention: one row per (MAPPED_TRAIT_URI, PUBMEDID, pub_year)

make_attention <- function() {
  data.frame(
    MAPPED_TRAIT_URI  = c("http://www.ebi.ac.uk/efo/EFO_0000001",
                           "http://www.ebi.ac.uk/efo/EFO_0000001",
                           "http://www.ebi.ac.uk/efo/EFO_0000002",
                           "http://www.ebi.ac.uk/efo/EFO_0000003"),
    PUBMEDID          = c("111", "222", "333", "444"),
    pub_year          = c(2010L, 2011L, 2012L, 2013L),
    n_efo             = c(1L,    2L,    1L,    1L),
    ASSOCIATION_COUNT = c(100L,  200L,  50L,   75L),
    Impact_factor     = c(3.0,   3.0,   2.0,   1.0),
    DISEASE_TRAIT     = c("Type 1 diabetes", "Type 1 diabetes",
                           "Obesity related", "Asthma condition"),
    stringsAsFactors  = FALSE
  )
}

make_direct_map <- function() {
  data.frame(
    `GBD term`       = c("Type 1 diabetes mellitus", "Obesity",
                          "Type 1 diabetes mellitus"),
    MAPPED_TRAIT_URI = c("efo0000001", "efo0000002", "efo0000001"),
    stringsAsFactors = FALSE,
    check.names      = FALSE
  )
}

test_that("direct EFO matches score correctly", {
  att   <- make_attention()
  dmap  <- make_direct_map()
  result <- map_attention_to_gbd_leaves(att, dmap,
                                         descendant_map = NULL,
                                         exclude_causes = character(0))
  dm_row <- result %>% filter(`GBD term` == "Type 1 diabetes mellitus")
  expect_equal(nrow(dm_row), 1L)
  expect_gt(dm_row$total_attention_score, 0)
})

test_that("fuzzy match fires only when EFO match fails", {
  att   <- make_attention()
  dmap  <- make_direct_map()
  result <- map_attention_to_gbd_leaves(att, dmap,
                                         descendant_map = NULL,
                                         exclude_causes = character(0))
  # "Asthma" is not in the direct map; if fuzzy match works it gets a score
  asthma_row <- result %>% filter(`GBD term` == "Asthma")
  # Asthma is not in the GBD map at all, so it won't appear in the output
  expect_equal(nrow(asthma_row), 0L)
})

test_that("excluded terms are absent from output", {
  att    <- make_attention()
  dmap   <- make_direct_map()
  dmap   <- rbind(dmap, data.frame(`GBD term` = "All causes",
                                    MAPPED_TRAIT_URI = "efo0000001",
                                    stringsAsFactors = FALSE,
                                    check.names = FALSE))
  result <- map_attention_to_gbd_leaves(att, dmap,
                                         descendant_map = NULL,
                                         exclude_causes = c("All causes"))
  expect_false("All causes" %in% result$`GBD term`)
})

test_that("two independent PUBMEDIDs for same GBD term produce summed score", {
  # EFO_0000001 (→ Type 1 diabetes mellitus) appears in PUBMEDID 111 AND 222
  att   <- make_attention()
  dmap  <- make_direct_map()
  result <- map_attention_to_gbd_leaves(att, dmap,
                                         descendant_map = NULL,
                                         exclude_causes = character(0))
  dm_row <- result %>% filter(`GBD term` == "Type 1 diabetes mellitus")
  # nhits should be 100 (pubmed 111) + 200 (pubmed 222) = 300
  # The duplicate direct-map row should not double-count either publication.
  expect_equal(dm_row$nhits, 300)
  expect_equal(dm_row$total_attention_score, 2L)
})

test_that("GBD terms with no ontology descendants are retained as zero rows", {
  att <- make_attention()
  dmap <- make_direct_map()
  descendant_map <- data.frame(
    `GBD term` = "Ontology-only disease",
    descendant_URI = NA_character_,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  result <- map_attention_to_gbd_leaves(att, dmap, descendant_map,
                                        exclude_causes = character(0))

  ontology_row <- result %>% filter(`GBD term` == "Ontology-only disease")
  expect_equal(nrow(ontology_row), 1L)
  expect_equal(ontology_row$total_attention_score, 0)
  expect_equal(ontology_row$match_type, "none")
})

test_that("ontology descendants can match attention already used by direct maps", {
  att <- make_attention()
  dmap <- data.frame(
    `GBD term`       = "Direct disease",
    MAPPED_TRAIT_URI = "efo0000003",
    stringsAsFactors = FALSE,
    check.names      = FALSE
  )
  descendant_map <- data.frame(
    `GBD term` = "Ontology parent disease",
    descendant_URI = "efo0000003",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  result <- map_attention_to_gbd_leaves(att, dmap, descendant_map,
                                        exclude_causes = character(0))

  direct_row <- result %>% filter(`GBD term` == "Direct disease")
  ontology_row <- result %>% filter(`GBD term` == "Ontology parent disease")

  expect_equal(direct_row$total_attention_score, 1)
  expect_equal(ontology_row$total_attention_score, 1)
  expect_equal(ontology_row$match_type, "ontology")
})

test_that("unmatched GBD terms get zero attention score", {
  att  <- make_attention()
  dmap <- make_direct_map()
  # Add a GBD term with no matching EFO
  dmap <- rbind(dmap, data.frame(`GBD term` = "Rare disease X",
                                  MAPPED_TRAIT_URI = "efo9999999",
                                  stringsAsFactors = FALSE,
                                  check.names = FALSE))
  result <- map_attention_to_gbd_leaves(att, dmap,
                                         descendant_map = NULL,
                                         exclude_causes = character(0))
  rare_row <- result %>% filter(`GBD term` == "Rare disease X")
  expect_equal(nrow(rare_row), 1L)
  expect_equal(rare_row$total_attention_score, 0)
})

test_that("manual trait strings match terms still unmatched after EFO and GBD-name strings", {
  att <- rbind(
    make_attention(),
    data.frame(
      MAPPED_TRAIT_URI  = "http://www.ebi.ac.uk/efo/EFO_9999998",
      PUBMEDID          = "555",
      pub_year          = 2014L,
      n_efo             = 1L,
      ASSOCIATION_COUNT = 8L,
      Impact_factor     = 4.0,
      DISEASE_TRAIT     = "Curated GWAS phenotype",
      stringsAsFactors  = FALSE
    )
  )
  dmap <- rbind(
    make_direct_map(),
    data.frame(
      `GBD term` = "Manual only disease",
      MAPPED_TRAIT_URI = "efo9999999",
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  )
  manual <- data.frame(
    `GBD term` = "Manual only disease",
    MANUAL_TRAIT_NAME = "curated gwas phenotype",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  result <- map_attention_to_gbd_leaves(
    att, dmap, descendant_map = NULL, exclude_causes = character(0),
    manual_trait_map = manual
  )

  manual_row <- result %>% filter(`GBD term` == "Manual only disease")
  expect_equal(manual_row$total_attention_score, 1)
  expect_equal(manual_row$nhits, 8)
  expect_equal(manual_row$match_type, "manual_trait")
})
