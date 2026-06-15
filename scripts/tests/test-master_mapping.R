library(testthat)
library(dplyr)

make_master_attention <- function() {
  data.frame(
    MAPPED_TRAIT_URI = "http://www.ebi.ac.uk/efo/EFO_0000001",
    PUBMEDID = "111",
    pub_year = 2020L,
    n_efo = 1L,
    ASSOCIATION_COUNT = 10L,
    Impact_factor = 0,
    DISEASE_TRAIT = "Example trait",
    stringsAsFactors = FALSE
  )
}

test_that("master map duplicate source rows do not double-count the same edge", {
  master <- data.frame(
    gbd_term = c("Example disease", "Example disease"),
    mapped_trait_uri = c("efo0000001", "efo0000001"),
    mapping_strategy = c("direct", "direct"),
    mapping_source = c("first_part", "gbd_gwascat"),
    include_in_pipeline = c(TRUE, TRUE),
    hierarchy_distance = c(0L, 0L),
    hierarchy_weight = c(1, 1),
    stringsAsFactors = FALSE
  )

  result <- map_attention_to_gbd_leaves_from_master(
    make_master_attention(),
    master,
    exclude_causes = character(0)
  )

  example <- result %>% filter(`GBD term` == "Example disease")
  expect_equal(example$total_attention_score, 1)
  expect_equal(example$nhits, 10)
})

test_that("master map hierarchy weights scale contributions", {
  master <- data.frame(
    gbd_term = "Hierarchy disease",
    mapped_trait_uri = "efo0000001",
    mapping_strategy = "hierarchy_descendant",
    mapping_source = "second_part",
    include_in_pipeline = TRUE,
    hierarchy_distance = 1L,
    hierarchy_weight = 0.1,
    stringsAsFactors = FALSE
  )

  result <- map_attention_to_gbd_leaves_from_master(
    make_master_attention(),
    master,
    exclude_causes = character(0)
  )

  hierarchy <- result %>% filter(`GBD term` == "Hierarchy disease")
  expect_equal(hierarchy$total_attention_score, 0.1)
  expect_equal(hierarchy$nhits, 1)
})

test_that("master map source roots with no GWAS match are retained as zero rows", {
  master <- data.frame(
    gbd_term = "Unresolved ontology disease",
    mapped_trait_uri = "ncitc123",
    mapping_strategy = "hierarchy_root_source",
    mapping_source = "second_part",
    include_in_pipeline = TRUE,
    hierarchy_distance = 0L,
    hierarchy_weight = 1,
    stringsAsFactors = FALSE
  )

  result <- map_attention_to_gbd_leaves_from_master(
    make_master_attention(),
    master,
    exclude_causes = character(0)
  )

  unresolved <- result %>% filter(`GBD term` == "Unresolved ontology disease")
  expect_equal(unresolved$total_attention_score, 0)
  expect_equal(unresolved$match_type, "none")
})
