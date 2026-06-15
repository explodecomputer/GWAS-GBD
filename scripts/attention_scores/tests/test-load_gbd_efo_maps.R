library(testthat)
library(dplyr)

test_that("GBD-GWAS Catalog map splits, cleans, and excludes rows", {
  extra_map <- data.frame(
    Trait = c("Acute hepatitis B", "All causes", "Excluded disease"),
    `EFO term` = c("EFO_0004197, MONDO_0100370", "EFO_0000001", "EFO_9999999"),
    Exclude = c(NA, NA, "yes"),
    check.names = FALSE
  )

  parsed <- .parse_gbd_gwascat_map(extra_map, exclude_causes = "All causes")

  expect_equal(nrow(parsed), 2L)
  expect_equal(
    parsed %>% arrange(MAPPED_TRAIT_URI) %>% pull(MAPPED_TRAIT_URI),
    c("efo0004197", "mondo0100370")
  )
  expect_true(all(parsed$`GBD term` == "Acute hepatitis B"))
})

test_that("GBD-GWAS Catalog map requires expected columns", {
  expect_error(
    .parse_gbd_gwascat_map(data.frame(Trait = "A"), exclude_causes = character(0)),
    "missing required columns"
  )
})
