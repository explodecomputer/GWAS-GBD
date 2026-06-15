library(testthat)

test_that("EFO ids are formatted for ontologyIndex EFO namespace", {
  expect_equal(.format_efo_id("EFO:0000405"), "efo:EFO_0000405")
  expect_equal(.format_efo_id("http://www.ebi.ac.uk/efo/EFO_0000405"),
               "efo:EFO_0000405")
})

test_that("imported ontology ids keep OBO-style prefixes", {
  expect_equal(.format_efo_id("MONDO:0002356"), "MONDO:0002356")
  expect_equal(.format_efo_id("Orphanet:123"), "Orphanet:123")
  expect_equal(.format_efo_id("NCIT:C35730"), "NCIT:C35730")
})

test_that("ontologyIndex EFO ids clean back to GWAS-compatible ids", {
  expect_equal(.clean_uri(.revert_efo_id("efo:EFO_0000405")), "efo0000405")
  expect_equal(.clean_uri(.revert_efo_id("MONDO:0002356")), "mondo0002356")
})

test_that("obsolete OBO replacements are parsed from raw stanzas", {
  obo <- tempfile(fileext = ".obo")
  writeLines(c(
    "format-version: 1.2",
    "",
    "[Term]",
    "id: efo:EFO_0000405",
    "name: obsolete_digestive system disease",
    "is_obsolete: true",
    "replaced_by: http://purl.obolibrary.org/obo/MONDO_0004335",
    "",
    "[Term]",
    "id: efo:EFO_9999999",
    "name: obsolete_example",
    "is_obsolete: true",
    "consider: http://www.ebi.ac.uk/efo/EFO_0000001"
  ), obo)

  replacements <- .parse_obo_obsolete_replacements(obo)

  expect_true(any(
    replacements$source_id == "efo:EFO_0000405" &
      replacements$replacement_id == "MONDO:0004335" &
      replacements$replacement_type == "replaced_by"
  ))
  expect_true(any(
    replacements$source_id == "efo:EFO_9999999" &
      replacements$replacement_id == "efo:EFO_0000001" &
      replacements$replacement_type == "consider"
  ))
})

test_that("obsolete replacements are added to direct maps", {
  obo <- tempfile(fileext = ".obo")
  writeLines(c(
    "format-version: 1.2",
    "",
    "[Term]",
    "id: efo:EFO_0000389",
    "name: obsolete_cutaneous melanoma",
    "is_obsolete: true",
    "replaced_by: http://purl.obolibrary.org/obo/MONDO_0005012"
  ), obo)

  direct_map <- data.frame(
    `GBD term` = "Malignant skin melanoma",
    MAPPED_TRAIT_URI = .clean_uri("EFO:0000389"),
    check.names = FALSE
  )

  expanded <- .add_obsolete_replacement_mappings(
    direct_map, "MAPPED_TRAIT_URI", obo
  )

  expect_setequal(
    expanded$MAPPED_TRAIT_URI,
    c(.clean_uri("EFO:0000389"), .clean_uri("MONDO:0005012"))
  )
})
