library(dplyr)
library(tidyr)
library(stringr)

# ── Issue 012: Observed Catalog Term Universe ──────────────────────────────
# Builds the release-specific set of ontology terms observed in a GWAS Catalog
# release. Only terms present here may be accepted into a canonical mapping.

.normalise_ontology_id <- function(uri) {
  uri <- trimws(as.character(uri))
  uri <- gsub("^https?://[^/]+/", "", uri)
  uri <- gsub("^(efo|obo)/", "", uri, ignore.case = TRUE)
  uri <- gsub("_", ":", uri)
  uri <- toupper(uri)
  uri
}

.is_blank_uri <- function(uri) {
  is.na(uri) | trimws(as.character(uri)) == "" |
    tolower(trimws(as.character(uri))) %in% c("na", "null", "none", "n/a")
}

.load_catalog_raw <- function(gwas_catalog_path) {
  ext <- tolower(tools::file_ext(gwas_catalog_path))
  switch(
    ext,
    xlsx = readxl::read_xlsx(gwas_catalog_path, col_types = "text"),
    xls  = readxl::read_xlsx(gwas_catalog_path, col_types = "text"),
    tsv  = data.table::fread(gwas_catalog_path, sep = "\t", quote = "",
                             data.table = FALSE, colClasses = "character"),
    txt  = data.table::fread(gwas_catalog_path, sep = "\t", quote = "",
                             data.table = FALSE, colClasses = "character"),
    csv  = data.table::fread(gwas_catalog_path, data.table = FALSE,
                             colClasses = "character"),
    stop("Unsupported file extension: ", ext)
  )
}

#' Build the observed ontology term universe for one GWAS Catalog release.
#'
#' @param gwas_catalog_path Path to the GWAS Catalog file.
#' @param catalog_release Optional release name/date string for labelling.
#' @return A list with two elements:
#'   - `terms`: data frame with one row per observed normalized ontology ID
#'   - `metadata`: named list with catalog_release, n_total_rows,
#'     n_multi_term_rows, n_blank_uri_rows, n_malformed_uri_rows
build_observed_term_universe <- function(gwas_catalog_path,
                                         catalog_release = NULL) {
  raw <- .load_catalog_raw(gwas_catalog_path)

  required <- "MAPPED_TRAIT_URI"
  if (!required %in% names(raw)) {
    stop("GWAS Catalog file missing required column: ", required)
  }

  n_total_rows <- nrow(raw)

  # Detect and report blank URIs before expansion
  n_blank_uri_rows <- sum(.is_blank_uri(raw$MAPPED_TRAIT_URI))

  # Expand multi-term rows (comma-separated URIs)
  expanded <- raw %>%
    tidyr::separate_rows(MAPPED_TRAIT_URI, sep = ",\\s*") %>%
    mutate(MAPPED_TRAIT_URI = trimws(MAPPED_TRAIT_URI))

  n_multi_term_rows <- nrow(expanded) - n_total_rows

  # Remove blank/NA URIs
  expanded <- expanded %>%
    filter(!.is_blank_uri(MAPPED_TRAIT_URI))

  # Normalise URIs
  expanded <- expanded %>%
    mutate(ontology_id = .normalise_ontology_id(MAPPED_TRAIT_URI))

  # Detect malformed IDs (no colon separator after normalisation)
  malformed <- expanded %>% filter(!grepl(":", ontology_id, fixed = TRUE))
  n_malformed_uri_rows <- nrow(malformed)
  expanded <- expanded %>% filter(grepl(":", ontology_id, fixed = TRUE))

  # Determine available columns for evidence extraction
  pubmed_col <- grep("^PUBMEDID$", names(expanded), ignore.case = TRUE, value = TRUE)[1]
  assoc_col  <- grep("^ASSOCIATION.COUNT$", names(expanded), ignore.case = TRUE,
                     value = TRUE)[1]
  trait_col  <- grep("^DISEASE.?TRAIT$", names(expanded), ignore.case = TRUE,
                     value = TRUE)[1]

  # Build per-term evidence
  evidence <- expanded %>%
    group_by(ontology_id) %>%
    summarise(
      n_uri_rows = n(),
      pubmed_count = if (!is.na(pubmed_col))
        n_distinct(.data[[pubmed_col]], na.rm = TRUE) else NA_integer_,
      association_count = if (!is.na(assoc_col))
        sum(suppressWarnings(as.integer(.data[[assoc_col]])), na.rm = TRUE)
      else NA_integer_,
      example_trait_labels = if (!is.na(trait_col))
        paste(head(unique(na.omit(.data[[trait_col]])), 5), collapse = " | ")
      else NA_character_,
      example_pubmed_ids = if (!is.na(pubmed_col))
        paste(head(unique(na.omit(.data[[pubmed_col]])), 5), collapse = " | ")
      else NA_character_,
      example_uris = paste(head(unique(MAPPED_TRAIT_URI), 3), collapse = " | "),
      .groups = "drop"
    ) %>%
    mutate(
      catalog_release = if (is.null(catalog_release)) NA_character_
      else as.character(catalog_release)
    ) %>%
    select(catalog_release, ontology_id, everything())

  list(
    terms = evidence,
    metadata = list(
      catalog_release       = catalog_release,
      n_total_rows          = n_total_rows,
      n_multi_term_rows     = n_multi_term_rows,
      n_blank_uri_rows      = n_blank_uri_rows,
      n_malformed_uri_rows  = n_malformed_uri_rows,
      n_observed_terms      = nrow(evidence)
    )
  )
}

#' Return the set of observed ontology IDs as a character vector.
observed_term_ids <- function(observed_term_universe) {
  observed_term_universe$terms$ontology_id
}
