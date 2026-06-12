library(dplyr)
library(stringr)
library(tidyr)
library(readxl)

# ── Issue 014: Deterministic Candidate Mapping Review Set ──────────────────
# Generates candidate (GBD condition × observed ontology term) rows from
# non-LLM channels: lexical, GWAS trait-label, legacy mapping, ontology
# neighborhood, and obsolete replacement.
# Ontology hierarchy is candidate evidence ONLY; it cannot directly create
# accepted mappings or scoring.

.norm_text <- function(x) {
  tolower(trimws(gsub("[^a-zA-Z0-9 ]", "", as.character(x))))
}

# ── Helpers ────────────────────────────────────────────────────────────────

# Collapse multiple channel entries into one row per (condition × term)
.merge_candidate_channels <- function(candidates) {
  candidates %>%
    group_by(gbd_condition, ontology_id) %>%
    summarise(
      channels = paste(sort(unique(channel)), collapse = "|"),
      channel_details = paste(sort(unique(channel_detail)), collapse = " ;; "),
      .groups = "drop"
    )
}

# ── Channel 1: Lexical search ──────────────────────────────────────────────
# Match GBD condition name (and aliases) against observed ontology term labels.
# `ontology_labels` is a data frame: ontology_id, label (and optionally synonyms).
.lexical_candidates <- function(gbd_context, ontology_labels) {
  if (nrow(gbd_context) == 0 || nrow(ontology_labels) == 0) {
    return(empty_candidate_rows())
  }

  search_terms <- gbd_context %>%
    select(condition_name, alias) %>%
    mutate(name_search = condition_name) %>%
    tidyr::pivot_longer(
      cols      = c(name_search, alias),
      names_to  = "term_source",
      values_to = "search_text"
    ) %>%
    filter(!is.na(search_text), trimws(search_text) != "") %>%
    mutate(search_norm = .norm_text(search_text)) %>%
    filter(search_norm != "") %>%
    distinct(condition_name, search_text, search_norm, term_source)

  labels_norm <- ontology_labels %>%
    mutate(label_norm = .norm_text(label))

  # Exact substring: GBD search term appears in ontology label
  forward <- search_terms %>%
    cross_join(labels_norm) %>%
    filter(
      str_detect(label_norm, fixed(search_norm)) |
        str_detect(search_norm, fixed(label_norm))
    ) %>%
    transmute(
      gbd_condition  = condition_name,
      ontology_id,
      channel        = "lexical",
      channel_detail = paste0("search='", search_text, "' label='", label, "'")
    )

  forward
}

# ── Channel 2: GWAS trait-label evidence ──────────────────────────────────
# Match GBD condition name against DISEASE/TRAIT strings in the GWAS Catalog,
# then link the matched publications' observed ontology terms.
.gwas_trait_candidates <- function(gbd_context, gwas_attention_raw,
                                   observed_term_ids) {
  if (nrow(gbd_context) == 0 || nrow(gwas_attention_raw) == 0) {
    return(empty_candidate_rows())
  }

  trait_col <- grep("^DISEASE.?TRAIT$", names(gwas_attention_raw),
                    ignore.case = TRUE, value = TRUE)[1]
  uri_col   <- grep("^MAPPED_TRAIT_URI$", names(gwas_attention_raw),
                    ignore.case = TRUE, value = TRUE)[1]
  if (is.na(trait_col) || is.na(uri_col)) return(empty_candidate_rows())

  gwas_index <- gwas_attention_raw %>%
    tidyr::separate_rows(all_of(uri_col), sep = ",\\s*") %>%
    mutate(
      uri_norm   = toupper(trimws(.data[[uri_col]])),
      uri_norm   = gsub("^https?://[^/]+/", "", uri_norm),
      uri_norm   = gsub("^obo/", "", uri_norm),
      uri_norm   = gsub("_", ":", uri_norm),
      trait_norm = .norm_text(.data[[trait_col]])
    ) %>%
    filter(uri_norm %in% observed_term_ids, trait_norm != "") %>%
    distinct(uri_norm, trait_norm, .data[[trait_col]])

  search_terms <- gbd_context %>%
    select(condition_name, alias) %>%
    mutate(name_search = condition_name) %>%
    tidyr::pivot_longer(
      cols      = c(name_search, alias),
      names_to  = "term_source",
      values_to = "search_text"
    ) %>%
    filter(!is.na(search_text), trimws(search_text) != "") %>%
    mutate(search_norm = .norm_text(search_text)) %>%
    filter(search_norm != "") %>%
    distinct(condition_name, search_text, search_norm)

  hits <- search_terms %>%
    cross_join(gwas_index) %>%
    filter(str_detect(trait_norm, fixed(search_norm))) %>%
    transmute(
      gbd_condition  = condition_name,
      ontology_id    = uri_norm,
      channel        = "gwas_trait",
      channel_detail = paste0("search='", search_text, "' trait='",
                              .data[[trait_col]], "'")
    )
  hits
}

# ── Channel 3: Legacy mapping evidence ────────────────────────────────────
# First_part EFO columns and Second_part root URIs, filtered to observed terms.
.legacy_first_part_candidates <- function(first_part_path, exclude_causes,
                                          observed_term_ids) {
  if (is.null(first_part_path) || !file.exists(first_part_path)) {
    return(empty_candidate_rows())
  }

  efo_cols <- paste("EFO", 1:30)
  raw <- readxl::read_xlsx(first_part_path, col_types = "text")
  if (!"GBD term" %in% names(raw)) return(empty_candidate_rows())

  raw %>%
    filter(!is.na(`GBD term`), !`GBD term` %in% exclude_causes) %>%
    tidyr::pivot_longer(
      cols      = any_of(efo_cols),
      names_to  = "source_col",
      values_to = "raw_uri"
    ) %>%
    filter(!is.na(raw_uri), trimws(raw_uri) != "") %>%
    tidyr::separate_rows(raw_uri, sep = ",\\s*") %>%
    mutate(
      ontology_id = toupper(trimws(raw_uri)),
      ontology_id = gsub("^https?://[^/]+/", "", ontology_id),
      ontology_id = gsub("^obo/", "", ontology_id),
      ontology_id = gsub("_", ":", ontology_id)
    ) %>%
    filter(ontology_id %in% observed_term_ids) %>%
    transmute(
      gbd_condition  = str_squish(`GBD term`),
      ontology_id,
      channel        = "legacy_first_part",
      channel_detail = paste0("source_col=", source_col, " raw=", raw_uri)
    )
}

.legacy_second_part_candidates <- function(second_part_path, exclude_causes,
                                           observed_term_ids) {
  if (is.null(second_part_path) || !file.exists(second_part_path)) {
    return(empty_candidate_rows())
  }

  raw <- readxl::read_xlsx(second_part_path, col_types = "text")
  if (!"GBD term" %in% names(raw)) return(empty_candidate_rows())

  uri_cols <- grep("^MAPPED_TRAIT_URI", names(raw), value = TRUE)
  if (length(uri_cols) == 0) return(empty_candidate_rows())

  raw %>%
    filter(!is.na(`GBD term`), !`GBD term` %in% exclude_causes) %>%
    tidyr::pivot_longer(
      cols      = all_of(uri_cols),
      names_to  = "source_col",
      values_to = "raw_uri"
    ) %>%
    filter(!is.na(raw_uri), trimws(raw_uri) != "") %>%
    mutate(
      ontology_id = toupper(trimws(raw_uri)),
      ontology_id = gsub("^https?://[^/]+/", "", ontology_id),
      ontology_id = gsub("^obo/", "", ontology_id),
      ontology_id = gsub("_", ":", ontology_id)
    ) %>%
    filter(ontology_id %in% observed_term_ids) %>%
    transmute(
      gbd_condition  = str_squish(`GBD term`),
      ontology_id,
      channel        = "legacy_second_part",
      channel_detail = paste0("source_col=", source_col, " raw=", raw_uri)
    )
}

# ── Channel 4: Obsolete replacement evidence ──────────────────────────────
# Maps old observed IDs to their replacement IDs (from OBO) and flags them.
.obsolete_replacement_candidates <- function(gbd_context,
                                             legacy_candidates,
                                             obsolete_replacements,
                                             observed_term_ids) {
  if (is.null(obsolete_replacements) || nrow(obsolete_replacements) == 0 ||
      nrow(legacy_candidates) == 0) {
    return(empty_candidate_rows())
  }

  # Normalise replacement table IDs to observed universe format
  obs_tbl <- obsolete_replacements %>%
    mutate(
      source_norm  = toupper(gsub("^efo:", "", source_id, ignore.case = TRUE)),
      source_norm  = gsub("_", ":", source_norm),
      replace_norm = toupper(gsub("^efo:", "", replacement_id, ignore.case = TRUE)),
      replace_norm = gsub("_", ":", replace_norm)
    ) %>%
    filter(replace_norm %in% observed_term_ids)

  if (nrow(obs_tbl) == 0) return(empty_candidate_rows())

  legacy_candidates %>%
    inner_join(obs_tbl, by = c("ontology_id" = "source_norm"),
               relationship = "many-to-many") %>%
    transmute(
      gbd_condition  = gbd_condition,
      ontology_id    = replace_norm,
      channel        = "obsolete_replacement",
      channel_detail = paste0("replaced=", ontology_id, " via=", replacement_type)
    ) %>%
    filter(ontology_id %in% observed_term_ids)
}

# ── Channel 5: Ontology neighborhood evidence (candidates only) ────────────
# Proposes ontology parents and children as candidates for awareness/review.
# These are NOT accepted mappings and MUST NOT produce scores directly.
.ontology_neighborhood_candidates <- function(gbd_context,
                                              confirmed_candidates,
                                              ontology_parents,
                                              ontology_children,
                                              observed_term_ids) {
  if (is.null(ontology_parents) && is.null(ontology_children)) {
    return(empty_candidate_rows())
  }
  if (nrow(confirmed_candidates) == 0) return(empty_candidate_rows())

  rows <- list()

  if (!is.null(ontology_parents) && nrow(ontology_parents) > 0) {
    parents <- confirmed_candidates %>%
      inner_join(ontology_parents, by = c("ontology_id" = "child_id"),
                 relationship = "many-to-many") %>%
      filter(parent_id %in% observed_term_ids) %>%
      transmute(
        gbd_condition  = gbd_condition,
        ontology_id    = parent_id,
        channel        = "ontology_neighborhood",
        channel_detail = paste0("via_parent_of=", ontology_id)
      )
    rows <- c(rows, list(parents))
  }

  if (!is.null(ontology_children) && nrow(ontology_children) > 0) {
    children <- confirmed_candidates %>%
      inner_join(ontology_children, by = c("ontology_id" = "parent_id"),
                 relationship = "many-to-many") %>%
      filter(child_id %in% observed_term_ids) %>%
      transmute(
        gbd_condition  = gbd_condition,
        ontology_id    = child_id,
        channel        = "ontology_neighborhood",
        channel_detail = paste0("via_child_of=", ontology_id)
      )
    rows <- c(rows, list(children))
  }

  bind_rows(rows)
}

# ── Empty scaffold ─────────────────────────────────────────────────────────
empty_candidate_rows <- function() {
  tibble(
    gbd_condition  = character(),
    ontology_id    = character(),
    channel        = character(),
    channel_detail = character()
  )
}

#' Generate deterministic candidate mapping rows.
#'
#' @param observed_term_universe Output of build_observed_term_universe().
#' @param gbd_context Output of build_gbd_condition_context().
#' @param gwas_catalog_raw Raw data frame loaded from GWAS Catalog (optional).
#' @param ontology_labels Data frame with columns ontology_id, label (optional).
#' @param first_part_path Path to First_part_GBD.xlsx (optional).
#' @param second_part_path Path to Second_part_GBD.xlsx (optional).
#' @param obsolete_replacements Data frame from .parse_obo_obsolete_replacements() (optional).
#' @param ontology_parents Data frame with columns child_id, parent_id (optional).
#' @param ontology_children Data frame with columns parent_id, child_id (optional).
#' @param exclude_causes Character vector of GBD cause names to exclude.
#' @return Data frame with one row per (gbd_condition × ontology_id):
#'   channels (pipe-separated list), channel_details
generate_deterministic_candidates <- function(
    observed_term_universe,
    gbd_context,
    gwas_catalog_raw      = NULL,
    ontology_labels       = NULL,
    first_part_path       = NULL,
    second_part_path      = NULL,
    obsolete_replacements = NULL,
    ontology_parents      = NULL,
    ontology_children     = NULL,
    exclude_causes        = character(0)
) {
  obs_ids <- observed_term_universe$terms$ontology_id

  all_candidates <- list()

  if (!is.null(ontology_labels) && nrow(ontology_labels) > 0) {
    obs_labels <- ontology_labels %>% filter(ontology_id %in% obs_ids)
    lex <- .lexical_candidates(gbd_context, obs_labels)
    if (nrow(lex) > 0) all_candidates[["lexical"]] <- lex
  }

  if (!is.null(gwas_catalog_raw) && nrow(gwas_catalog_raw) > 0) {
    gw <- .gwas_trait_candidates(gbd_context, gwas_catalog_raw, obs_ids)
    if (nrow(gw) > 0) all_candidates[["gwas_trait"]] <- gw
  }

  if (!is.null(first_part_path)) {
    fp <- .legacy_first_part_candidates(first_part_path, exclude_causes, obs_ids)
    if (nrow(fp) > 0) all_candidates[["legacy_first_part"]] <- fp
  }

  if (!is.null(second_part_path)) {
    sp <- .legacy_second_part_candidates(second_part_path, exclude_causes, obs_ids)
    if (nrow(sp) > 0) all_candidates[["legacy_second_part"]] <- sp
  }

  if (length(all_candidates) == 0) return(empty_candidate_rows())

  # Combine and deduplicate with provenance
  raw_candidates <- bind_rows(all_candidates)

  if (!is.null(obsolete_replacements)) {
    obsrep <- .obsolete_replacement_candidates(
      gbd_context, raw_candidates, obsolete_replacements, obs_ids
    )
    if (nrow(obsrep) > 0) raw_candidates <- bind_rows(raw_candidates, obsrep)
  }

  if (!is.null(ontology_parents) || !is.null(ontology_children)) {
    neighborhood <- .ontology_neighborhood_candidates(
      gbd_context, raw_candidates, ontology_parents, ontology_children, obs_ids
    )
    if (nrow(neighborhood) > 0) raw_candidates <- bind_rows(raw_candidates, neighborhood)
  }

  # Collapse to one row per (gbd_condition × ontology_id)
  .merge_candidate_channels(raw_candidates)
}
