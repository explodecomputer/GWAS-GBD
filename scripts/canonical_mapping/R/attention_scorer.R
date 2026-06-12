library(dplyr)
library(tidyr)
library(stringr)
library(readxl)

# ── Issue 020: Canonical Mapping Attention Scorer ─────────────────────────
# Computes GWAS attention only from human-accepted canonical mappings.
# No ontology hierarchy expansion; parent scores come from GBD hierarchy rollup.

.normalise_id_for_join <- function(uri) {
  uri <- toupper(trimws(as.character(uri)))
  uri <- gsub("^https?://[^/]+/", "", uri)
  uri <- gsub("^obo/", "", uri)
  uri <- gsub("_", ":", uri)
  uri
}

.load_canonical_attention_raw <- function(gwas_catalog_path) {
  ext <- tolower(tools::file_ext(gwas_catalog_path))
  raw <- switch(
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

  required <- c("MAPPED_TRAIT_URI", "PUBMEDID")
  missing  <- setdiff(required, names(raw))
  if (length(missing) > 0) stop("GWAS Catalog missing columns: ", paste(missing, collapse = ", "))

  # Resolve column names before mutate (handles space vs underscore variants)
  assoc_col <- intersect(c("ASSOCIATION COUNT", "ASSOCIATION_COUNT"), names(raw))[1]
  date_col  <- intersect(c("DATE", "Date"), names(raw))[1]

  # Compute n_efo before expanding rows (count distinct URIs per publication)
  pubmed_efo_count <- raw %>%
    group_by(PUBMEDID) %>%
    summarise(n_efo = length(unique(MAPPED_TRAIT_URI)), .groups = "drop")

  raw <- raw %>%
    left_join(pubmed_efo_count, by = "PUBMEDID") %>%
    tidyr::separate_rows(MAPPED_TRAIT_URI, sep = ",\\s*") %>%
    mutate(
      ontology_id_norm  = .normalise_id_for_join(MAPPED_TRAIT_URI),
      ASSOCIATION_COUNT = coalesce(
        suppressWarnings(as.integer(if (!is.na(assoc_col)) .data[[assoc_col]] else NA_character_)),
        0L
      ),
      pub_year = suppressWarnings(as.integer(
        format(as.Date(if (!is.na(date_col)) .data[[date_col]] else "1970-01-01"), "%Y")
      ))
    ) %>%
    filter(!is.na(ontology_id_norm), ontology_id_norm != "")

  raw
}

# ── All-time scoring ───────────────────────────────────────────────────────

#' Score GWAS attention from canonical accepted mappings (all-time).
#'
#' @param canonical Data frame of accepted mappings: gbd_condition, ontology_id.
#' @param gwas_catalog_path Path to the GWAS Catalog release file.
#' @return Data frame: gbd_condition, total_attention_score, pubmed_count,
#'   association_count, zero_attention (logical).
score_canonical_attention <- function(canonical, gwas_catalog_path) {
  raw <- .load_canonical_attention_raw(gwas_catalog_path)

  # Canonical mapping: one row per (gbd_condition × ontology_id) — dedup
  edges <- canonical %>%
    distinct(gbd_condition, ontology_id)

  # Match catalog rows to canonical edges (one edge per publication per EFO)
  matched <- raw %>%
    inner_join(edges, by = c("ontology_id_norm" = "ontology_id"),
               relationship = "many-to-many")

  # Aggregate to (gbd_condition × PUBMEDID) first to avoid double-counting
  per_pubmed <- matched %>%
    group_by(gbd_condition, PUBMEDID) %>%
    summarise(
      n_per_pubmed    = sum(1 / coalesce(n_efo, 1), na.rm = TRUE),
      assoc_per_pubmed = sum(ASSOCIATION_COUNT, na.rm = TRUE),
      .groups = "drop"
    )

  scored <- per_pubmed %>%
    group_by(gbd_condition) %>%
    summarise(
      total_attention_score = sum(n_per_pubmed, na.rm = TRUE),
      pubmed_count          = n_distinct(PUBMEDID),
      association_count     = sum(assoc_per_pubmed, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(zero_attention = FALSE)

  # Zero-attention conditions: in canonical but no GWAS match
  all_conditions <- canonical %>% distinct(gbd_condition)
  zero_rows <- all_conditions %>%
    filter(!gbd_condition %in% scored$gbd_condition) %>%
    mutate(
      total_attention_score = 0,
      pubmed_count          = 0L,
      association_count     = 0L,
      zero_attention        = TRUE
    )

  bind_rows(scored, zero_rows) %>%
    arrange(gbd_condition)
}

# ── Temporal scoring ───────────────────────────────────────────────────────

#' Build year-level and 3-year sliding window attention scores.
#'
#' @param canonical Data frame: gbd_condition, ontology_id.
#' @param gwas_catalog_path Path to the GWAS Catalog release file.
#' @return Data frame: gbd_condition, total_attention_score, analysis_type,
#'   time_strata.
build_canonical_temporal_scores <- function(canonical, gwas_catalog_path) {
  raw <- .load_canonical_attention_raw(gwas_catalog_path)
  edges <- canonical %>% distinct(gbd_condition, ontology_id)

  matched <- raw %>%
    inner_join(edges, by = c("ontology_id_norm" = "ontology_id"),
               relationship = "many-to-many") %>%
    filter(!is.na(pub_year))

  years <- sort(unique(matched$pub_year))

  year_scores <- lapply(years, function(yr) {
    yr_data <- matched %>% filter(pub_year == yr)
    per_pubmed <- yr_data %>%
      group_by(gbd_condition, PUBMEDID) %>%
      summarise(
        n_per_pubmed = sum(1 / coalesce(n_efo, 1), na.rm = TRUE),
        .groups = "drop"
      )
    per_pubmed %>%
      group_by(gbd_condition) %>%
      summarise(
        total_attention_score = sum(n_per_pubmed, na.rm = TRUE),
        analysis_type         = "year",
        time_strata           = yr,
        .groups = "drop"
      ) %>%
      filter(total_attention_score > 0)
  })

  year_data <- bind_rows(year_scores)

  if (nrow(year_data) == 0) return(year_data)

  min_yr <- min(year_data$time_strata)
  max_yr <- max(year_data$time_strata) - 2L

  sliding_data <- if (max_yr >= min_yr) {
    bind_rows(lapply(min_yr:max_yr, function(i) {
      year_data %>%
        filter(time_strata %in% c(i, i + 1L, i + 2L)) %>%
        group_by(gbd_condition) %>%
        summarise(
          total_attention_score = sum(total_attention_score, na.rm = TRUE),
          analysis_type         = "sliding_3yr",
          time_strata           = i,
          .groups = "drop"
        )
    }))
  } else {
    tibble(
      gbd_condition = character(), total_attention_score = numeric(),
      analysis_type = character(), time_strata = integer()
    )
  }

  bind_rows(year_data, sliding_data)
}

# ── GBD hierarchy rollup ───────────────────────────────────────────────────

#' Roll up canonical leaf scores through the GBD hierarchy.
#' Parent scores are the sum of accepted children; no duplicate direct mappings.
#'
#' @param leaf_scores Output of score_canonical_attention().
#' @param hierarchy_path Path to IHME GBD hierarchy Excel file.
#' @return Data frame with GBD condition, cause_id, total_attention_score.
rollup_canonical_hierarchy <- function(leaf_scores, hierarchy_path) {
  sheet_names <- excel_sheets(hierarchy_path)
  cause_sheet <- grep("cause hierarchy", sheet_names, ignore.case = TRUE,
                      value = TRUE)[1]
  if (is.na(cause_sheet)) stop("No Cause Hierarchy sheet in: ", hierarchy_path)

  h <- read_xlsx(hierarchy_path, sheet = cause_sheet, col_types = "text")
  names(h) <- trimws(names(h))

  cause_id_col   <- grep("^[Cc]ause.?[Ii][Dd]$",  names(h), value = TRUE)[1]
  cause_name_col <- grep("^[Cc]ause.?[Nn]ame$",    names(h), value = TRUE)[1]
  parent_col     <- grep("^[Pp]arent.?[Nn]ame$",   names(h), value = TRUE)[1]
  level_col      <- grep("^[Ll]evel$",              names(h), value = TRUE)[1]
  outline_col    <- grep("^[Cc]ause.?[Oo]utline$", names(h), value = TRUE)[1]

  h <- h %>%
    rename(
      cause_name  = all_of(cause_name_col),
      parent_name = all_of(parent_col),
      gbd_level   = all_of(level_col)
    ) %>%
    mutate(
      cause_id  = if (!is.na(cause_id_col)) .data[[cause_id_col]] else NA_character_,
      outline   = if (!is.na(outline_col))  .data[[outline_col]]  else NA_character_,
      gbd_level = suppressWarnings(as.integer(gbd_level))
    )

  h_analysis <- h %>%
    filter(grepl("^[AB]", outline), gbd_level %in% c(3L, 4L))

  leaf_nodes <- h_analysis %>%
    filter(!cause_name %in% parent_name) %>%
    pull(cause_name)

  leaf_joined <- leaf_nodes %>%
    tibble(cause_name = .) %>%
    left_join(leaf_scores, by = c("cause_name" = "gbd_condition")) %>%
    left_join(h_analysis %>% select(cause_name, cause_id), by = "cause_name") %>%
    mutate(total_attention_score = coalesce(total_attention_score, 0))

  # Parents: rollup leaf children's scores
  parent_nodes <- h_analysis %>%
    filter(cause_name %in% parent_name) %>%
    distinct(cause_name, cause_id)

  child_sums <- h_analysis %>%
    filter(!is.na(parent_name)) %>%
    select(cause_name, parent_name) %>%
    inner_join(leaf_scores, by = c("cause_name" = "gbd_condition")) %>%
    group_by(parent_name) %>%
    summarise(child_score = sum(total_attention_score, na.rm = TRUE), .groups = "drop")

  parents_rolled <- parent_nodes %>%
    left_join(child_sums, by = c("cause_name" = "parent_name")) %>%
    left_join(leaf_scores, by = c("cause_name" = "gbd_condition")) %>%
    mutate(
      total_attention_score = coalesce(total_attention_score, 0) +
        coalesce(child_score, 0)
    ) %>%
    select(cause_name, cause_id, total_attention_score)

  bind_rows(
    leaf_joined %>% select(cause_name, cause_id, total_attention_score),
    parents_rolled
  ) %>%
    distinct(cause_name, .keep_all = TRUE) %>%
    arrange(cause_name)
}
