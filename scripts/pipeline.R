#!/usr/bin/env Rscript
# Usage: Rscript scripts/pipeline.R
# Regenerates Data/GBD_combined_dataset_EFO.csv and
# Data/merged_dataset_exclude_Injuries.csv from source inputs.

library(here)
library(data.table)
source(here("scripts/pipeline_functions.R"))

# ── Input paths ───────────────────────────────────────────────────────────
GWAS_CATALOG  <- here("Data/gwas_catalog_v1.0.2.1-studies_r2026-06-01.tsv")
MASTER_MAPPING <- here("Data/gbd_efo_master_mapping.tsv")
GBD_HIERARCHY <- here("Data/IHME_GBD_2023_HIERARCHIES_Y2025M10D23.XLSX")

# ── Output paths ──────────────────────────────────────────────────────────
OUT_UNIVERSE <- here("Data/gbd_universe_275.csv")
OUT_COMBINED <- here("Data/GBD_combined_dataset_EFO.csv")
OUT_TEMPORAL <- here("Data/merged_dataset_exclude_Injuries.csv")

# ── Input validation ──────────────────────────────────────────────────────
for (f in c(GWAS_CATALOG, MASTER_MAPPING, GBD_HIERARCHY)) {
  if (!file.exists(f)) stop("Required input not found: ", f)
}

# ── Step 0: Derive and save GBD universe ──────────────────────────────────
message("Step 0: Deriving GBD universe (non-injury leaf terms at levels 3/4)...")
gbd_universe <- derive_gbd_universe(GBD_HIERARCHY)
message("  GBD universe: ", nrow(gbd_universe), " terms")
fwrite(gbd_universe, OUT_UNIVERSE)
message("  Written: ", OUT_UNIVERSE)

# ── Step 1: Attention scores per EFO × publication ────────────────────────
message("Step 1: Loading GWAS attention data…")
attention <- load_gwas_attention(GWAS_CATALOG)
message("  ", nrow(attention), " EFO × publication rows loaded")

# ── Step 2: GBD–EFO master mapping table ──────────────────────────────────
message("Step 2: Loading GBD–EFO master mapping…")
master_map <- load_gbd_efo_master_mapping(MASTER_MAPPING, active_only = TRUE)
message("  Master map: ", nrow(master_map), " active rows")
message("  GBD terms: ", dplyr::n_distinct(master_map$gbd_term))

# ── Step 2b: Append manual zero-attention mappings ────────────────────────
MANUAL_ZERO_MAP <- here("Data/manual_zero_attention_mapping.csv")
if (file.exists(MANUAL_ZERO_MAP)) {
  manual_zero <- data.table::fread(MANUAL_ZERO_MAP, na.strings = c("", "NA")) %>%
    dplyr::filter(!is.na(mapped_trait_uri), nzchar(mapped_trait_uri)) %>%
    # Split comma-separated URIs into one row each, then normalise _ → :
    tidyr::separate_rows(mapped_trait_uri, sep = ",\\s*") %>%
    dplyr::mutate(mapped_trait_uri = gsub("_", ":", trimws(mapped_trait_uri))) %>%
    dplyr::filter(nzchar(mapped_trait_uri))
  if (nrow(manual_zero) > 0) {
    manual_rows <- manual_zero %>%
      dplyr::transmute(
        gbd_term              = cause_name,
        mapped_trait_uri      = mapped_trait_uri,
        mapping_strategy      = "manual",
        mapping_source        = "manual_zero_attention_mapping",
        include_in_pipeline   = TRUE,
        hierarchy_distance    = 0L,
        hierarchy_weight      = 1,
        root_mapped_trait_uri = mapped_trait_uri,
        lookup_trait_uri      = NA_character_,
        replacement_type      = "original",
        source_file           = "manual_zero_attention_mapping.csv",
        source_column         = "mapped_trait_uri",
        source_value          = mapped_trait_uri,
        search_term           = cause_name,
        matched_gwas_trait_examples = NA_character_,
        n_matching_pubmeds    = NA_integer_
      )
    master_map <- dplyr::bind_rows(master_map, manual_rows)
    message("  Manual zero-attention mappings added: ", nrow(manual_rows),
            " rows for ", dplyr::n_distinct(manual_rows$gbd_term), " terms")
  } else {
    message("  Manual zero-attention mapping file found but no filled rows yet")
  }
}

# ── Step 4: Map attention → GBD leaf terms ────────────────────────────────
message("Step 4: Mapping attention scores to GBD leaf terms…")
leaf_scores <- map_attention_to_gbd_leaves_from_master(
  attention, master_map, EXCLUDE_CAUSES
)
message("  Leaf scores: ", nrow(leaf_scores), " GBD terms")

# ── Step 5: Roll up through GBD hierarchy ─────────────────────────────────
message("Step 5: Rolling up hierarchy…")
all_scores_raw <- rollup_hierarchy(leaf_scores, GBD_HIERARCHY)
all_scores <- all_scores_raw %>%
  dplyr::filter(GBD.term %in% gbd_universe$cause_name)
message("  All-time scores: ", nrow(all_scores), " GBD terms (", nrow(all_scores_raw),
        " before universe filter)")

fwrite(
  all_scores %>% dplyr::select(GBD.term, total_attention_score, weighted_nhits,
                                weighted_attention_score_impact_factor, weighted_n, nhit),
  OUT_COMBINED
)
message("  Written: ", OUT_COMBINED)

# ── Step 6: Temporal stratifications ──────────────────────────────────────
message("Step 6: Building temporal scores (year + sliding 3yr)…")
temporal_scores <- build_temporal_scores_from_master(
  attention, master_map, EXCLUDE_CAUSES, GBD_HIERARCHY
)
message("  Temporal scores: ", nrow(temporal_scores), " rows")

# ── Step 7: Assemble and write final output ────────────────────────────────
message("Step 7: Assembling final output…")
final <- assemble_output(all_scores, temporal_scores)

fwrite(final, OUT_TEMPORAL)
message("  Written: ", OUT_TEMPORAL)

message("\nDone.")
message("  ", OUT_COMBINED, " — ", nrow(all_scores), " rows")
message("  ", OUT_TEMPORAL, " — ", nrow(final), " rows")
