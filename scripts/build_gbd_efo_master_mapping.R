#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(here)
})

source(here("scripts/pipeline_functions.R"))

GWAS_CATALOG  <- here("Data/gwas_catalog_v1.0.2.1-studies_r2026-06-01.tsv")
FIRST_PART    <- here("Data/First_part_GBD.xlsx")
SECOND_PART   <- here("Data/Second_part_GBD.xlsx")
GBD_GWASCAT   <- here("Data/gbd-gwascat-20260402.xlsx")
MANUAL_TRAITS <- here("Data/Manually curated GBD conditions with zero attention.xlsx")
EFO_OBO       <- here("Data/efo.obo")

OUT_MASTER  <- here("Data/gbd_efo_master_mapping.tsv")
OUT_SUMMARY <- here("Data/gbd_efo_master_mapping_summary.tsv")
CACHE_PATH  <- here("Data/pipeline/cache_descendants.rds")

HIERARCHY_DECAY <- as.numeric(Sys.getenv("GBD_EFO_HIERARCHY_DECAY", "1"))
if (is.na(HIERARCHY_DECAY) || HIERARCHY_DECAY < 0) {
  stop("GBD_EFO_HIERARCHY_DECAY must be a non-negative number")
}

main <- function() {
  for (f in c(GWAS_CATALOG, FIRST_PART, SECOND_PART, MANUAL_TRAITS, EFO_OBO)) {
    if (!file.exists(f)) stop("Required input not found: ", f)
  }

  master <- build_gbd_efo_master_mapping(
    gwas_catalog_path = GWAS_CATALOG,
    first_part_path = FIRST_PART,
    second_part_path = SECOND_PART,
    efo_obo_path = EFO_OBO,
    exclude_causes = EXCLUDE_CAUSES,
    gbd_gwascat_path = GBD_GWASCAT,
    manual_trait_mapping_path = MANUAL_TRAITS,
    descendant_cache_path = CACHE_PATH,
    hierarchy_decay = HIERARCHY_DECAY
  )

  summary <- master %>%
    group_by(gbd_term, mapping_strategy, mapping_source) %>%
    summarise(
      n_mapped_trait_uris = n_distinct(mapped_trait_uri),
      min_hierarchy_distance = min(hierarchy_distance, na.rm = TRUE),
      max_hierarchy_distance = max(hierarchy_distance, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(gbd_term, mapping_strategy, mapping_source)

  fwrite(master, OUT_MASTER, sep = "\t")
  fwrite(summary, OUT_SUMMARY, sep = "\t")

  message("Wrote: ", OUT_MASTER)
  message("  rows: ", nrow(master))
  message("  GBD terms: ", n_distinct(master$gbd_term))
  message("  Active rows: ", sum(master$include_in_pipeline))
  message("  Hierarchy decay: ", HIERARCHY_DECAY)
  message("Wrote: ", OUT_SUMMARY)
}

main()
