library(here)
library(dplyr)
library(readr)
source(here("scripts/config.R"))
source(here("scripts/canonical_mapping/R/quality_gates.R"))
source(here("scripts/canonical_mapping/R/human_review_compiler.R"))

# ── Export canonical mapping → gbd_efo_master_mapping.tsv ─────────────────
# Converts accepted rows from 04_evidence_package_reviewed.csv into the
# master mapping format consumed by scripts/pipeline.R.
#
# Usage:
#   Rscript scripts/canonical_mapping/export_to_master_mapping.R
#
# Output: configured canonical_mapping.master_mapping path
#
# Key constraint: hierarchy_distance = 0, hierarchy_weight = 1 for all rows.
# The canonical mapping contains ONLY observed, human-accepted direct mappings;
# no hierarchy expansion is applied here. Rollup through the GBD hierarchy
# happens inside pipeline.R's rollup_hierarchy() as normal.

cfg <- load_project_config()
evidence_pkg_path <- cfg_path(cfg_get(cfg, "canonical_mapping.reviewed_evidence_package"), cfg)
observed_terms_path <- cfg_path(cfg_get(cfg, "canonical_mapping.observed_terms"), cfg)
condition_context_path <- cfg_path(cfg_get(cfg, "canonical_mapping.condition_context"), cfg)
output_path       <- cfg_path(cfg_get(cfg, "canonical_mapping.master_mapping"), cfg)

if (!file.exists(evidence_pkg_path)) {
  stop("Evidence package not found: ", evidence_pkg_path,
       "\nRun workflow.R and llm_batch_review or llm_batch_collect first.")
}

pkg <- read.csv(evidence_pkg_path, stringsAsFactors = FALSE, na.strings = c("", "NA"))
observed_terms <- read.csv(observed_terms_path, stringsAsFactors = FALSE, na.strings = c("", "NA"))
condition_context <- read.csv(condition_context_path, stringsAsFactors = FALSE, na.strings = c("", "NA"))
observed_term_universe <- list(terms = observed_terms)

compiled <- compile_human_review(
  reviewed_pkg = pkg,
  observed_term_universe = observed_term_universe,
  gbd_context = condition_context,
  sentinel_conditions = unlist(cfg_get(
    cfg,
    "canonical_mapping.sentinel_conditions",
    default = character(0),
    required = FALSE
  ))
)

message(compiled$summary$gate_report)

master <- canonical_export_table(compiled, evidence_source = basename(evidence_pkg_path))

n_accepted <- nrow(master)
n_conditions <- dplyr::n_distinct(master$gbd_term)

message(sprintf("Accepted human mappings : %d", n_accepted))
message(sprintf("GBD conditions          : %d", n_conditions))

dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
readr::write_tsv(master, output_path, na = "")

message(sprintf("\nWritten: %s", output_path))
message(sprintf("  Rows           : %d", nrow(master)))
message(sprintf("  GBD terms      : %d", dplyr::n_distinct(master$gbd_term)))
message(sprintf("  Unique EFO/OBO : %d", dplyr::n_distinct(master$mapped_trait_uri)))
message("\nNext: Rscript scripts/pipeline.R")
