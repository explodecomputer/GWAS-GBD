library(here)
library(dplyr)
library(readr)

# ── Export canonical mapping → gbd_efo_master_mapping.tsv ─────────────────
# Converts accepted rows from 04_evidence_package_reviewed.csv into the
# master mapping format consumed by scripts/pipeline.R.
#
# Usage:
#   Rscript scripts/canonical_mapping/export_to_master_mapping.R
#
# Output: Data/gbd_efo_master_mapping.tsv
#
# Key constraint: hierarchy_distance = 0, hierarchy_weight = 1 for all rows.
# The canonical mapping contains ONLY observed, human-accepted direct mappings;
# no hierarchy expansion is applied here. Rollup through the GBD hierarchy
# happens inside pipeline.R's rollup_hierarchy() as normal.

evidence_pkg_path <- here("outputs/canonical_mapping/04_evidence_package_reviewed.csv")
output_path       <- here("Data/gbd_efo_master_mapping.tsv")

if (!file.exists(evidence_pkg_path)) {
  stop("Evidence package not found: ", evidence_pkg_path,
       "\nRun workflow.R and llm_batch_review or llm_batch_collect first.")
}

pkg <- read.csv(evidence_pkg_path, stringsAsFactors = FALSE, na.strings = c("", "NA"))

# Prefer explicit human decision; fall back to LLM model recommendation
pkg <- pkg %>%
  mutate(decision = dplyr::coalesce(
    dplyr::na_if(as.character(human_decision), "NA"),
    model_recommendation
  ))

accepted <- pkg %>%
  filter(decision == "accept") %>%
  select(
    gbd_condition, ontology_id, label,
    pubmed_count, example_trait_labels,
    human_decision, model_recommendation,
    human_relationship, model_relationship,
    human_notes, reviewer_id
  )

n_accepted   <- nrow(accepted)
n_conditions <- dplyr::n_distinct(accepted$gbd_condition)
n_human      <- sum(!is.na(accepted$human_decision) & accepted$human_decision == "accept")

message(sprintf("Accepted mappings : %d", n_accepted))
message(sprintf("GBD conditions    : %d", n_conditions))
message(sprintf("Human decisions   : %d  (remainder: LLM recommendation)", n_human))

# ── Build master mapping rows ──────────────────────────────────────────────
# mapping_source distinguishes human-accepted from LLM-only rows so the
# comparison QMD and downstream diagnostics can filter if needed.

master <- accepted %>%
  mutate(
    gbd_term             = gbd_condition,
    mapped_trait_uri     = ontology_id,   # cleaned by pipeline.R on load
    mapping_strategy     = "canonical",
    mapping_source       = dplyr::if_else(
      !is.na(human_decision) & human_decision == "accept",
      "canonical_human_review",
      "canonical_llm_review"
    ),
    include_in_pipeline  = TRUE,
    hierarchy_distance   = 0L,
    hierarchy_weight     = 1,
    root_mapped_trait_uri = ontology_id,
    lookup_trait_uri     = NA_character_,
    replacement_type     = "original",
    source_file          = basename(evidence_pkg_path),
    source_column        = "ontology_id",
    source_value         = ontology_id,
    search_term          = gbd_condition,
    matched_gwas_trait_examples = example_trait_labels,
    n_matching_pubmeds   = pubmed_count
  ) %>%
  select(
    gbd_term,
    mapped_trait_uri,
    mapping_strategy,
    mapping_source,
    include_in_pipeline,
    hierarchy_distance,
    hierarchy_weight,
    root_mapped_trait_uri,
    lookup_trait_uri,
    replacement_type,
    source_file,
    source_column,
    source_value,
    search_term,
    matched_gwas_trait_examples,
    n_matching_pubmeds
  )

readr::write_tsv(master, output_path, na = "")

message(sprintf("\nWritten: %s", output_path))
message(sprintf("  Rows           : %d", nrow(master)))
message(sprintf("  GBD terms      : %d", dplyr::n_distinct(master$gbd_term)))
message(sprintf("  Unique EFO/OBO : %d", dplyr::n_distinct(master$mapped_trait_uri)))
message("\nNext: Rscript scripts/pipeline.R")
