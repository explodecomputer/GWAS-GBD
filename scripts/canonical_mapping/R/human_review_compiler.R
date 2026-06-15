library(dplyr)

# ── Issue 019: Human Review Compiler ──────────────────────────────────────
# Compiles reviewed evidence packages into a canonical catalog-release mapping.
# Human decisions take precedence. Where human_decision is absent, model-accepted
# rows are included as a fallback (decision_status = "model_accepted"). Human
# reject/unsure always overrides the model for that row.

#' Compile a reviewed evidence package into a canonical catalog-release mapping.
#'
#' @param reviewed_pkg Evidence package data frame with human_decision column filled.
#' @param observed_term_universe Output of build_observed_term_universe().
#' @param gbd_context Output of build_gbd_condition_context().
#' @param sentinel_conditions Character vector of known non-zero conditions.
#' @return A list:
#'   - `canonical`: data frame of accepted mappings ready for quality gate checks.
#'   - `audit_trail`: full evidence package with decision statuses.
#'   - `gate_result`: output of run_mapping_quality_gates() on the canonical mapping.
#'   - `summary`: named list with row counts by decision status.
compile_human_review <- function(reviewed_pkg,
                                  observed_term_universe,
                                  gbd_context,
                                  sentinel_conditions = character(0)) {
  if (!"human_decision" %in% names(reviewed_pkg)) {
    stop("Evidence package missing 'human_decision' column. Has it been reviewed?")
  }

  # Classify each row. Human decision wins when present; model-accept is the
  # fallback for unreviewed rows where the model recommended acceptance.
  reviewed_pkg <- reviewed_pkg %>%
    mutate(
      decision_status = case_when(
        tolower(trimws(human_decision)) == "accept"  ~ "accepted",
        tolower(trimws(human_decision)) == "reject"  ~ "rejected",
        tolower(trimws(human_decision)) == "unsure"  ~ "unsure",
        is.na(human_decision) & tolower(trimws(model_recommendation)) == "accept"  ~ "model_accepted",
        !is.na(model_recommendation) & is.na(human_decision) ~ "model_reviewed",
        TRUE ~ "unreviewed"
      )
    )

  n_by_status_tbl <- reviewed_pkg %>%
    count(decision_status, name = "n")
  .status_count <- function(s) {
    v <- n_by_status_tbl %>% dplyr::filter(decision_status == s) %>% dplyr::pull(n)
    if (length(v) == 0L) 0L else v
  }

  # Canonical = human-accepted + model-accepted fallback. Keep duplicate rows
  # until quality gates run so duplicate decisions fail loudly.
  canonical <- reviewed_pkg %>%
    filter(decision_status %in% c("accepted", "model_accepted")) %>%
    select(
      decision_status,
      gbd_condition,
      ontology_id,
      any_of(c("catalog_release")),
      any_of(c("human_relationship", "human_notes", "reviewer_id", "review_date")),
      any_of(c("channels", "channel_details")),
      any_of(c("label", "pubmed_count", "association_count"))
    )

  # Run quality gates on the canonical mapping
  gate_result <- run_mapping_quality_gates(
    canonical            = canonical,
    observed_term_universe = observed_term_universe,
    gbd_context          = gbd_context,
    sentinel_conditions  = sentinel_conditions
  )

  list(
    canonical    = canonical,
    audit_trail  = reviewed_pkg,
    gate_result  = gate_result,
    summary      = list(
      n_accepted          = .status_count("accepted"),
      n_model_accepted    = .status_count("model_accepted"),
      n_canonical_total   = nrow(canonical),
      n_rejected          = .status_count("rejected"),
      n_unsure            = .status_count("unsure"),
      n_model_reviewed    = .status_count("model_reviewed"),
      n_unreviewed        = .status_count("unreviewed"),
      gates_passed        = gate_result$passed,
      gate_report         = gate_result$report
    )
  )
}

#' Extract the scoring-ready canonical mapping from a compiled result.
#' Fails with an error if quality gates did not pass.
#'
#' @param compiled Output of compile_human_review().
#' @return Data frame: gbd_condition, ontology_id (and metadata columns).
canonical_mapping_for_scoring <- function(compiled) {
  if (!compiled$summary$gates_passed) {
    stop(
      "Canonical mapping failed quality gates. Resolve FAIL issues before scoring.\n",
      compiled$summary$gate_report
    )
  }
  compiled$canonical %>%
    distinct(gbd_condition, ontology_id, .keep_all = TRUE)
}

#' Build the master mapping table consumed by the attention score pipeline.
#'
#' @param compiled Output of compile_human_review().
#' @param evidence_source File name or label used for provenance.
#' @return Data frame matching empty_gbd_efo_master_mapping() from
#'   scripts/attention_scores/pipeline_functions.R, without depending on that module.
canonical_export_table <- function(compiled, evidence_source = NA_character_) {
  canonical <- canonical_mapping_for_scoring(compiled)

  canonical %>%
    mutate(
      gbd_term = gbd_condition,
      mapped_trait_uri = ontology_id,
      mapping_strategy = "canonical",
      mapping_source = dplyr::if_else(
        decision_status == "accepted",
        "canonical_human_review",
        "canonical_model_review"
      ),
      include_in_pipeline = TRUE,
      hierarchy_distance = 0L,
      hierarchy_weight = 1,
      root_mapped_trait_uri = ontology_id,
      lookup_trait_uri = NA_character_,
      replacement_type = "original",
      source_file = evidence_source,
      source_column = "ontology_id",
      source_value = ontology_id,
      search_term = gbd_condition,
      matched_gwas_trait_examples = if ("example_trait_labels" %in% names(.)) example_trait_labels else NA_character_,
      n_matching_pubmeds = if ("pubmed_count" %in% names(.)) pubmed_count else NA_integer_
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
}
