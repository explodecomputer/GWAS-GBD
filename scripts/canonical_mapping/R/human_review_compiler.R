library(dplyr)

# ── Issue 019: Human Review Compiler ──────────────────────────────────────
# Compiles reviewed evidence packages into a canonical catalog-release mapping.
# Only human_decision == "accept" rows enter the default canonical mapping.
# Rejected, unsure, and provisional (model-only) rows are preserved for audit.

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

  # Classify each row
  reviewed_pkg <- reviewed_pkg %>%
    mutate(
      decision_status = case_when(
        tolower(trimws(human_decision)) == "accept"  ~ "accepted",
        tolower(trimws(human_decision)) == "reject"  ~ "rejected",
        tolower(trimws(human_decision)) == "unsure"  ~ "unsure",
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

  # Canonical = human-accepted only
  canonical <- reviewed_pkg %>%
    filter(decision_status == "accepted") %>%
    select(
      gbd_condition,
      ontology_id,
      any_of(c("catalog_release")),
      any_of(c("human_relationship", "human_notes", "reviewer_id", "review_date")),
      any_of(c("channels", "channel_details")),
      any_of(c("label", "pubmed_count", "association_count"))
    ) %>%
    distinct(gbd_condition, ontology_id, .keep_all = TRUE)

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
      n_accepted      = nrow(canonical),
      n_rejected      = .status_count("rejected"),
      n_unsure        = .status_count("unsure"),
      n_model_only    = .status_count("model_reviewed"),
      n_unreviewed    = .status_count("unreviewed"),
      gates_passed    = gate_result$passed,
      gate_report     = gate_result$report
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
  compiled$canonical
}
