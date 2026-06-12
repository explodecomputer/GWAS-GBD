library(dplyr)
library(tidyr)

# ── Issue 021: Diagnostic Mapping QC Report ────────────────────────────────
# Compares canonical attention outputs against reference datasets and expected
# scientific patterns. All checks here are DIAGNOSTIC, not hard gates.

#' Compute condition-level attention deltas between canonical and reference.
#'
#' @param canonical_scores Output of score_canonical_attention().
#' @param reference_scores Data frame: gbd_condition (or GBD.term / cause_name),
#'   total_attention_score.
#' @return Data frame: gbd_condition, canonical_score, reference_score, delta,
#'   status (rescued / lost / increased / decreased / unchanged / new_zero).
compare_with_reference <- function(canonical_scores, reference_scores) {
  # Normalise reference column name
  ref_col <- intersect(c("gbd_condition", "GBD.term", "cause_name"),
                       names(reference_scores))[1]
  if (is.na(ref_col)) stop("Reference scores missing condition name column")

  ref <- reference_scores %>%
    rename(gbd_condition = all_of(ref_col)) %>%
    select(gbd_condition, reference_score = total_attention_score)

  merged <- canonical_scores %>%
    select(gbd_condition, canonical_score = total_attention_score) %>%
    full_join(ref, by = "gbd_condition") %>%
    mutate(
      canonical_score = coalesce(canonical_score, 0),
      reference_score = coalesce(reference_score, 0),
      delta           = canonical_score - reference_score,
      status = case_when(
        reference_score == 0 & canonical_score >  0 ~ "rescued",
        reference_score >  0 & canonical_score == 0 ~ "lost",
        canonical_score >  reference_score          ~ "increased",
        canonical_score <  reference_score          ~ "decreased",
        canonical_score == reference_score & canonical_score > 0 ~ "unchanged",
        canonical_score == 0 & reference_score == 0             ~ "both_zero",
        TRUE                                                     ~ "other"
      )
    )

  merged
}

#' Detect broad-term dominance: terms that account for a disproportionate
#' share of total attention, which may indicate over-broad mappings.
#'
#' @param canonical Data frame: gbd_condition, ontology_id.
#' @param scores Output of score_canonical_attention().
#' @param top_n Number of top-scoring conditions to examine.
#' @param dominance_threshold Fraction of total attention a single condition
#'   may hold before flagging (default 0.25).
#' @return List with `dominant_conditions` data frame and `total_attention`.
broad_term_inflation_diagnostic <- function(canonical, scores,
                                             top_n = 20L,
                                             dominance_threshold = 0.25) {
  n_terms_per_cond <- canonical %>%
    count(gbd_condition, name = "n_mapped_terms")

  top_scores <- scores %>%
    arrange(desc(total_attention_score)) %>%
    head(top_n) %>%
    left_join(n_terms_per_cond, by = "gbd_condition") %>%
    mutate(
      attention_share = total_attention_score / sum(scores$total_attention_score),
      is_dominant     = attention_share >= dominance_threshold
    )

  list(
    dominant_conditions = top_scores %>% filter(is_dominant),
    top_conditions      = top_scores,
    total_attention     = sum(scores$total_attention_score)
  )
}

#' Compute concentration-index-like time trend for diagnostic purposes.
#' Returns mean attention per condition per year.
#'
#' @param temporal_scores Output of build_canonical_temporal_scores().
#' @param analysis_type One of "year" or "sliding_3yr".
#' @return Data frame: time_strata, n_conditions_with_attention,
#'   mean_attention_per_condition.
concentration_index_trend <- function(temporal_scores,
                                       analysis_type = "sliding_3yr") {
  temporal_scores %>%
    filter(analysis_type == .env$analysis_type) %>%
    group_by(time_strata) %>%
    summarise(
      n_conditions = n_distinct(gbd_condition),
      total_attention = sum(total_attention_score, na.rm = TRUE),
      mean_attention  = mean(total_attention_score, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(time_strata)
}

#' Generate a full diagnostic report.
#'
#' @param canonical Data frame: gbd_condition, ontology_id.
#' @param canonical_scores Output of score_canonical_attention().
#' @param temporal_scores Output of build_canonical_temporal_scores() (optional).
#' @param reference_scores Reference dataset (optional, e.g. _updated6 scores).
#' @param sdi_scores Named list of SDI-group attention scores for SDI gradient
#'   check (optional). Names should be SDI group labels.
#' @return List of diagnostic sections.
generate_diagnostic_report <- function(canonical,
                                        canonical_scores,
                                        temporal_scores   = NULL,
                                        reference_scores  = NULL,
                                        sdi_scores        = NULL) {
  report <- list()

  # -- Reference comparison
  if (!is.null(reference_scores)) {
    report$reference_comparison <- compare_with_reference(
      canonical_scores, reference_scores
    )
    rescued <- report$reference_comparison %>%
      filter(status == "rescued") %>%
      pull(gbd_condition)
    lost <- report$reference_comparison %>%
      filter(status == "lost") %>%
      pull(gbd_condition)
    report$rescued_conditions <- rescued
    report$lost_conditions    <- lost
  }

  # -- Zero-attention summary
  report$zero_attention <- canonical_scores %>%
    filter(zero_attention | total_attention_score == 0) %>%
    pull(gbd_condition)

  # -- Broad-term inflation
  report$broad_term_inflation <- broad_term_inflation_diagnostic(
    canonical, canonical_scores
  )

  # -- Temporal trend
  if (!is.null(temporal_scores) && nrow(temporal_scores) > 0) {
    report$temporal_trend <- concentration_index_trend(temporal_scores)
  }

  # -- SDI gradient check (diagnostic only)
  if (!is.null(sdi_scores) && length(sdi_scores) >= 2) {
    sdi_summary <- lapply(names(sdi_scores), function(grp) {
      s <- sdi_scores[[grp]]
      data.frame(
        sdi_group  = grp,
        n_nonzero  = sum(s$total_attention_score > 0, na.rm = TRUE),
        mean_score = mean(s$total_attention_score, na.rm = TRUE),
        stringsAsFactors = FALSE
      )
    })
    report$sdi_gradient <- bind_rows(sdi_summary)
  }

  # -- Summary counts
  report$summary <- list(
    n_conditions_total    = nrow(canonical_scores),
    n_with_attention      = sum(canonical_scores$total_attention_score > 0),
    n_zero_attention      = sum(canonical_scores$total_attention_score == 0),
    n_accepted_mappings   = nrow(canonical),
    n_distinct_terms      = n_distinct(canonical$ontology_id),
    n_distinct_conditions = n_distinct(canonical$gbd_condition)
  )

  class(report) <- c("diagnostic_report", "list")
  report
}

#' Print a human-readable summary of a diagnostic report.
print.diagnostic_report <- function(x, ...) {
  cat("Canonical Mapping Diagnostic Report\n")
  cat("=====================================\n")
  s <- x$summary
  cat(sprintf(
    "  Conditions: %d total, %d with attention, %d zero\n",
    s$n_conditions_total, s$n_with_attention, s$n_zero_attention
  ))
  cat(sprintf(
    "  Accepted mappings: %d (across %d conditions, %d distinct terms)\n",
    s$n_accepted_mappings, s$n_distinct_conditions, s$n_distinct_terms
  ))

  if (!is.null(x$rescued_conditions))
    cat(sprintf("  Rescued from zero: %d conditions\n",
                length(x$rescued_conditions)))
  if (!is.null(x$lost_conditions))
    cat(sprintf("  Lost attention vs reference: %d conditions\n",
                length(x$lost_conditions)))

  infl <- x$broad_term_inflation
  if (!is.null(infl) && nrow(infl$dominant_conditions) > 0) {
    cat(sprintf("  WARN: %d dominant condition(s) hold >=25%% of total attention\n",
                nrow(infl$dominant_conditions)))
  }

  invisible(x)
}
