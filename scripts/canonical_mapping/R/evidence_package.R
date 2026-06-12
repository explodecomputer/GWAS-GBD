library(dplyr)
library(stringr)

# ── Issue 016: Mapping Evidence Package Export ─────────────────────────────
# Joins GBD condition context, ontology metadata, GWAS Catalog evidence, and
# candidate provenance into curator-ready review rows.

#' Build a curator-ready mapping evidence package.
#'
#' @param candidates Data frame from generate_deterministic_candidates()
#'   (optionally with embedding channel added). Columns: gbd_condition,
#'   ontology_id, channels, channel_details.
#' @param observed_term_universe Output of build_observed_term_universe().
#' @param gbd_context Output of build_gbd_condition_context().
#' @param ontology_metadata Data frame: ontology_id, label, prefix (optional),
#'   definition (optional), synonyms (optional), is_obsolete (optional).
#' @return Data frame with one row per candidate mapping decision, containing
#'   all review context plus empty model and human review fields.
build_evidence_package <- function(candidates,
                                   observed_term_universe,
                                   gbd_context,
                                   ontology_metadata = NULL) {
  obs_terms <- observed_term_universe$terms %>%
    select(
      ontology_id,
      catalog_release,
      pubmed_count,
      association_count,
      example_trait_labels,
      example_pubmed_ids
    )

  cond_cols <- c(
    "condition_name", "gbd_level", "parent_name", "siblings", "n_siblings",
    "is_residual", "alias", "scope_note"
  )
  cond_cols <- intersect(cond_cols, names(gbd_context))
  cond_info <- gbd_context %>%
    select(all_of(cond_cols)) %>%
    rename(gbd_condition = condition_name)

  pkg <- candidates %>%
    left_join(cond_info,  by = "gbd_condition") %>%
    left_join(obs_terms,  by = "ontology_id")

  if (!is.null(ontology_metadata) && nrow(ontology_metadata) > 0) {
    meta_cols <- intersect(
      c("ontology_id", "label", "prefix", "definition", "synonyms", "is_obsolete"),
      names(ontology_metadata)
    )
    ont_info <- ontology_metadata %>% select(all_of(meta_cols))
    pkg <- pkg %>% left_join(ont_info, by = "ontology_id")
  } else {
    pkg$label       <- NA_character_
    pkg$is_obsolete <- NA
  }

  # Add empty review fields
  pkg <- pkg %>%
    mutate(
      model_recommendation  = NA_character_,
      model_relationship    = NA_character_,
      model_rationale       = NA_character_,
      model_confidence      = NA_real_,
      human_decision        = NA_character_,
      human_relationship    = NA_character_,
      human_notes           = NA_character_,
      reviewer_id           = NA_character_,
      review_date           = NA_character_
    )

  # Row identity: deterministic key for each decision
  pkg <- pkg %>%
    mutate(
      row_id = paste(
        gbd_condition,
        ontology_id,
        sep = "||"
      )
    )

  pkg %>%
    arrange(gbd_condition, ontology_id) %>%
    select(
      row_id,
      gbd_condition,
      any_of(c("gbd_level", "parent_name", "siblings", "n_siblings",
               "is_residual", "alias", "scope_note")),
      ontology_id,
      any_of(c("label", "prefix", "definition", "synonyms", "is_obsolete")),
      any_of(c("catalog_release", "pubmed_count", "association_count",
               "example_trait_labels", "example_pubmed_ids")),
      channels,
      channel_details,
      model_recommendation, model_relationship, model_rationale, model_confidence,
      human_decision, human_relationship, human_notes, reviewer_id, review_date
    )
}

#' Return the distinct GBD conditions in an evidence package.
evidence_conditions <- function(pkg) {
  sort(unique(pkg$gbd_condition))
}

#' Return a subset of the evidence package grouped by one condition.
filter_by_condition <- function(pkg, condition_name) {
  pkg %>% filter(gbd_condition == condition_name)
}
