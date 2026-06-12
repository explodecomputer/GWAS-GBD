library(dplyr)

# ── Issue 022: Catalog Release Drift Report ───────────────────────────────
# Compares observed ontology term universes and accepted mappings across GWAS
# Catalog releases, identifying what can be carried forward and what needs review.

#' Build a release-drift report between two GWAS Catalog releases.
#'
#' @param old_universe Output of build_observed_term_universe() for the old release.
#' @param new_universe Output of build_observed_term_universe() for the new release.
#' @param old_canonical Optional canonical mapping from the old release
#'   (data frame: gbd_condition, ontology_id). Used to classify carry-forward status.
#' @param obsolete_replacements Optional data frame from .parse_obo_obsolete_replacements()
#'   for the new EFO OBO file.
#' @return A list with term-level and mapping-level drift tables.
generate_drift_report <- function(old_universe,
                                   new_universe,
                                   old_canonical         = NULL,
                                   obsolete_replacements = NULL) {
  old_ids <- old_universe$terms$ontology_id
  new_ids <- new_universe$terms$ontology_id

  # -- Term-level drift
  term_drift <- bind_rows(
    tibble(ontology_id = intersect(old_ids, new_ids), term_status = "retained"),
    tibble(ontology_id = setdiff(new_ids, old_ids),   term_status = "new"),
    tibble(ontology_id = setdiff(old_ids, new_ids),   term_status = "dropped")
  )

  # Annotate obsolete status (dropped terms that are obsolete in new OBO)
  if (!is.null(obsolete_replacements) && nrow(obsolete_replacements) > 0) {
    obs_norm <- obsolete_replacements %>%
      mutate(
        source_norm = toupper(gsub("^efo:", "", source_id, ignore.case = TRUE)),
        source_norm = gsub("_", ":", source_norm),
        replace_norm = toupper(gsub("^efo:", "", replacement_id, ignore.case = TRUE)),
        replace_norm = gsub("_", ":", replace_norm)
      ) %>%
      select(source_norm, replace_norm, replacement_type)

    term_drift <- term_drift %>%
      left_join(obs_norm, by = c("ontology_id" = "source_norm"),
                relationship = "many-to-many") %>%
      mutate(
        term_status = case_when(
          term_status == "dropped" & !is.na(replace_norm) & replace_norm %in% new_ids
          ~ "replaced",
          term_status == "dropped" & !is.na(replace_norm) ~ "obsolete_no_replacement",
          TRUE ~ term_status
        )
      ) %>%
      select(ontology_id, term_status, replace_norm, replacement_type) %>%
      rename(replacement_id = replace_norm) %>%
      distinct()
  } else {
    term_drift$replacement_id   <- NA_character_
    term_drift$replacement_type <- NA_character_
  }

  # -- Mapping-level drift (if old canonical provided)
  mapping_drift <- NULL
  if (!is.null(old_canonical) && nrow(old_canonical) > 0) {
    mapping_drift <- old_canonical %>%
      distinct(gbd_condition, ontology_id) %>%
      left_join(term_drift, by = "ontology_id") %>%
      mutate(
        carry_forward_status = case_when(
          term_status == "retained"             ~ "carry_forward",
          term_status == "replaced"             ~ "review_replacement",
          term_status %in% c("dropped",
                             "obsolete_no_replacement") ~ "needs_review",
          TRUE ~ "unknown"
        )
      )
  }

  # -- Human-readable summary
  term_summary <- term_drift %>%
    count(term_status, name = "n") %>%
    tibble::deframe()

  mapping_summary <- if (!is.null(mapping_drift)) {
    mapping_drift %>%
      count(carry_forward_status, name = "n") %>%
      tibble::deframe()
  } else {
    NULL
  }

  report_lines <- c(
    "Catalog Release Drift Report",
    "==============================",
    sprintf("  Old release: %s (%d terms)",
            coalesce(old_universe$metadata$catalog_release, "unknown"),
            length(old_ids)),
    sprintf("  New release: %s (%d terms)",
            coalesce(new_universe$metadata$catalog_release, "unknown"),
            length(new_ids)),
    "",
    "  Term status:",
    vapply(names(term_summary), function(k)
      sprintf("    %s: %d", k, term_summary[[k]]), character(1))
  )

  if (!is.null(mapping_summary)) {
    report_lines <- c(report_lines, "",
      "  Accepted mapping carry-forward:",
      vapply(names(mapping_summary), function(k)
        sprintf("    %s: %d", k, mapping_summary[[k]]), character(1))
    )
  }

  list(
    term_drift     = term_drift,
    mapping_drift  = mapping_drift,
    term_summary   = term_summary,
    mapping_summary = mapping_summary,
    report         = paste(report_lines, collapse = "\n")
  )
}
