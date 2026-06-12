library(dplyr)
library(stringr)

# ── Issue 017: Mapping Quality Gates ──────────────────────────────────────
# Hard invariant checks that must pass before canonical mappings can be used
# for default GWAS attention scoring. Diagnostic alignment checks are handled
# separately in diagnostic_report.R.

# ── Individual gate checks ─────────────────────────────────────────────────

.check_observed_only <- function(canonical, observed_ids) {
  unobserved <- canonical %>%
    filter(!.data$ontology_id %in% .env$observed_ids)
  if (nrow(unobserved) == 0) return(NULL)
  list(
    gate    = "observed_only",
    status  = "FAIL",
    message = paste(
      nrow(unobserved),
      "accepted mapping(s) point to ontology terms not observed in the catalog release:",
      paste(head(unique(unobserved$ontology_id), 10), collapse = ", ")
    ),
    rows    = unobserved
  )
}

.check_no_duplicate_edges <- function(canonical) {
  dups <- canonical %>%
    group_by(gbd_condition, ontology_id) %>%
    filter(n() > 1) %>%
    ungroup()
  if (nrow(dups) == 0) return(NULL)
  list(
    gate    = "no_duplicate_edges",
    status  = "FAIL",
    message = paste(
      nrow(dups),
      "duplicate accepted (GBD condition × ontology term) edge(s):",
      paste(
        head(unique(paste(dups$gbd_condition, dups$ontology_id, sep = " × ")), 10),
        collapse = ", "
      )
    ),
    rows    = dups
  )
}

.check_residual_exclusivity <- function(canonical, gbd_context) {
  residuals <- gbd_context %>%
    filter(is_residual) %>%
    pull(condition_name)

  if (length(residuals) == 0) return(NULL)

  # For each residual, find its siblings
  violations <- lapply(residuals, function(res) {
    parent <- gbd_context %>%
      filter(condition_name == res) %>%
      pull(parent_name)
    siblings <- gbd_context %>%
      filter(parent_name == parent, !is_residual, condition_name != res) %>%
      pull(condition_name)

    if (length(siblings) == 0) return(NULL)

    sibling_accepted <- canonical %>%
      filter(gbd_condition %in% siblings) %>%
      pull(ontology_id) %>%
      unique()

    residual_accepted <- canonical %>%
      filter(gbd_condition == res) %>%
      pull(ontology_id)

    overlap <- intersect(residual_accepted, sibling_accepted)
    if (length(overlap) == 0) return(NULL)

    data.frame(
      residual_condition = res,
      ontology_id        = overlap,
      stringsAsFactors   = FALSE
    )
  })

  viol <- bind_rows(Filter(Negate(is.null), violations))
  if (nrow(viol) == 0) return(NULL)
  list(
    gate    = "residual_exclusivity",
    status  = "FAIL",
    message = paste(
      nrow(viol),
      "residual condition(s) accept term(s) already accepted by specific siblings:",
      paste(head(paste(viol$residual_condition, viol$ontology_id, sep = " × "), 10),
            collapse = ", ")
    ),
    rows    = viol
  )
}

.check_specificity_conflicts <- function(canonical, gbd_context) {
  # A specificity conflict: a parent/broader condition accepts a term that a
  # more specific child already accepts.
  child_accepted <- canonical %>%
    inner_join(
      gbd_context %>% select(condition_name, parent_name),
      by = c("gbd_condition" = "condition_name")
    ) %>%
    rename(child_condition = gbd_condition)

  parent_accepted <- canonical %>%
    rename(parent_condition = gbd_condition)

  conflicts <- child_accepted %>%
    inner_join(
      parent_accepted,
      by = c("parent_name" = "parent_condition", "ontology_id"),
      relationship = "many-to-many"
    ) %>%
    select(parent_condition = parent_name, child_condition, ontology_id) %>%
    distinct()

  if (nrow(conflicts) == 0) return(NULL)
  list(
    gate    = "specificity_conflicts",
    status  = "WARN",
    message = paste(
      nrow(conflicts),
      "term(s) accepted for both a parent and a more specific child condition",
      "(child wins by default, parent should use hierarchy rollup):",
      paste(head(paste(conflicts$parent_condition, conflicts$ontology_id, sep = " × "), 10),
            collapse = ", ")
    ),
    rows    = conflicts
  )
}

.check_sentinel_non_zero <- function(canonical, sentinel_conditions) {
  if (length(sentinel_conditions) == 0) return(NULL)

  mapped <- canonical %>%
    filter(gbd_condition %in% sentinel_conditions) %>%
    pull(gbd_condition) %>%
    unique()

  zero_sentinels <- setdiff(sentinel_conditions, mapped)
  if (length(zero_sentinels) == 0) return(NULL)
  list(
    gate    = "sentinel_non_zero",
    status  = "FAIL",
    message = paste(
      length(zero_sentinels),
      "known non-zero sentinel condition(s) have no accepted mappings:",
      paste(zero_sentinels, collapse = ", ")
    ),
    rows    = data.frame(gbd_condition = zero_sentinels, stringsAsFactors = FALSE)
  )
}

.check_required_fields <- function(canonical) {
  required <- c("gbd_condition", "ontology_id")
  missing_fields <- setdiff(required, names(canonical))
  if (length(missing_fields) > 0) {
    return(list(
      gate    = "required_fields",
      status  = "FAIL",
      message = paste("Canonical mapping missing required columns:",
                      paste(missing_fields, collapse = ", ")),
      rows    = data.frame()
    ))
  }

  bad_rows <- canonical %>%
    filter(is.na(gbd_condition) | gbd_condition == "" |
             is.na(ontology_id) | ontology_id == "")
  if (nrow(bad_rows) == 0) return(NULL)
  list(
    gate    = "required_fields",
    status  = "FAIL",
    message = paste(nrow(bad_rows), "row(s) with missing gbd_condition or ontology_id"),
    rows    = bad_rows
  )
}

# ── Main entry point ───────────────────────────────────────────────────────

#' Run all mapping quality gates.
#'
#' @param canonical Data frame of accepted mappings: gbd_condition, ontology_id.
#' @param observed_term_universe Output of build_observed_term_universe().
#' @param gbd_context Output of build_gbd_condition_context().
#' @param sentinel_conditions Character vector of condition names that must
#'   have at least one accepted mapping.
#' @return A list:
#'   - `passed`: logical, TRUE if all FAIL gates passed.
#'   - `results`: list of gate result objects (NULL = passed, list = issue found)
#'   - `report`: character, human-readable summary
run_mapping_quality_gates <- function(canonical,
                                       observed_term_universe,
                                       gbd_context,
                                       sentinel_conditions = character(0)) {
  obs_ids <- observed_term_universe$terms$ontology_id

  req_check <- .check_required_fields(canonical)

  # If required fields are missing, skip gates that depend on column presence
  gate_results <- if (!is.null(req_check) && req_check$status == "FAIL") {
    list(
      required_fields      = req_check,
      observed_only        = NULL,
      no_duplicate_edges   = NULL,
      residual_exclusivity = NULL,
      specificity_conflicts = NULL,
      sentinel_non_zero    = NULL
    )
  } else {
    list(
      required_fields      = req_check,
      observed_only        = .check_observed_only(canonical, obs_ids),
      no_duplicate_edges   = .check_no_duplicate_edges(canonical),
      residual_exclusivity = .check_residual_exclusivity(canonical, gbd_context),
      specificity_conflicts = .check_specificity_conflicts(canonical, gbd_context),
      sentinel_non_zero    = .check_sentinel_non_zero(canonical, sentinel_conditions)
    )
  }

  issues <- Filter(Negate(is.null), gate_results)

  fail_issues <- Filter(function(x) x$status == "FAIL", issues)
  warn_issues <- Filter(function(x) x$status == "WARN", issues)

  report_lines <- c(
    paste("Canonical mapping quality gate report"),
    paste("  Accepted rows:", nrow(canonical)),
    paste("  Observed term universe size:", length(obs_ids)),
    "",
    if (length(fail_issues) == 0 && length(warn_issues) == 0)
      "  All gates PASSED"
    else {
      c(
        if (length(fail_issues) > 0)
          c("  FAILURES:", vapply(fail_issues, function(x)
            paste("    FAIL [", x$gate, "]", x$message), character(1))),
        if (length(warn_issues) > 0)
          c("  WARNINGS:", vapply(warn_issues, function(x)
            paste("    WARN [", x$gate, "]", x$message), character(1)))
      )
    }
  )

  list(
    passed  = length(fail_issues) == 0,
    results = gate_results,
    report  = paste(report_lines, collapse = "\n")
  )
}
