library(dplyr)
library(jsonlite)

# ── Issue 018: LLM Review Adapter ─────────────────────────────────────────
# Formats evidence package rows into structured review prompts and parses
# structured model responses into recommendation artifacts.
# Model output is NEVER canonical without explicit human review status.

RELATIONSHIP_LABELS <- c(
  "exact",
  "subtype",
  "broader_grouping",
  "narrower_grouping",
  "association_only",
  "unrelated"
)

DECISION_LABELS <- c("accept", "reject", "unsure")

# ── Prompt building ────────────────────────────────────────────────────────

.format_evidence_row <- function(row) {
  lines <- c(
    "## Candidate mapping for review",
    "",
    "### GBD condition",
    paste("Name:", row$gbd_condition),
    if (!is.na(row$parent_name))   paste("Parent:", row$parent_name),
    if (!is.na(row$gbd_level))     paste("Level:", row$gbd_level),
    if (!is.na(row$is_residual))   paste("Residual category:", row$is_residual),
    if (!is.na(row$siblings) && row$siblings != "")
      paste("Sibling conditions:", row$siblings),
    if (!is.na(row$alias) && row$alias != "")
      paste("Aliases:", row$alias),
    if (!is.na(row$scope_note) && row$scope_note != "")
      paste("Scope note:", row$scope_note),
    "",
    "### Ontology term",
    paste("ID:", row$ontology_id),
    if (!is.na(row$label))      paste("Label:", row$label),
    if ("prefix" %in% names(row) && !is.na(row$prefix))
      paste("Prefix:", row$prefix),
    if ("definition" %in% names(row) && !is.na(row$definition))
      paste("Definition:", row$definition),
    if ("synonyms" %in% names(row) && !is.na(row$synonyms))
      paste("Synonyms:", row$synonyms),
    if ("is_obsolete" %in% names(row) && isTRUE(row$is_obsolete))
      "Status: OBSOLETE",
    "",
    "### GWAS Catalog evidence",
    if (!is.na(row$pubmed_count))
      paste("Publications:", row$pubmed_count),
    if (!is.na(row$association_count))
      paste("Associations:", row$association_count),
    if (!is.na(row$example_trait_labels))
      paste("Example trait labels:", row$example_trait_labels),
    if (!is.na(row$example_pubmed_ids))
      paste("Example PubMed IDs:", row$example_pubmed_ids),
    "",
    "### Candidate provenance",
    paste("Channels:", row$channels),
    if (!is.na(row$channel_details))
      paste("Details:", row$channel_details)
  )
  paste(lines[!vapply(lines, is.null, logical(1))], collapse = "\n")
}

.llm_system_prompt <- function() {
  paste(
    "You are a biomedical ontology curator reviewing candidate mappings between",
    "GBD (Global Burden of Disease) conditions and observed GWAS Catalog ontology terms.",
    "",
    "For each candidate, decide:",
    "  - accept: the ontology term denotes the GBD condition, a named subtype of it,",
    "    or a deliberately accepted broader/narrower clinical grouping.",
    "  - reject: the term is unrelated, overly broad, or only associated (not the condition).",
    "  - unsure: the evidence is ambiguous; a human should decide.",
    "",
    "A term should NOT be accepted solely because of:",
    "  - ontology proximity or string containment",
    "  - comorbidity or statistical association",
    "  - it being a common co-occurring trait",
    "",
    "Your response MUST be valid JSON with these exact keys:",
    "  recommendation: one of 'accept', 'reject', 'unsure'",
    "  relationship_label: one of 'exact', 'subtype', 'broader_grouping',",
    "    'narrower_grouping', 'association_only', 'unrelated'",
    "  rationale: a 1-3 sentence plain-English explanation",
    "  confidence: a number 0.0 to 1.0 (your certainty in your recommendation)",
    sep = "\n"
  )
}

#' Format a single evidence package row as a structured LLM review prompt.
#'
#' @param evidence_row One-row data frame from build_evidence_package().
#' @return List with `system` and `user` fields suitable for Claude messages API.
format_llm_review_prompt <- function(evidence_row) {
  stopifnot(nrow(evidence_row) == 1)
  list(
    system = .llm_system_prompt(),
    user   = .format_evidence_row(evidence_row)
  )
}

# ── Response parsing ───────────────────────────────────────────────────────

EMPTY_REVIEW_RECOMMENDATION <- list(
  recommendation    = NA_character_,
  relationship_label = NA_character_,
  rationale          = NA_character_,
  confidence         = NA_real_,
  parse_error        = NA_character_
)

#' Parse a structured LLM response text into a recommendation record.
#'
#' @param response_text Character string; expected to be valid JSON.
#' @return Named list with recommendation, relationship_label, rationale,
#'   confidence, and parse_error (NA if parsed successfully).
parse_llm_review_response <- function(response_text) {
  if (is.null(response_text) || is.na(response_text) ||
      trimws(response_text) == "") {
    result <- EMPTY_REVIEW_RECOMMENDATION
    result$parse_error <- "empty_response"
    return(result)
  }

  # Extract JSON block if wrapped in markdown code fence
  json_text <- response_text
  md_match  <- regmatches(json_text, regexpr("```json\\s*(\\{.*?\\})\\s*```",
                                             json_text, perl = TRUE))
  if (length(md_match) > 0) {
    json_text <- gsub("```json\\s*|\\s*```", "", md_match[[1]], perl = TRUE)
  } else {
    bare_match <- regmatches(json_text, regexpr("\\{[^{}]+\\}", json_text, perl = TRUE))
    if (length(bare_match) > 0) json_text <- bare_match[[1]]
  }

  parsed <- tryCatch(
    jsonlite::fromJSON(json_text, simplifyVector = TRUE),
    error = function(e) NULL
  )

  if (is.null(parsed)) {
    result <- EMPTY_REVIEW_RECOMMENDATION
    result$parse_error <- paste("json_parse_failed:", substr(response_text, 1, 80))
    return(result)
  }

  rec <- as.character(parsed[["recommendation"]])
  rel <- as.character(parsed[["relationship_label"]])
  rat <- as.character(parsed[["rationale"]])
  con <- suppressWarnings(as.numeric(parsed[["confidence"]]))

  errors <- character(0)
  if (!rec %in% DECISION_LABELS)
    errors <- c(errors, paste("invalid_recommendation:", rec))
  if (!rel %in% RELATIONSHIP_LABELS)
    errors <- c(errors, paste("invalid_relationship:", rel))
  if (is.na(con) || con < 0 || con > 1)
    errors <- c(errors, paste("invalid_confidence:", con))

  list(
    recommendation     = rec,
    relationship_label = rel,
    rationale          = rat,
    confidence         = con,
    parse_error        = if (length(errors) > 0) paste(errors, collapse = "; ") else NA_character_
  )
}

#' Apply LLM review recommendations back to an evidence package.
#'
#' @param pkg Evidence package data frame.
#' @param recommendations Named list (key = row_id) of parse_llm_review_response outputs.
#' @return pkg with model_recommendation, model_relationship, model_rationale,
#'   model_confidence filled in.
apply_llm_recommendations <- function(pkg, recommendations) {
  for (rid in names(recommendations)) {
    rec <- recommendations[[rid]]
    idx <- which(pkg$row_id == rid)
    if (length(idx) == 0) next
    pkg$model_recommendation[idx]  <- rec$recommendation
    pkg$model_relationship[idx]    <- rec$relationship_label
    pkg$model_rationale[idx]       <- rec$rationale
    pkg$model_confidence[idx]      <- rec$confidence
  }
  pkg
}
