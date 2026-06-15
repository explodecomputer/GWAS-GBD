#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(curl)
  library(data.table)
  library(dplyr)
  library(jsonlite)
  library(stringr)
})

DATA_DIR <- "Data"
PIPELINE_OUTPUT <- file.path(DATA_DIR, "merged_dataset_exclude_Injuries.csv")
UNEXPECTED_MISSING <- file.path(DATA_DIR, "unexpected_missing_terms_for_manual_mapping.tsv")

OLS_SEARCH_URL <- "https://www.ebi.ac.uk/ols4/api/search"
OLS_ONTOLOGY_FILTER <- "efo"
OLS_ROWS <- 25

OUT_SEARCH_TERMS <- file.path(DATA_DIR, "ols_search_friendly_terms.tsv")
OUT_SEARCH_RESULTS <- file.path(DATA_DIR, sprintf("ols_search_top%d_results.tsv", OLS_ROWS))
OUT_CAUSE_CANDIDATES <- file.path(DATA_DIR, "ols_candidate_terms_for_manual_review.tsv")
CACHE_DIR <- file.path(DATA_DIR, "ols_search_cache")

clean_space <- function(x) {
  str_squish(as.character(x))
}

first_nonempty <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) NA_character_ else x[[1]]
}

collapse_nonempty <- function(x) {
  x <- unique(x[!is.na(x) & x != ""])
  paste(x, collapse = " | ")
}

normalize_search_term <- function(x) {
  x <- clean_space(x)
  x <- str_replace_all(x, "[[:punct:]]+", " ")
  str_squish(x)
}

dedupe_nonempty <- function(x) {
  x <- clean_space(x)
  x <- x[!is.na(x) & x != "" & x != "NA"]
  unique(x)
}

strip_leading_other <- function(x) {
  str_replace(x, regex("^other\\s+", ignore_case = TRUE), "")
}

drop_total_prefix <- function(x) {
  str_replace(x, regex("^total\\s+", ignore_case = TRUE), "")
}

expand_abbreviations <- function(x) {
  y <- x
  y <- str_replace_all(y, regex("\\bCKD\\b", ignore_case = TRUE), "chronic kidney disease")
  y <- str_replace_all(y, regex("\\bUTI\\b", ignore_case = TRUE), "urinary tract infection")
  y <- str_replace_all(y, regex("\\bNASH\\b", ignore_case = TRUE), "nonalcoholic steatohepatitis")
  y <- str_replace_all(y, regex("\\bNAFLD\\b", ignore_case = TRUE), "nonalcoholic fatty liver disease")
  y
}

make_search_terms_one <- function(cause_name) {
  cause_name <- clean_space(cause_name)
  normalized <- normalize_search_term(cause_name)
  no_other <- normalize_search_term(strip_leading_other(cause_name))
  no_total <- normalize_search_term(drop_total_prefix(cause_name))
  expanded <- normalize_search_term(expand_abbreviations(cause_name))

  terms <- tibble(
    search_term = c(cause_name, normalized, no_other, no_total, expanded),
    search_term_source = c("original", "normalized", "without_leading_other",
                           "without_total_prefix", "expanded_abbreviations")
  )

  if (str_detect(cause_name, regex("\\bdue to\\b", ignore_case = TRUE))) {
    parts <- str_split(cause_name, regex("\\bdue to\\b", ignore_case = TRUE), n = 2)[[1]]
    terms <- bind_rows(
      terms,
      tibble(
        search_term = normalize_search_term(c(parts[1], parts[2], paste(parts[1], parts[2]))),
        search_term_source = c("due_to_base", "due_to_cause", "due_to_combined")
      )
    )
  }

  if (str_detect(cause_name, regex("\\bincluding\\b", ignore_case = TRUE))) {
    parts <- str_split(cause_name, regex("\\bincluding\\b", ignore_case = TRUE), n = 2)[[1]]
    terms <- bind_rows(
      terms,
      tibble(
        search_term = normalize_search_term(c(parts[1], parts[2])),
        search_term_source = c("including_base", "including_component")
      )
    )
  }

  if (str_detect(cause_name, regex("\\bexcluding\\b", ignore_case = TRUE))) {
    parts <- str_split(cause_name, regex("\\bexcluding\\b", ignore_case = TRUE), n = 2)[[1]]
    terms <- bind_rows(
      terms,
      tibble(
        search_term = normalize_search_term(parts[1]),
        search_term_source = "excluding_base"
      )
    )
  }

  if (str_detect(cause_name, regex("\\band other\\b", ignore_case = TRUE))) {
    before_other <- normalize_search_term(str_replace(
      cause_name,
      regex("\\band other\\b.*$", ignore_case = TRUE),
      ""
    ))
    generic_before_other <- str_detect(
      before_other,
      regex("^(age related|other|total)$", ignore_case = TRUE)
    )

    if (!is.na(before_other) && before_other != "" && !generic_before_other) {
    terms <- bind_rows(
      terms,
      tibble(
          search_term = before_other,
        search_term_source = "before_and_other"
      )
    )
    }
  }

  # A few GBD labels benefit from medical names that OLS/GWAS Catalog use directly.
  synonym_terms <- case_when(
    cause_name == "Age-related and other hearing loss" ~ "hearing loss disorder; presbycusis",
    cause_name == "Nonalcoholic fatty liver disease including cirrhosis" ~ "nonalcoholic fatty liver disease; nonalcoholic steatohepatitis",
    cause_name == "Liver cancer due to NASH" ~ "nonalcoholic steatohepatitis; hepatocellular carcinoma",
    cause_name == "Urinary tract infections and interstitial nephritis" ~ "urinary tract infection; interstitial nephritis",
    TRUE ~ NA_character_
  )

  if (!is.na(synonym_terms)) {
    terms <- bind_rows(
      terms,
      tibble(
        search_term = normalize_search_term(unlist(str_split(synonym_terms, ";"))),
        search_term_source = "curated_synonym"
      )
    )
  }

  terms %>%
    mutate(
      cause_name = cause_name,
      search_term = clean_space(search_term)
    ) %>%
    filter(!is.na(search_term), search_term != "") %>%
    distinct(cause_name, search_term, .keep_all = TRUE) %>%
    select(cause_name, search_term, search_term_source)
}

cache_key <- function(query, ontology, rows) {
  raw <- charToRaw(enc2utf8(paste(query, ontology, rows, sep = "\n")))
  paste(sprintf("%02x", as.integer(raw)), collapse = "")
}

normalize_for_rank <- function(x) {
  normalize_search_term(str_to_lower(x))
}

ols_search <- function(query, rows = OLS_ROWS, ontology = OLS_ONTOLOGY_FILTER) {
  dir.create(CACHE_DIR, showWarnings = FALSE, recursive = TRUE)
  cache_path <- file.path(CACHE_DIR, paste0(cache_key(query, ontology, rows), ".json"))

  if (file.exists(cache_path)) {
    txt <- paste(readLines(cache_path, warn = FALSE), collapse = "\n")
  } else {
    url <- paste0(
      OLS_SEARCH_URL,
      "?q=", utils::URLencode(query, reserved = TRUE),
      "&rows=", rows,
      "&ontology=", utils::URLencode(ontology, reserved = TRUE)
    )
    response <- curl_fetch_memory(url)
    txt <- rawToChar(response$content)
    writeLines(txt, cache_path)
    Sys.sleep(0.05)
  }

  parsed <- tryCatch(fromJSON(txt, simplifyVector = FALSE), error = function(e) NULL)
  docs <- parsed$response$docs
  if (is.null(docs) || length(docs) == 0) {
    return(tibble())
  }

  bind_rows(lapply(seq_along(docs), function(i) {
    doc <- docs[[i]]
    tibble(
      result_rank = i,
      result_label = doc$label %||% NA_character_,
      result_obo_id = doc$obo_id %||% NA_character_,
      result_iri = doc$iri %||% NA_character_,
      result_short_form = doc$short_form %||% NA_character_,
      result_ontology = doc$ontology_name %||% NA_character_,
      result_type = doc$type %||% NA_character_,
      result_score = as.numeric(doc$score %||% NA_real_),
      result_synonyms = paste(unlist(doc$synonym %||% character(0)), collapse = " | "),
      result_description = paste(unlist(doc$description %||% character(0)), collapse = " | ")
    )
  }))
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

main <- function() {
  if (!file.exists(PIPELINE_OUTPUT)) {
    stop("Missing pipeline output: ", PIPELINE_OUTPUT)
  }

  all_causes <- fread(PIPELINE_OUTPUT) %>%
    as_tibble() %>%
    filter(analysis_type == "all") %>%
    transmute(cause_name = clean_space(cause_name), cause_id = cause_id) %>%
    distinct()

  unexpected <- if (file.exists(UNEXPECTED_MISSING)) {
    fread(UNEXPECTED_MISSING) %>%
      as_tibble() %>%
      transmute(cause_name = clean_space(cause_name), is_unexpected_missing = TRUE) %>%
      distinct()
  } else {
    tibble(cause_name = character(), is_unexpected_missing = logical())
  }

  search_terms <- bind_rows(lapply(all_causes$cause_name, make_search_terms_one)) %>%
    left_join(all_causes, by = "cause_name") %>%
    left_join(unexpected, by = "cause_name") %>%
    mutate(is_unexpected_missing = coalesce(is_unexpected_missing, FALSE)) %>%
    arrange(desc(is_unexpected_missing), cause_name, search_term_source, search_term)

  fwrite(search_terms, OUT_SEARCH_TERMS, sep = "\t", na = "")

  message("Searching OLS for ", n_distinct(search_terms$search_term), " unique search terms...")
  message("Using OLS ontology filter: ", OLS_ONTOLOGY_FILTER, "; rows per search term: ", OLS_ROWS)

  results <- bind_rows(lapply(seq_len(nrow(search_terms)), function(i) {
    if (i %% 50 == 0) {
      message("  searched ", i, " / ", nrow(search_terms))
    }
    row <- search_terms[i, ]
    ols_search(row$search_term, rows = OLS_ROWS, ontology = OLS_ONTOLOGY_FILTER) %>%
      mutate(
        cause_name = row$cause_name,
        cause_id = row$cause_id,
        is_unexpected_missing = row$is_unexpected_missing,
        search_term = row$search_term,
        search_term_source = row$search_term_source,
        ols_ontology_filter = OLS_ONTOLOGY_FILTER,
        ols_rows_requested = OLS_ROWS,
        .before = 1
      )
  }))

  fwrite(results, OUT_SEARCH_RESULTS, sep = "\t", na = "")

  candidates <- results %>%
    filter(!is.na(result_iri), result_iri != "") %>%
    mutate(
      result_key = coalesce(result_obo_id, result_iri, result_short_form),
      normalized_search_term = normalize_for_rank(search_term),
      normalized_result_label = normalize_for_rank(result_label),
      exact_label_match = normalized_result_label == normalized_search_term,
      label_contains_search_term = str_detect(normalized_result_label, fixed(normalized_search_term)),
      search_term_contains_label = str_detect(normalized_search_term, fixed(normalized_result_label))
    ) %>%
    group_by(cause_name, cause_id, is_unexpected_missing,
             result_key, result_iri, result_obo_id, result_short_form, result_type) %>%
    summarise(
      ols_ontology_filter = paste(unique(ols_ontology_filter), collapse = " | "),
      ols_rows_requested = max(ols_rows_requested, na.rm = TRUE),
      best_rank = min(result_rank, na.rm = TRUE),
      best_score = {
        scores <- result_score[!is.na(result_score)]
        if (length(scores) == 0) NA_real_ else max(scores)
      },
      exact_label_match = any(exact_label_match, na.rm = TRUE),
      label_contains_search_term = any(label_contains_search_term, na.rm = TRUE),
      search_term_contains_label = any(search_term_contains_label, na.rm = TRUE),
      result_label = first_nonempty(result_label[order(result_rank, desc(result_score))]),
      result_ontology = collapse_nonempty(result_ontology),
      matched_search_terms = paste(unique(search_term), collapse = " | "),
      search_term_sources = paste(unique(search_term_source), collapse = " | "),
      result_synonyms = collapse_nonempty(result_synonyms),
      result_description = collapse_nonempty(result_description),
      .groups = "drop"
    ) %>%
    select(-result_key) %>%
    mutate(
      keep = "",
      manual_notes = ""
    ) %>%
    arrange(
      desc(is_unexpected_missing),
      cause_name,
      desc(exact_label_match),
      desc(label_contains_search_term),
      desc(search_term_contains_label),
      best_rank,
      desc(best_score),
      result_label
    )

  fwrite(candidates, OUT_CAUSE_CANDIDATES, sep = "\t", na = "")

  message("Wrote: ", OUT_SEARCH_TERMS, " (", nrow(search_terms), " rows)")
  message("Wrote: ", OUT_SEARCH_RESULTS, " (", nrow(results), " rows)")
  message("Wrote: ", OUT_CAUSE_CANDIDATES, " (", nrow(candidates), " rows)")
}

main()
