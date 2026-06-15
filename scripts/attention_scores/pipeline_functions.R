library(dplyr)
library(tidyr)
library(stringr)
library(here)
library(readxl)
library(ontologyIndex)
library(data.table)


# ── GBD universe derivation ───────────────────────────────────────────────
# Returns the canonical set of non-injury leaf terms at levels 3/4.
# Criteria: A/B cause outline only (no injuries); levels 3 and 4; level-3
# terms dropped if they are a parent of any other level-3/4 term (i.e. keep
# only the highest-resolution leaves).
derive_gbd_universe <- function(hierarchy_path) {
  sheet_names <- excel_sheets(hierarchy_path)
  cause_sheet <- grep("cause hierarchy", sheet_names, ignore.case = TRUE,
                      value = TRUE)[1]
  if (is.na(cause_sheet)) stop("No Cause Hierarchy sheet in: ", hierarchy_path)

  h <- read_xlsx(hierarchy_path, sheet = cause_sheet)
  names(h) <- trimws(names(h))

  cause_id_col   <- grep("^[Cc]ause.?[Ii][Dd]$",  names(h), value = TRUE)[1]
  cause_name_col <- grep("^[Cc]ause.?[Nn]ame$",    names(h), value = TRUE)[1]
  parent_col     <- grep("^[Pp]arent.?[Nn]ame$",   names(h), value = TRUE)[1]
  level_col      <- grep("^[Ll]evel$",              names(h), value = TRUE)[1]
  outline_col    <- grep("^[Cc]ause.?[Oo]utline$", names(h), value = TRUE)[1]

  h <- h %>%
    rename(
      cause_name  = all_of(cause_name_col),
      parent_name = all_of(parent_col),
      level       = all_of(level_col),
      outline     = all_of(outline_col)
    ) %>%
    mutate(
      cause_id = if (!is.na(cause_id_col)) .data[[cause_id_col]] else NA_character_,
      level    = suppressWarnings(as.integer(level))
    )

  h_sub <- h %>% filter(grepl("^[AB]", outline), level %in% c(3L, 4L))

  h_sub %>%
    filter(!cause_name %in% parent_name) %>%
    select(cause_name, cause_id, level) %>%
    arrange(cause_name)
}

# Returns all GBD cause names that are NOT in the universe — i.e. aggregate
# terms (levels 0-2) and injury terms (C/D outline at levels 3/4).
# Pass the already-derived universe to avoid re-reading the XLSX.
derive_excluded_causes <- function(hierarchy_path,
                                   universe = derive_gbd_universe(hierarchy_path)) {
  sheet_names <- excel_sheets(hierarchy_path)
  cause_sheet <- grep("cause hierarchy", sheet_names, ignore.case = TRUE,
                      value = TRUE)[1]
  h <- read_xlsx(hierarchy_path, sheet = cause_sheet)
  names(h) <- trimws(names(h))
  cause_name_col <- grep("^[Cc]ause.?[Nn]ame$", names(h), value = TRUE)[1]
  all_names <- unique(trimws(as.character(h[[cause_name_col]])))
  all_names <- all_names[nzchar(all_names) & !is.na(all_names)]
  setdiff(all_names, universe$cause_name)
}

# ── Internal URI normalisation ─────────────────────────────────────────────
# Strips prefix/namespace, lowercases, removes punctuation. Applied to both
# GWAS catalog URIs and GBD EFO columns so joins work across different formats.
.clean_uri <- function(uri) {
  uri <- tolower(trimws(as.character(uri)))
  uri <- gsub("\u00a0", "", uri, fixed = TRUE)
  uri <- gsub("\\s+|\"", "", uri)
  uri <- gsub("_", ":", uri)
  uri <- gsub("[:\\s]", "", uri)
  uri <- sub(".*[:/]", "", uri)
  uri <- gsub("[^[:alnum:]]", "", uri)
  uri
}

.parse_sample_cases_one <- function(x) {
  if (is.na(x) || is.null(x) || trimws(as.character(x)) == "") return(0)

  parts <- gsub("(\\d),(?=\\d)", "\\1", as.character(x), perl = TRUE) %>%
    strsplit(", ") %>%
    unlist()

  case_parts <- grep("cases", parts, value = TRUE, ignore.case = TRUE)
  if (length(case_parts) == 0) {
    case_parts <- parts
  }

  nums <- vapply(case_parts, function(y) {
    vals <- suppressWarnings(as.numeric(strsplit(y, " ")[[1]]))
    vals <- vals[!is.na(vals)]
    if (length(vals) == 0) 0 else vals[[1]]
  }, numeric(1))

  sum(nums, na.rm = TRUE)
}

.parse_sample_cases <- function(x) {
  vapply(x, .parse_sample_cases_one, numeric(1))
}

.normalise_trait_name <- function(x) {
  tolower(trimws(gsub("[^a-zA-Z0-9 ]", "", as.character(x))))
}

# Format mapped ontology IDs for ontologyIndex lookup.
# EFO's OBO parse uses ids like "efo:EFO_0000405"; most imported
# ontologies use regular OBO ids such as "MONDO:0002356" or "HP:0000010".
.format_efo_id <- function(id) {
  id <- trimws(as.character(id))
  id <- gsub("\\s+", "", id)
  id <- sub("^.*[/#]", "", id)

  if (grepl("^(efo:)?efo[:_][0-9]+$", id, ignore.case = TRUE)) {
    digits <- sub(".*[Ee][Ff][Oo][:_]", "", id)
    return(paste0("efo:EFO_", digits))
  }

  id <- gsub("_", ":", id)
  parts <- strsplit(id, ":", fixed = TRUE)[[1]]
  if (length(parts) < 2) return(id)

  prefix <- parts[[1]]
  local_id <- paste(parts[-1], collapse = ":")
  prefix <- if (tolower(prefix) == "orphanet") "Orphanet" else toupper(prefix)
  paste0(prefix, ":", local_id)
}

# Reverse ontologyIndex ids to compact identifiers that .clean_uri can align
# with GWAS catalog MAPPED_TRAIT_URI values.
.revert_efo_id <- function(id) {
  id <- as.character(id)
  id <- sub("^efo:", "", id, ignore.case = TRUE)
  id <- gsub("_", ":", id)
  id
}

.extract_obo_values <- function(stanza, tag) {
  pattern <- paste0("(?m)^", tag, ":\\s+(.+)$")
  matches <- stringr::str_match_all(stanza, pattern)[[1]]
  if (nrow(matches) == 0) character(0) else matches[, 2]
}

.parse_obo_obsolete_replacements <- function(efo_obo_path) {
  text <- paste(readLines(efo_obo_path, warn = FALSE), collapse = "\n")
  stanzas <- unlist(strsplit(text, "\n\\[Term\\]\n"))

  rows <- lapply(stanzas, function(stanza) {
    if (!grepl("(?m)^is_obsolete:\\s+true$", stanza, perl = TRUE)) {
      return(NULL)
    }

    source_id <- .extract_obo_values(stanza, "id")
    if (length(source_id) == 0) return(NULL)

    replaced_by <- .extract_obo_values(stanza, "replaced_by")
    consider <- .extract_obo_values(stanza, "consider")

    bind_rows(
      if (length(replaced_by) > 0) {
        data.frame(
          source_id = .format_efo_id(source_id[[1]]),
          replacement_id = vapply(replaced_by, .format_efo_id, character(1)),
          replacement_type = "replaced_by",
          stringsAsFactors = FALSE
        )
      },
      if (length(consider) > 0) {
        data.frame(
          source_id = .format_efo_id(source_id[[1]]),
          replacement_id = vapply(consider, .format_efo_id, character(1)),
          replacement_type = "consider",
          stringsAsFactors = FALSE
        )
      }
    )
  })

  bind_rows(rows) %>% distinct()
}

.resolve_obsolete_id <- function(fmt_id, replacements, ont) {
  if (fmt_id %in% ont$id && !isTRUE(ont$obsolete[fmt_id])) {
    return(data.frame(
      lookup_id = fmt_id,
      replacement_type = "original",
      stringsAsFactors = FALSE
    ))
  }

  candidates <- replacements %>%
    filter(source_id == fmt_id)

  if (nrow(candidates) == 0) {
    return(data.frame(
      lookup_id = fmt_id,
      replacement_type = if (fmt_id %in% ont$id) "obsolete_unresolved" else "missing",
      stringsAsFactors = FALSE
    ))
  }

  preferred <- candidates %>%
    filter(replacement_type == "replaced_by")

  if (nrow(preferred) == 0) {
    preferred <- candidates %>% filter(replacement_type == "consider")
  }

  preferred %>%
    transmute(
      lookup_id = replacement_id,
      replacement_type = replacement_type
    ) %>%
    distinct()
}

.add_obsolete_replacement_mappings <- function(map_df, uri_col, efo_obo_path) {
  if (nrow(map_df) == 0) return(map_df)
  if (!uri_col %in% names(map_df)) {
    stop("URI column not found in mapping table: ", uri_col)
  }

  replacements <- .parse_obo_obsolete_replacements(efo_obo_path) %>%
    group_by(source_id) %>%
    filter(
      replacement_type == "replaced_by" |
        (!any(replacement_type == "replaced_by") & replacement_type == "consider")
    ) %>%
    ungroup() %>%
    mutate(
      source_clean = .clean_uri(.revert_efo_id(source_id)),
      replacement_clean = .clean_uri(.revert_efo_id(replacement_id))
    ) %>%
    select(source_clean, replacement_clean) %>%
    distinct()

  if (nrow(replacements) == 0) return(map_df)

  replacement_rows <- map_df %>%
    inner_join(replacements, by = setNames("source_clean", uri_col),
               relationship = "many-to-many") %>%
    mutate("{uri_col}" := replacement_clean) %>%
    select(all_of(names(map_df)))

  bind_rows(map_df, replacement_rows) %>% distinct()
}

# ── CiteScore journal matching ─────────────────────────────────────────────

.normalise_journal_name <- function(x) {
  x <- tolower(trimws(x))
  x <- gsub("[^a-z0-9 ]", " ", x)
  gsub("\\s+", " ", trimws(x))
}

#' Load and deduplicate CiteScore journal data.
#'
#' @param path Path to CiteScore CSV (Scopus format).
#' @return Data frame: Title, CiteScore (max per title), title_norm.
load_citescore <- function(path) {
  cs <- data.table::fread(path, data.table = FALSE)
  cs <- cs[cs$Type == "j", c("Title", "CiteScore")]
  cs %>%
    group_by(Title) %>%
    summarise(CiteScore = max(CiteScore, na.rm = TRUE), .groups = "drop") %>%
    mutate(title_norm = .normalise_journal_name(Title))
}

# Match a character vector of GWAS journal names to CiteScore titles.
# Returns a data frame: JOURNAL, citescore_value, match_type.
# Four-stage pipeline:
#   1. Exact match on normalised name
#   2. Substring: GWAS name contained in CiteScore title
#   3. Token-prefix coverage: handles NLM abbreviations ("Nat Genet" → "Nature Genetics")
#   4. Strip parenthetical location suffixes and retry 1-3
#      (e.g. "Int J Obes (Lond)" → "Int J Obes")
.match_journals_to_citescore <- function(gwas_journals, citescore) {
  norm_q <- .normalise_journal_name(gwas_journals)
  n      <- length(norm_q)
  cs_val <- rep(NA_real_, n)
  mtype  <- rep("unmatched", n)

  # Stage 1: exact on normalised name
  exact_pos <- match(norm_q, citescore$title_norm)
  hits <- !is.na(exact_pos)
  cs_val[hits] <- citescore$CiteScore[exact_pos[hits]]
  mtype[hits]  <- "exact"

  # Stage 2: GWAS name as substring of CiteScore title → pick shortest match
  for (i in which(mtype == "unmatched")) {
    cands <- which(grepl(norm_q[i], citescore$title_norm, fixed = TRUE))
    if (length(cands) == 0) next
    best      <- cands[which.min(nchar(citescore$title_norm[cands]))]
    cs_val[i] <- citescore$CiteScore[best]
    mtype[i]  <- "substring"
  }

  # Stage 3: token-prefix coverage (handles NLM-style abbreviations).
  # Each GWAS token must bidirectionally prefix-match some CS title token.
  # Bidirectional: either the abbreviated token is a prefix of the full word,
  # or the full word is a prefix of the abbreviated form (e.g. "alzheimers"/"alzheimer").
  # Pre-index CiteScore by first 3 chars of the first SIGNIFICANT token
  # (skipping "the"/"a"/"an") to avoid O(n×28k) scan while still finding
  # titles like "The Lancet Infectious Diseases" for query "Lancet Infect Dis".
  .STOP_TOKS <- c("the", "a", "an")
  .first_sig <- function(toks) {
    sig <- toks[!toks %in% .STOP_TOKS]
    if (length(sig) > 0) sig[1] else toks[1]
  }

  cs_tokens  <- strsplit(citescore$title_norm, " ")
  first_tok3 <- vapply(cs_tokens, function(x) {
    if (length(x) == 0) return("")
    substr(.first_sig(x), 1, 3)
  }, character(1))
  cs_idx <- split(seq_along(cs_tokens), first_tok3)

  .cov <- function(qt, ct) {
    sum(vapply(qt, function(tok) {
      any(startsWith(ct, tok) | (nchar(tok) >= 3L & startsWith(tok, ct)))
    }, logical(1))) / length(qt)
  }

  .token_match <- function(qt) {
    if (length(qt) == 0) return(NULL)
    sig_tok <- .first_sig(qt)
    pfx     <- substr(sig_tok, 1, min(3L, nchar(sig_tok)))
    keys    <- names(cs_idx)[startsWith(names(cs_idx), pfx)]
    cands   <- unlist(cs_idx[keys], use.names = FALSE)
    if (length(cands) == 0) return(NULL)
    cov <- vapply(cs_tokens[cands], .cov, numeric(1), qt = qt)
    if (max(cov) < 0.8) return(NULL)
    list(val = citescore$CiteScore[cands[which.max(cov)]], type = "token")
  }

  q_tokens <- strsplit(norm_q, " ")
  for (i in which(mtype == "unmatched")) {
    r <- .token_match(q_tokens[[i]])
    if (!is.null(r)) { cs_val[i] <- r$val; mtype[i] <- r$type }
  }

  # Stage 4: strip parenthetical location suffixes from the ORIGINAL name, then
  # re-normalise and retry stages 1-3.
  # e.g. "Int J Obes (Lond)" → "Int J Obes" → "international journal of obesity"
  stripped_orig <- gsub("\\s*\\([^)]+\\)\\s*$", "", gwas_journals)
  stripped_norm <- .normalise_journal_name(stripped_orig)

  for (i in which(mtype == "unmatched")) {
    sn <- stripped_norm[i]
    if (sn == norm_q[i] || !nzchar(sn)) next

    ep <- match(sn, citescore$title_norm)
    if (!is.na(ep)) {
      cs_val[i] <- citescore$CiteScore[ep]; mtype[i] <- "stripped_exact"; next
    }
    cands <- which(grepl(sn, citescore$title_norm, fixed = TRUE))
    if (length(cands) > 0) {
      best      <- cands[which.min(nchar(citescore$title_norm[cands]))]
      cs_val[i] <- citescore$CiteScore[best]; mtype[i] <- "stripped_substring"; next
    }
    r <- .token_match(strsplit(sn, " ")[[1]])
    if (!is.null(r)) { cs_val[i] <- r$val; mtype[i] <- paste0("stripped_", r$type) }
  }

  data.frame(
    JOURNAL          = gwas_journals,
    citescore_value  = coalesce(cs_val, 0),
    match_type       = mtype,
    stringsAsFactors = FALSE
  )
}

# ── Step 1 ─────────────────────────────────────────────────────────────────
load_gwas_attention <- function(gwas_catalog_path,
                                citescore = NULL,
                                n_efo_max = NA_integer_) {
  ext <- tolower(tools::file_ext(gwas_catalog_path))
  a <- switch(
    ext,
    xlsx = read_xlsx(gwas_catalog_path),
    xls  = read_xlsx(gwas_catalog_path),
    tsv  = data.table::fread(gwas_catalog_path, sep = "\t", quote = "", data.table = FALSE),
    txt  = data.table::fread(gwas_catalog_path, sep = "\t", quote = "", data.table = FALSE),
    csv  = data.table::fread(gwas_catalog_path, data.table = FALSE),
    stop("Unsupported GWAS catalog file extension: ", ext)
  )

  if (!"Impact factor" %in% names(a)) {
    a$`Impact factor` <- 0
  }

  if ("INITIAL SAMPLE SIZE" %in% names(a)) {
    initial_cases <- .parse_sample_cases(a$`INITIAL SAMPLE SIZE`)
  } else {
    initial_cases <- rep(0, nrow(a))
  }

  if ("REPLICATION SAMPLE SIZE" %in% names(a)) {
    replication_cases <- .parse_sample_cases(a$`REPLICATION SAMPLE SIZE`)
  } else {
    replication_cases <- rep(0, nrow(a))
  }

  a$NCASE <- initial_cases + replication_cases
  if (all(a$NCASE == 0, na.rm = TRUE)) {
    a$NCASE <- 1
  }

  a <- a %>% tidyr::separate_rows(MAPPED_TRAIT_URI, sep = ", ")

  pubmed_count <- a %>%
    group_by(PUBMEDID) %>%
    summarise(n_efo = length(unique(MAPPED_TRAIT_URI)), .groups = "drop")
  a <- left_join(a, pubmed_count, by = "PUBMEDID")

  # Optional cap: exclude publications mapped to more than n_efo_max ontology terms.
  # Disabled by default — broad biobank/PheWAS studies are included at 1/n_efo
  # fractional weight, which already dilutes their per-trait contribution naturally.
  if (!is.na(n_efo_max) && is.finite(n_efo_max)) {
    n_excluded <- length(unique(a$PUBMEDID[a$n_efo > n_efo_max]))
    if (n_excluded > 0) {
      message(sprintf("  Excluding %d publications with n_efo > %d",
                      n_excluded, n_efo_max))
    }
    a <- a %>% filter(n_efo <= n_efo_max)
  }

  a$`Impact factor` <- trimws(a$`Impact factor`)
  a$`Impact factor` <- gsub("·", ".", a$`Impact factor`)  # middle dot → period
  a$`Impact factor` <- as.numeric(a$`Impact factor`)
  a$`Impact factor`[is.na(a$`Impact factor`)] <- 0

  # Override with CiteScore when provided
  if (!is.null(citescore) && "JOURNAL" %in% names(a)) {
    jlookup <- .match_journals_to_citescore(unique(a$JOURNAL), citescore)
    n_total   <- nrow(jlookup)
    n_exact   <- sum(jlookup$match_type == "exact")
    n_fuzzy   <- sum(jlookup$match_type == "fuzzy")
    n_miss    <- sum(jlookup$match_type == "unmatched")
    message(sprintf(
      "  CiteScore: %d unique journals — exact %d, fuzzy %d, unmatched %d",
      n_total, n_exact, n_fuzzy, n_miss
    ))
    a <- left_join(a, jlookup[, c("JOURNAL", "citescore_value")], by = "JOURNAL")
    a$`Impact factor` <- a$citescore_value
    a$citescore_value <- NULL
  }

  a$pub_year <- as.integer(format(as.Date(a$DATE), "%Y"))

  a %>%
    rename(
      ASSOCIATION_COUNT = `ASSOCIATION COUNT`,
      Impact_factor     = `Impact factor`,
      DISEASE_TRAIT     = `DISEASE/TRAIT`
    ) %>%
    select(MAPPED_TRAIT_URI, PUBMEDID, pub_year, n_efo,
           ASSOCIATION_COUNT, Impact_factor, DISEASE_TRAIT, NCASE) %>%
    distinct()
}

# ── Step 2 ─────────────────────────────────────────────────────────────────
.parse_gbd_gwascat_map <- function(gbd_gwascat, exclude_causes) {
  required <- c("Trait", "EFO term")
  missing_cols <- setdiff(required, names(gbd_gwascat))
  if (length(missing_cols) > 0) {
    stop("GBD-GWAS Catalog map is missing required columns: ",
         paste(missing_cols, collapse = ", "))
  }

  if (!"Exclude" %in% names(gbd_gwascat)) {
    gbd_gwascat$Exclude <- NA_character_
  }

  gbd_gwascat %>%
    mutate(
      `GBD term` = str_squish(as.character(Trait)),
      MAPPED_TRAIT_URI = as.character(`EFO term`),
      exclude_flag = tolower(str_squish(as.character(Exclude)))
    ) %>%
    filter(
      is.na(exclude_flag) |
        exclude_flag == "" |
        !exclude_flag %in% c("1", "true", "yes", "y", "exclude", "excluded")
    ) %>%
    filter(!is.na(`GBD term`), `GBD term` != "") %>%
    filter(!`GBD term` %in% exclude_causes) %>%
    drop_na(MAPPED_TRAIT_URI) %>%
    separate_rows(MAPPED_TRAIT_URI, sep = ",") %>%
    mutate(MAPPED_TRAIT_URI = .clean_uri(MAPPED_TRAIT_URI)) %>%
    filter(MAPPED_TRAIT_URI != "", MAPPED_TRAIT_URI != "na") %>%
    select(`GBD term`, MAPPED_TRAIT_URI) %>%
    distinct()
}

# ── OLS mapping loader ─────────────────────────────────────────────────────
# Reads the manually-judged OLS candidate file and returns a direct-map
# data frame (GBD term → MAPPED_TRAIT_URI) for keep_for_mapping == "yes" rows.
load_ols_mapping <- function(path, exclude_causes) {
  read_xlsx(path) %>%
    filter(keep_for_mapping == "yes") %>%
    filter(!cause_name %in% exclude_causes) %>%
    mutate(MAPPED_TRAIT_URI = .clean_uri(result_short_form)) %>%
    filter(MAPPED_TRAIT_URI != "", MAPPED_TRAIT_URI != "na") %>%
    select(`GBD term` = cause_name, MAPPED_TRAIT_URI) %>%
    distinct()
}

load_manual_trait_mapping <- function(path, exclude_causes) {
  manual <- read_xlsx(path, sheet = 1, col_types = "text")
  if (!"GBD terms" %in% names(manual)) {
    stop("Manual trait map is missing required column: GBD terms")
  }

  trait_cols <- setdiff(names(manual), "GBD terms")
  if (length(trait_cols) == 0) {
    stop("Manual trait map has no GWAS trait columns")
  }

  manual %>%
    rename(`GBD term` = `GBD terms`) %>%
    mutate(`GBD term` = str_squish(as.character(`GBD term`))) %>%
    filter(!is.na(`GBD term`), `GBD term` != "") %>%
    filter(!`GBD term` %in% exclude_causes) %>%
    pivot_longer(
      cols = all_of(trait_cols),
      names_to = "manual_source_col",
      values_to = "manual_trait"
    ) %>%
    mutate(manual_trait = str_squish(as.character(manual_trait))) %>%
    filter(!is.na(manual_trait), manual_trait != "") %>%
    filter(tolower(manual_trait) != "na") %>%
    separate_rows(manual_trait, sep = ",|\\r?\\n") %>%
    mutate(
      manual_trait = str_squish(manual_trait),
      MANUAL_TRAIT_NAME = tolower(trimws(gsub("[^a-zA-Z0-9 ]", "", manual_trait)))
    ) %>%
    filter(MANUAL_TRAIT_NAME != "", MANUAL_TRAIT_NAME != "na") %>%
    select(`GBD term`, MANUAL_TRAIT_NAME) %>%
    distinct()
}

load_gbd_efo_maps <- function(first_part_path, second_part_path, exclude_causes,
                              gbd_gwascat_path = NULL,
                              ols_mapping_path = NULL,
                              manual_trait_mapping_path = NULL) {
  efo_cols <- paste("EFO", 1:30)

  first <- read_xlsx(first_part_path) %>%
    distinct(`GBD term`, .keep_all = TRUE) %>%
    filter(!`GBD term` %in% exclude_causes) %>%
    pivot_longer(
      cols      = any_of(efo_cols),
      names_to  = "EFO_number",
      values_to = "MAPPED_TRAIT_URI"
    ) %>%
    drop_na(MAPPED_TRAIT_URI) %>%
    separate_rows(MAPPED_TRAIT_URI, sep = ",") %>%
    mutate(MAPPED_TRAIT_URI = .clean_uri(MAPPED_TRAIT_URI)) %>%
    filter(MAPPED_TRAIT_URI != "") %>%
    select(`GBD term`, MAPPED_TRAIT_URI)

  if (!is.null(gbd_gwascat_path)) {
    gbd_gwascat <- read_xlsx(gbd_gwascat_path) %>%
      .parse_gbd_gwascat_map(exclude_causes)
    first <- bind_rows(first, gbd_gwascat) %>%
      distinct(`GBD term`, MAPPED_TRAIT_URI)
  }

  if (!is.null(ols_mapping_path)) {
    ols <- load_ols_mapping(ols_mapping_path, exclude_causes)
    message("  OLS mapping: ", nrow(ols), " GBD term × EFO pairs")
    first <- bind_rows(first, ols) %>%
      distinct(`GBD term`, MAPPED_TRAIT_URI)
  }

  manual_traits <- NULL
  if (!is.null(manual_trait_mapping_path)) {
    manual_traits <- load_manual_trait_mapping(manual_trait_mapping_path, exclude_causes)
    message("  Manual trait mapping: ", nrow(manual_traits),
            " GBD term × GWAS trait strings")
  }

  second <- read_xlsx(second_part_path) %>%
    filter(!`GBD term` %in% exclude_causes)

  # Root EFO URIs from Second_part_GBD (columns MAPPED_TRAIT_URI...2 through ...13)
  uri_cols <- grep("^MAPPED_TRAIT_URI", names(second), value = TRUE)
  ontology <- second %>%
    select(`GBD term`, all_of(uri_cols)) %>%
    pivot_longer(
      cols      = all_of(uri_cols),
      names_to  = "col",
      values_to = "root_EFO_URI"
    ) %>%
    drop_na(root_EFO_URI) %>%
    filter(root_EFO_URI != "") %>%
    select(`GBD term`, root_EFO_URI) %>%
    distinct()

  list(direct = first, ontology = ontology, manual_traits = manual_traits)
}

empty_gbd_efo_master_mapping <- function() {
  tibble(
    gbd_term = character(),
    mapped_trait_uri = character(),
    mapping_strategy = character(),
    mapping_source = character(),
    include_in_pipeline = logical(),
    hierarchy_distance = integer(),
    hierarchy_weight = numeric(),
    root_mapped_trait_uri = character(),
    lookup_trait_uri = character(),
    replacement_type = character(),
    source_file = character(),
    source_column = character(),
    source_value = character(),
    search_term = character(),
    matched_gwas_trait_examples = character(),
    n_matching_pubmeds = integer()
  )
}

.standardise_master_mapping <- function(x) {
  template <- empty_gbd_efo_master_mapping()
  for (nm in setdiff(names(template), names(x))) {
    x[[nm]] <- template[[nm]][NA]
  }

  x %>%
    mutate(
      gbd_term = str_squish(as.character(gbd_term)),
      mapped_trait_uri = .clean_uri(mapped_trait_uri),
      include_in_pipeline = if_else(is.na(include_in_pipeline), TRUE, include_in_pipeline),
      hierarchy_distance = suppressWarnings(as.integer(hierarchy_distance)),
      hierarchy_distance = if_else(is.na(hierarchy_distance), 0L, hierarchy_distance),
      hierarchy_weight = suppressWarnings(as.numeric(hierarchy_weight)),
      hierarchy_weight = if_else(is.na(hierarchy_weight), 1, hierarchy_weight)
    ) %>%
    filter(!is.na(gbd_term), gbd_term != "") %>%
    filter(!is.na(mapped_trait_uri), mapped_trait_uri != "", mapped_trait_uri != "na") %>%
    select(all_of(names(template))) %>%
    distinct()
}

load_gbd_efo_master_mapping <- function(path, active_only = TRUE) {
  master <- data.table::fread(path, sep = "\t", data.table = FALSE) %>%
    .standardise_master_mapping()

  if (active_only) {
    master <- master %>% filter(include_in_pipeline)
  }

  master
}

.first_part_master_rows <- function(path, exclude_causes) {
  efo_cols <- paste("EFO", 1:30)

  read_xlsx(path, col_types = "text") %>%
    distinct(`GBD term`, .keep_all = TRUE) %>%
    filter(!`GBD term` %in% exclude_causes) %>%
    pivot_longer(
      cols = any_of(efo_cols),
      names_to = "source_column",
      values_to = "source_value"
    ) %>%
    drop_na(source_value) %>%
    separate_rows(source_value, sep = ",") %>%
    transmute(
      gbd_term = str_squish(as.character(`GBD term`)),
      mapped_trait_uri = .clean_uri(source_value),
      mapping_strategy = "direct",
      mapping_source = "first_part",
      include_in_pipeline = TRUE,
      hierarchy_distance = 0L,
      hierarchy_weight = 1,
      root_mapped_trait_uri = mapped_trait_uri,
      lookup_trait_uri = NA_character_,
      replacement_type = "original",
      source_file = basename(path),
      source_column,
      source_value = str_squish(as.character(source_value)),
      search_term = NA_character_,
      matched_gwas_trait_examples = NA_character_,
      n_matching_pubmeds = NA_integer_
    ) %>%
    .standardise_master_mapping()
}

.gbd_gwascat_master_rows <- function(path, exclude_causes) {
  if (is.null(path) || !file.exists(path)) return(empty_gbd_efo_master_mapping())

  read_xlsx(path, col_types = "text") %>%
    .parse_gbd_gwascat_map(exclude_causes) %>%
    transmute(
      gbd_term = `GBD term`,
      mapped_trait_uri = MAPPED_TRAIT_URI,
      mapping_strategy = "direct",
      mapping_source = "gbd_gwascat",
      include_in_pipeline = TRUE,
      hierarchy_distance = 0L,
      hierarchy_weight = 1,
      root_mapped_trait_uri = mapped_trait_uri,
      lookup_trait_uri = NA_character_,
      replacement_type = "original",
      source_file = basename(path),
      source_column = "EFO term",
      source_value = MAPPED_TRAIT_URI,
      search_term = NA_character_,
      matched_gwas_trait_examples = NA_character_,
      n_matching_pubmeds = NA_integer_
    ) %>%
    .standardise_master_mapping()
}

.second_part_root_map <- function(path, exclude_causes) {
  second <- read_xlsx(path, col_types = "text") %>%
    filter(!`GBD term` %in% exclude_causes)
  uri_cols <- grep("^MAPPED_TRAIT_URI", names(second), value = TRUE)

  second %>%
    select(`GBD term`, all_of(uri_cols)) %>%
    pivot_longer(
      cols = all_of(uri_cols),
      names_to = "source_column",
      values_to = "root_EFO_URI"
    ) %>%
    drop_na(root_EFO_URI) %>%
    mutate(root_EFO_URI = str_squish(as.character(root_EFO_URI))) %>%
    filter(root_EFO_URI != "") %>%
    transmute(
      `GBD term` = str_squish(as.character(`GBD term`)),
      root_EFO_URI,
      source_column,
      source_value = root_EFO_URI,
      source_file = basename(path)
    ) %>%
    distinct()
}

.add_direct_obsolete_replacements_to_master <- function(master, efo_obo_path) {
  if (is.null(efo_obo_path) || !file.exists(efo_obo_path) || nrow(master) == 0) {
    return(master)
  }

  replacements <- .parse_obo_obsolete_replacements(efo_obo_path) %>%
    group_by(source_id) %>%
    filter(
      replacement_type == "replaced_by" |
        (!any(replacement_type == "replaced_by") & replacement_type == "consider")
    ) %>%
    ungroup() %>%
    mutate(
      source_clean = .clean_uri(.revert_efo_id(source_id)),
      replacement_clean = .clean_uri(.revert_efo_id(replacement_id))
    ) %>%
    select(source_clean, replacement_clean, replacement_type) %>%
    distinct()

  replacement_rows <- master %>%
    filter(mapping_strategy == "direct") %>%
    inner_join(replacements, by = c("mapped_trait_uri" = "source_clean"),
               relationship = "many-to-many") %>%
    mutate(
      mapped_trait_uri = replacement_clean,
      mapping_strategy = "direct_obsolete_replacement",
      root_mapped_trait_uri = source_value,
      replacement_type = replacement_type.y
    ) %>%
    select(all_of(names(master)))

  bind_rows(master, replacement_rows) %>%
    .standardise_master_mapping()
}

.attention_trait_index <- function(attention) {
  attention %>%
    mutate(
      mapped_trait_uri = .clean_uri(MAPPED_TRAIT_URI),
      trait_name_normalised = .normalise_trait_name(DISEASE_TRAIT)
    ) %>%
    filter(mapped_trait_uri != "", mapped_trait_uri != "na") %>%
    distinct(mapped_trait_uri, PUBMEDID, DISEASE_TRAIT, trait_name_normalised)
}

.terms_with_attention_from_master <- function(master, attention_index) {
  master %>%
    filter(include_in_pipeline) %>%
    inner_join(attention_index, by = "mapped_trait_uri", relationship = "many-to-many") %>%
    distinct(gbd_term) %>%
    pull(gbd_term)
}

.string_match_master_rows <- function(gbd_terms, attention_index, already_matched_terms,
                                      source_name = "gbd_name_string_match") {
  unmatched <- tibble(gbd_term = sort(unique(gbd_terms))) %>%
    filter(!gbd_term %in% already_matched_terms) %>%
    mutate(search_term = .normalise_trait_name(gbd_term)) %>%
    filter(search_term != "", search_term != "na")

  if (nrow(unmatched) == 0 || nrow(attention_index) == 0) {
    return(empty_gbd_efo_master_mapping())
  }

  unmatched %>%
    cross_join(attention_index) %>%
    filter(str_detect(trait_name_normalised, fixed(search_term))) %>%
    group_by(gbd_term, mapped_trait_uri, search_term) %>%
    summarise(
      matched_gwas_trait_examples = paste(head(sort(unique(DISEASE_TRAIT)), 5), collapse = " | "),
      n_matching_pubmeds = n_distinct(PUBMEDID),
      .groups = "drop"
    ) %>%
    transmute(
      gbd_term,
      mapped_trait_uri,
      mapping_strategy = "string_match",
      mapping_source = source_name,
      include_in_pipeline = TRUE,
      hierarchy_distance = 0L,
      hierarchy_weight = 1,
      root_mapped_trait_uri = mapped_trait_uri,
      lookup_trait_uri = NA_character_,
      replacement_type = NA_character_,
      source_file = "GWAS catalog",
      source_column = "DISEASE/TRAIT",
      source_value = search_term,
      search_term,
      matched_gwas_trait_examples,
      n_matching_pubmeds
    ) %>%
    .standardise_master_mapping()
}

.manual_trait_master_rows <- function(manual_path, attention_index, already_matched_terms,
                                      exclude_causes) {
  if (is.null(manual_path) || !file.exists(manual_path)) {
    return(empty_gbd_efo_master_mapping())
  }

  manual <- load_manual_trait_mapping(manual_path, exclude_causes) %>%
    filter(!`GBD term` %in% already_matched_terms)

  if (nrow(manual) == 0 || nrow(attention_index) == 0) {
    return(empty_gbd_efo_master_mapping())
  }

  manual %>%
    rename(gbd_term = `GBD term`, search_term = MANUAL_TRAIT_NAME) %>%
    cross_join(attention_index) %>%
    filter(str_detect(trait_name_normalised, fixed(search_term))) %>%
    group_by(gbd_term, mapped_trait_uri, search_term) %>%
    summarise(
      matched_gwas_trait_examples = paste(head(sort(unique(DISEASE_TRAIT)), 5), collapse = " | "),
      n_matching_pubmeds = n_distinct(PUBMEDID),
      .groups = "drop"
    ) %>%
    transmute(
      gbd_term,
      mapped_trait_uri,
      mapping_strategy = "manual_trait_string",
      mapping_source = "manual_curated_gwas_trait",
      include_in_pipeline = TRUE,
      hierarchy_distance = 0L,
      hierarchy_weight = 1,
      root_mapped_trait_uri = mapped_trait_uri,
      lookup_trait_uri = NA_character_,
      replacement_type = NA_character_,
      source_file = basename(manual_path),
      source_column = "GWAS traits",
      source_value = search_term,
      search_term,
      matched_gwas_trait_examples,
      n_matching_pubmeds
    ) %>%
    .standardise_master_mapping()
}

# ── Step 3 ─────────────────────────────────────────────────────────────────
.descendants_with_distance <- function(ont, root_id) {
  if (!root_id %in% ont$id) {
    return(data.frame(id = character(), hierarchy_distance = integer()))
  }

  visited <- setNames(0L, root_id)
  queue <- root_id

  while (length(queue) > 0) {
    current <- queue[[1]]
    queue <- queue[-1]
    current_distance <- visited[[current]]
    children <- ont$children[[current]]
    children <- children[!is.na(children)]
    new_children <- children[!children %in% names(visited)]

    if (length(new_children) > 0) {
      visited[new_children] <- current_distance + 1L
      queue <- c(queue, new_children)
    }
  }

  data.frame(
    id = names(visited),
    hierarchy_distance = as.integer(unname(visited)),
    stringsAsFactors = FALSE
  )
}

expand_efo_descendants <- function(ontology_map, efo_obo_path,
                                   cache_path = NULL) {
  cache_version <- "obsolete-replacement-v4-distance"
  if (!is.null(cache_path) && file.exists(cache_path)) {
    cached <- readRDS(cache_path)
    if ("root_EFO_URI" %in% names(cached) &&
        identical(attr(cached, "cache_version"), cache_version)) {
      missing_roots <- ontology_map %>%
        distinct(`GBD term`, root_EFO_URI) %>%
        anti_join(cached %>% distinct(`GBD term`, root_EFO_URI),
                  by = c("GBD term", "root_EFO_URI"))
      if (nrow(missing_roots) == 0) {
        message("Loading descendant map from cache: ", cache_path)
        return(cached)
      }
      message("Ignoring descendant cache with ", nrow(missing_roots),
              " new ontology roots: ", cache_path)
    } else {
      message("Ignoring legacy descendant cache: ", cache_path)
    }
  }

  message("Parsing EFO OBO file (this takes ~2-5 min)…")
  ont <- get_OBO(efo_obo_path)
  replacements <- .parse_obo_obsolete_replacements(efo_obo_path)

  rows <- lapply(seq_len(nrow(ontology_map)), function(i) {
    gbd_term  <- ontology_map$`GBD term`[i]
    root_uri  <- ontology_map$root_EFO_URI[i]
    fmt_id    <- .format_efo_id(root_uri)
    lookup_ids <- .resolve_obsolete_id(fmt_id, replacements, ont)

    bind_rows(lapply(seq_len(nrow(lookup_ids)), function(j) {
      lookup_id <- lookup_ids$lookup_id[j]
      replacement_type <- lookup_ids$replacement_type[j]

      if (!lookup_id %in% ont$id) {
        return(data.frame(
          `GBD term`     = gbd_term,
          root_EFO_URI   = root_uri,
          lookup_EFO_URI = lookup_id,
          replacement_type = replacement_type,
          descendant_URI = NA_character_,
          hierarchy_distance = NA_integer_,
          stringsAsFactors = FALSE,
          check.names = FALSE
        ))
      }

      desc <- .descendants_with_distance(ont, lookup_id)
      if (nrow(desc) == 0) {
        return(data.frame(
          `GBD term`     = gbd_term,
          root_EFO_URI   = root_uri,
          lookup_EFO_URI = lookup_id,
          replacement_type = replacement_type,
          descendant_URI = NA_character_,
          hierarchy_distance = NA_integer_,
          stringsAsFactors = FALSE,
          check.names = FALSE
        ))
      }

      desc_reverted <- .revert_efo_id(desc$id)
      desc_clean    <- .clean_uri(desc_reverted)
      data.frame(
        `GBD term`     = gbd_term,
        root_EFO_URI   = root_uri,
        lookup_EFO_URI = lookup_id,
        replacement_type = replacement_type,
        descendant_URI = desc_clean,
        hierarchy_distance = desc$hierarchy_distance,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }))
  })

  result <- bind_rows(rows) %>% distinct()

  if (!is.null(cache_path)) {
    dir.create(dirname(cache_path), showWarnings = FALSE, recursive = TRUE)
    attr(result, "cache_version") <- cache_version
    saveRDS(result, cache_path)
    message("Descendant map cached to: ", cache_path)
  }

  result
}

build_gbd_efo_master_mapping <- function(gwas_catalog_path,
                                         first_part_path,
                                         second_part_path,
                                         efo_obo_path,
                                         exclude_causes,
                                         gbd_gwascat_path = NULL,
                                         manual_trait_mapping_path = NULL,
                                         descendant_cache_path = NULL,
                                         hierarchy_decay = 1) {
  attention_index <- load_gwas_attention(gwas_catalog_path) %>%
    .attention_trait_index()

  direct_rows <- bind_rows(
    .first_part_master_rows(first_part_path, exclude_causes),
    .gbd_gwascat_master_rows(gbd_gwascat_path, exclude_causes)
  ) %>%
    .add_direct_obsolete_replacements_to_master(efo_obo_path)

  second_roots <- .second_part_root_map(second_part_path, exclude_causes)
  second_root_rows <- second_roots %>%
    transmute(
      gbd_term = `GBD term`,
      mapped_trait_uri = .clean_uri(root_EFO_URI),
      mapping_strategy = "hierarchy_root_source",
      mapping_source = "second_part",
      include_in_pipeline = TRUE,
      hierarchy_distance = 0L,
      hierarchy_weight = 1,
      root_mapped_trait_uri = .clean_uri(root_EFO_URI),
      lookup_trait_uri = NA_character_,
      replacement_type = "source_root",
      source_file,
      source_column,
      source_value,
      search_term = NA_character_,
      matched_gwas_trait_examples = NA_character_,
      n_matching_pubmeds = NA_integer_
    ) %>%
    .standardise_master_mapping()

  descendant_map <- expand_efo_descendants(
    second_roots %>% select(`GBD term`, root_EFO_URI) %>% distinct(),
    efo_obo_path,
    cache_path = descendant_cache_path
  )

  hierarchy_rows <- descendant_map %>%
    filter(!is.na(descendant_URI), descendant_URI != "") %>%
    left_join(second_roots,
              by = c("GBD term" = "GBD term", "root_EFO_URI" = "root_EFO_URI"),
              relationship = "many-to-many") %>%
    mutate(
      hierarchy_distance = if_else(is.na(hierarchy_distance), 0L,
                                   as.integer(hierarchy_distance)),
      hierarchy_weight = hierarchy_decay ^ hierarchy_distance
    ) %>%
    transmute(
      gbd_term = `GBD term`,
      mapped_trait_uri = descendant_URI,
      mapping_strategy = if_else(hierarchy_distance == 0L,
                                 "hierarchy_root", "hierarchy_descendant"),
      mapping_source = "second_part",
      include_in_pipeline = TRUE,
      hierarchy_distance,
      hierarchy_weight,
      root_mapped_trait_uri = .clean_uri(root_EFO_URI),
      lookup_trait_uri = .clean_uri(.revert_efo_id(lookup_EFO_URI)),
      replacement_type,
      source_file,
      source_column,
      source_value,
      search_term = NA_character_,
      matched_gwas_trait_examples = NA_character_,
      n_matching_pubmeds = NA_integer_
    ) %>%
    .standardise_master_mapping()

  manual_terms <- if (!is.null(manual_trait_mapping_path) &&
                      file.exists(manual_trait_mapping_path)) {
    load_manual_trait_mapping(manual_trait_mapping_path, exclude_causes) %>%
      pull(`GBD term`)
  } else {
    character()
  }

  all_gbd_terms <- sort(unique(c(
    direct_rows$gbd_term,
    second_roots$`GBD term`,
    manual_terms
  )))

  mapped_after_efo <- .terms_with_attention_from_master(
    bind_rows(direct_rows, second_root_rows, hierarchy_rows),
    attention_index
  )

  string_rows <- .string_match_master_rows(
    all_gbd_terms,
    attention_index,
    already_matched_terms = mapped_after_efo
  )

  mapped_after_string <- .terms_with_attention_from_master(
    bind_rows(direct_rows, second_root_rows, hierarchy_rows, string_rows),
    attention_index
  )

  manual_rows <- .manual_trait_master_rows(
    manual_trait_mapping_path,
    attention_index,
    already_matched_terms = mapped_after_string,
    exclude_causes = exclude_causes
  )

  bind_rows(direct_rows, second_root_rows, hierarchy_rows, string_rows, manual_rows) %>%
    filter(!gbd_term %in% exclude_causes) %>%
    arrange(gbd_term, mapping_strategy, hierarchy_distance, mapping_source,
            mapped_trait_uri, search_term) %>%
    .standardise_master_mapping()
}

.active_master_edges <- function(master_map, exclude_causes) {
  master_map %>%
    .standardise_master_mapping() %>%
    filter(include_in_pipeline) %>%
    filter(!gbd_term %in% exclude_causes) %>%
    group_by(gbd_term, mapped_trait_uri) %>%
    summarise(
      hierarchy_weight = max(hierarchy_weight, na.rm = TRUE),
      hierarchy_distance = min(hierarchy_distance, na.rm = TRUE),
      mapping_strategy = paste(sort(unique(mapping_strategy)), collapse = ";"),
      mapping_source = paste(sort(unique(mapping_source)), collapse = ";"),
      .groups = "drop"
    ) %>%
    mutate(hierarchy_weight = if_else(is.finite(hierarchy_weight), hierarchy_weight, 1))
}

map_attention_to_gbd_leaves_from_master <- function(attention, master_map,
                                                   exclude_causes) {
  att <- .attention_efo_by_pubmed(attention)
  att$mapped_trait_uri <- .clean_uri(att$MAPPED_TRAIT_URI)
  att <- att %>% filter(!is.na(mapped_trait_uri), mapped_trait_uri != "na")

  active_edges <- .active_master_edges(master_map, exclude_causes)

  matched <- inner_join(
    att,
    active_edges,
    by = "mapped_trait_uri",
    relationship = "many-to-many"
  ) %>%
    mutate(
      `GBD term` = gbd_term,
      n = n * hierarchy_weight,
      weighted_n = weighted_n * hierarchy_weight,
      nhits = nhits * hierarchy_weight,
      weighted_nhits = weighted_nhits * hierarchy_weight,
      weighted_attention_score_impact_factor =
        weighted_attention_score_impact_factor * hierarchy_weight
    )

  scored <- if (nrow(matched) > 0) {
    .score_gbd_term(matched, "GBD term") %>%
      left_join(
        matched %>%
          distinct(`GBD term`, mapping_strategy) %>%
          group_by(`GBD term`) %>%
          summarise(match_type = paste(sort(unique(mapping_strategy)), collapse = ";"),
                    .groups = "drop"),
        by = "GBD term"
      )
  } else {
    tibble(
      `GBD term` = character(),
      total_attention_score = numeric(),
      weighted_n = numeric(),
      nhits = numeric(),
      weighted_nhits = numeric(),
      weighted_attention_score_impact_factor = numeric(),
      match_type = character()
    )
  }

  all_gbd <- active_edges %>%
    distinct(`GBD term` = gbd_term)

  unmatched <- all_gbd %>%
    filter(!`GBD term` %in% scored$`GBD term`) %>%
    mutate(
      total_attention_score = 0,
      weighted_n = 0,
      nhits = 0,
      weighted_nhits = 0,
      weighted_attention_score_impact_factor = 0,
      match_type = "none"
    )

  bind_rows(scored, unmatched) %>%
    distinct(`GBD term`, .keep_all = TRUE)
}

# ── Internal: aggregate raw attention to (MAPPED_TRAIT_URI × PUBMEDID) ─────
# Returns one contribution per EFO-publication pair. Keeping PUBMEDID in the
# aggregation prevents all-time EFO totals being counted once per publication.
.attention_efo_by_pubmed <- function(attention) {
  if (!"NCASE" %in% names(attention)) {
    attention$NCASE <- 1
  }

  attention %>%
    group_by(MAPPED_TRAIT_URI, PUBMEDID) %>%
    summarise(
      n             = sum(NCASE, na.rm = TRUE),
      weighted_n    = sum(NCASE / n_efo, na.rm = TRUE),
      nhits         = sum(ASSOCIATION_COUNT),
      weighted_nhits = sum(ASSOCIATION_COUNT / n_efo),
      weighted_attention_score_impact_factor = sum((1 / n_efo) * Impact_factor),
      DISEASE_TRAIT = paste(unique(DISEASE_TRAIT), collapse = "; "),
      .groups = "drop"
    )
}

# ── Internal: score one matched set ────────────────────────────────────────
# gbd_col is the column name for the GBD term in matched_df (GBD term or GBD_TERM).
.score_gbd_term <- function(matched_df, gbd_col) {
  matched_df %>%
    rename(`GBD term` = all_of(gbd_col)) %>%
    group_by(`GBD term`, PUBMEDID) %>%
    summarise(
      total_attention_score_per_pubmed       = sum(n,             na.rm = TRUE),
      weighted_n                             = sum(weighted_n,    na.rm = TRUE),
      nhits                                  = sum(nhits,         na.rm = TRUE),
      weighted_nhits                         = sum(weighted_nhits, na.rm = TRUE),
      weighted_attention_score_impact_factor = sum(weighted_attention_score_impact_factor, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(`GBD term`) %>%
    summarise(
      total_attention_score                  = sum(total_attention_score_per_pubmed, na.rm = TRUE),
      weighted_n                             = sum(weighted_n,    na.rm = TRUE),
      nhits                                  = sum(nhits,         na.rm = TRUE),
      weighted_nhits                         = sum(weighted_nhits, na.rm = TRUE),
      weighted_attention_score_impact_factor = sum(weighted_attention_score_impact_factor, na.rm = TRUE),
      .groups = "drop"
    )
}

# ── Step 4 ─────────────────────────────────────────────────────────────────
map_attention_to_gbd_leaves <- function(attention, direct_map, descendant_map,
                                        exclude_causes, manual_trait_map = NULL) {
  att <- .attention_efo_by_pubmed(attention)

  # Clean MAPPED_TRAIT_URI so URIs match across datasets
  att$MAPPED_TRAIT_URI <- .clean_uri(att$MAPPED_TRAIT_URI)
  att <- att %>% filter(!is.na(MAPPED_TRAIT_URI), MAPPED_TRAIT_URI != "na")

  direct_map <- direct_map %>%
    rename(`GBD term.x` = `GBD term`) %>%
    distinct(`GBD term.x`, MAPPED_TRAIT_URI)

  if (!is.null(descendant_map) && nrow(descendant_map) > 0) {
    descendant_map <- descendant_map %>%
      distinct(`GBD term`, descendant_URI)
  }

  # ── Stage A: direct EFO match ──────────────────────────────────────────
  matched_direct <- inner_join(att, direct_map,
                               by = "MAPPED_TRAIT_URI",
                               relationship = "many-to-many") %>%
    rename(`GBD term` = `GBD term.x`)

  scored_direct <- .score_gbd_term(matched_direct, "GBD term") %>%
    mutate(match_type = "direct")

  matched_direct_terms <- unique(scored_direct$`GBD term`)

  # ── Stage B: ontology descendant match ────────────────────────────────
  scored_B <- NULL
  if (!is.null(descendant_map) && nrow(descendant_map) > 0 && nrow(att) > 0) {
    matched_desc <- inner_join(
      att,
      descendant_map %>%
        filter(!is.na(descendant_URI), descendant_URI != "") %>%
        rename(MAPPED_TRAIT_URI = descendant_URI),
      by = "MAPPED_TRAIT_URI",
      relationship = "many-to-many"
    )

    if (nrow(matched_desc) > 0) {
      scored_B <- .score_gbd_term(matched_desc, "GBD term") %>%
        filter(!`GBD term` %in% matched_direct_terms) %>%
        mutate(match_type = "ontology")
    }
  }

  matched_B_terms <- if (!is.null(scored_B)) unique(scored_B$`GBD term`) else character(0)

  # ── Stage C: fuzzy string match on DISEASE_TRAIT ──────────────────────
  # Find GBD terms not yet matched in A or B
  all_gbd_terms <- bind_rows(
    direct_map %>% select(`GBD term` = `GBD term.x`),
    if (!is.null(descendant_map) && nrow(descendant_map) > 0)
      descendant_map %>% select(`GBD term`)
    else
      NULL
  ) %>%
    filter(!`GBD term` %in% exclude_causes) %>%
    distinct()

  unmatched_gbd <- all_gbd_terms %>%
    filter(!`GBD term` %in% c(matched_direct_terms, matched_B_terms)) %>%
    mutate(GBD_TERM_NAME = tolower(trimws(gsub("[^a-zA-Z0-9 ]", "", `GBD term`))))

  scored_C <- NULL
  if (nrow(unmatched_gbd) > 0 && nrow(att) > 0) {
    att_for_fuzzy <- att %>%
      mutate(TRAIT_NAME = tolower(trimws(gsub("[^a-zA-Z0-9 ]", "", DISEASE_TRAIT))))

    partial <- unmatched_gbd %>%
      cross_join(att_for_fuzzy) %>%
      filter(str_detect(TRAIT_NAME, fixed(GBD_TERM_NAME))) %>%
      select(`GBD term`, PUBMEDID, n, weighted_n, nhits,
             weighted_nhits, weighted_attention_score_impact_factor) %>%
      distinct()

    if (nrow(partial) > 0) {
      scored_C <- .score_gbd_term(partial, "GBD term") %>%
        mutate(match_type = "fuzzy")
    }
  }

  matched_C_terms <- if (!is.null(scored_C)) unique(scored_C$`GBD term`) else character(0)

  # ── Stage D: curated manual GWAS-trait string matches ────────────────────
  scored_D <- NULL
  if (!is.null(manual_trait_map) && nrow(manual_trait_map) > 0 &&
      nrow(att) > 0) {
    manual_unmatched <- manual_trait_map %>%
      filter(!`GBD term` %in% exclude_causes) %>%
      filter(!`GBD term` %in% c(matched_direct_terms, matched_B_terms,
                                matched_C_terms)) %>%
      distinct(`GBD term`, MANUAL_TRAIT_NAME)

    if (nrow(manual_unmatched) > 0) {
      att_for_manual <- att %>%
        mutate(TRAIT_NAME = tolower(trimws(gsub("[^a-zA-Z0-9 ]", "", DISEASE_TRAIT))))

      manual_matches <- manual_unmatched %>%
        cross_join(att_for_manual) %>%
        filter(str_detect(TRAIT_NAME, fixed(MANUAL_TRAIT_NAME))) %>%
        select(`GBD term`, PUBMEDID, n, weighted_n, nhits,
               weighted_nhits, weighted_attention_score_impact_factor) %>%
        distinct()

      if (nrow(manual_matches) > 0) {
        scored_D <- .score_gbd_term(manual_matches, "GBD term") %>%
          mutate(match_type = "manual_trait")
      }
    }
  }

  # ── Combine matched terms ──────────────────────────────────────────────
  matched_all <- bind_rows(scored_direct, scored_B, scored_C, scored_D) %>%
    filter(!`GBD term` %in% exclude_causes)

  # All GBD terms from both parts (post-exclusion)
  all_gbd <- bind_rows(
    direct_map %>% select(`GBD term` = `GBD term.x`),
    if (!is.null(descendant_map) && nrow(descendant_map) > 0)
      descendant_map %>% select(`GBD term`)
    else
      NULL,
    if (!is.null(manual_trait_map) && nrow(manual_trait_map) > 0)
      manual_trait_map %>% select(`GBD term`)
    else
      NULL
  ) %>%
    filter(!`GBD term` %in% exclude_causes) %>%
    distinct(`GBD term`)

  # Zero-score rows for unmatched GBD terms
  unmatched_final <- all_gbd %>%
    filter(!`GBD term` %in% unique(matched_all$`GBD term`)) %>%
    mutate(
      total_attention_score                  = 0,
      weighted_n                             = 0,
      nhits                                  = 0,
      weighted_nhits                         = 0,
      weighted_attention_score_impact_factor = 0,
      match_type                             = "none"
    )

  bind_rows(matched_all, unmatched_final) %>%
    distinct(`GBD term`, .keep_all = TRUE)
}

# ── Step 5 ─────────────────────────────────────────────────────────────────
rollup_hierarchy <- function(leaf_scores, hierarchy_path) {
  sheet_names <- excel_sheets(hierarchy_path)
  cause_sheet <- grep("cause hierarchy", sheet_names, ignore.case = TRUE, value = TRUE)[1]
  if (is.na(cause_sheet)) {
    stop("Could not find a Cause Hierarchy sheet in: ", hierarchy_path)
  }
  h <- read_xlsx(hierarchy_path, sheet = cause_sheet)

  # Normalise column names across file versions
  names(h) <- trimws(names(h))
  cause_id_col   <- grep("^[Cc]ause.?[Ii][Dd]$",   names(h), value = TRUE)[1]
  cause_name_col <- grep("^[Cc]ause.?[Nn]ame$",     names(h), value = TRUE)[1]
  parent_col     <- grep("^[Pp]arent.?[Nn]ame$",    names(h), value = TRUE)[1]
  level_col      <- grep("^[Ll]evel$",               names(h), value = TRUE)[1]
  outline_col    <- grep("^[Cc]ause.?[Oo]utline$",  names(h), value = TRUE)[1]

  h <- h %>%
    rename(
      cause_id    = all_of(cause_id_col),
      Cause_Name  = all_of(cause_name_col),
      Parent_Name = all_of(parent_col),
      Level       = all_of(level_col),
      Outline     = all_of(outline_col)
    )

  # Keep only CMNN (A…) and NCD (B…) at levels 3 and 4
  h1 <- h %>%
    filter(grepl("^[AB]", Outline), Level %in% c(3, 4))

  # Leaf nodes = cause names that are never a parent within h1
  h3 <- h1 %>% filter(!Cause_Name %in% Parent_Name)

  # cause_id lookup for all h1 terms
  cause_id_lookup <- h1 %>%
    select(Cause_Name, cause_id) %>%
    distinct()

  # Rename for clarity
  combined <- leaf_scores %>%
    rename(GBD.term = `GBD term`)

  # ── Leaf nodes: direct scores (left_join so zero-attention terms appear) ──
  matched_h3 <- left_join(
    h3 %>% select(Cause_Name, cause_id),
    combined %>% select(GBD.term, total_attention_score, nhits,
                        weighted_n, weighted_nhits,
                        weighted_attention_score_impact_factor),
    by = c("Cause_Name" = "GBD.term"),
    multiple = "first"
  ) %>%
    mutate(across(c(total_attention_score, nhits, weighted_n,
                    weighted_nhits, weighted_attention_score_impact_factor),
                  ~ coalesce(., 0)))

  # ── Level-3 parents: own score + sum of children ──────────────────────
  # Include parents even when they have no own EFO mapping; otherwise a
  # master map containing only child-level mappings would silently drop the
  # parent rollup row.
  parent_terms <- union(
    setdiff(combined$GBD.term, h3$Cause_Name),
    h1 %>%
      filter(Cause_Name %in% Parent_Name) %>%
      pull(Cause_Name)
  )
  parents <- tibble(GBD.term = parent_terms) %>%
    left_join(combined, by = "GBD.term") %>%
    mutate(
      total_attention_score = coalesce(total_attention_score, 0),
      nhits = coalesce(nhits, 0),
      weighted_n = coalesce(weighted_n, 0),
      weighted_nhits = coalesce(weighted_nhits, 0),
      weighted_attention_score_impact_factor =
        coalesce(weighted_attention_score_impact_factor, 0)
    )

  if (nrow(parents) > 0) {
    # Children of these parents in h1
    children_map <- h1 %>%
      filter(Parent_Name %in% parents$GBD.term) %>%
      select(Cause_Name, Parent_Name)

    children_scores <- inner_join(
      children_map,
      combined %>% select(GBD.term, total_attention_score, nhits,
                          weighted_n, weighted_nhits,
                          weighted_attention_score_impact_factor),
      by = c("Cause_Name" = "GBD.term")
    ) %>%
      group_by(Parent_Name) %>%
      summarise(
        child_total_attention_score                  = sum(total_attention_score,                  na.rm = TRUE),
        child_nhits                                  = sum(nhits,                                  na.rm = TRUE),
        child_weighted_n                             = sum(weighted_n,                             na.rm = TRUE),
        child_weighted_nhits                         = sum(weighted_nhits,                         na.rm = TRUE),
        child_weighted_attention_score_impact_factor = sum(weighted_attention_score_impact_factor, na.rm = TRUE),
        .groups = "drop"
      )

    parents_rolled <- parents %>%
      left_join(children_scores, by = c("GBD.term" = "Parent_Name")) %>%
      left_join(cause_id_lookup, by = c("GBD.term" = "Cause_Name")) %>%
      mutate(
        total_attention_score                  = coalesce(total_attention_score, 0) +
                                                   coalesce(child_total_attention_score, 0),
        nhits                                  = coalesce(nhits, 0) +
                                                   coalesce(child_nhits, 0),
        weighted_n                             = coalesce(weighted_n, 0) +
                                                   coalesce(child_weighted_n, 0),
        weighted_nhits                         = coalesce(weighted_nhits, 0) +
                                                   coalesce(child_weighted_nhits, 0),
        weighted_attention_score_impact_factor = coalesce(weighted_attention_score_impact_factor, 0) +
                                                   coalesce(child_weighted_attention_score_impact_factor, 0)
      ) %>%
      select(GBD.term, cause_id, total_attention_score, nhits,
             weighted_n, weighted_nhits, weighted_attention_score_impact_factor)
  } else {
    parents_rolled <- tibble(
      GBD.term = character(), cause_id = integer(),
      total_attention_score = numeric(), nhits = numeric(),
      weighted_n = numeric(), weighted_nhits = numeric(),
      weighted_attention_score_impact_factor = numeric()
    )
  }

  # ── Combine leaf + rolled-up parent scores ────────────────────────────
  bind_rows(
    matched_h3 %>%
      rename(GBD.term = Cause_Name) %>%
      select(GBD.term, cause_id, total_attention_score,
             weighted_nhits, weighted_attention_score_impact_factor,
             weighted_n, nhit = nhits),
    parents_rolled %>%
      rename(nhit = nhits) %>%
      select(GBD.term, cause_id, total_attention_score,
             weighted_nhits, weighted_attention_score_impact_factor,
             weighted_n, nhit)
  ) %>%
    distinct(GBD.term, .keep_all = TRUE)
}

# ── Step 6 ─────────────────────────────────────────────────────────────────
build_temporal_scores <- function(attention, direct_map, descendant_map,
                                  exclude_causes, hierarchy_path,
                                  manual_trait_map = NULL) {
  years <- sort(unique(attention$pub_year[!is.na(attention$pub_year)]))

  year_scores <- lapply(years, function(yr) {
    message("  Year ", yr, "…")
    att_yr <- attention %>% filter(pub_year == yr)
    if (nrow(att_yr) == 0) return(NULL)
    leaves  <- map_attention_to_gbd_leaves(
      att_yr, direct_map, descendant_map, exclude_causes, manual_trait_map
    )
    rolled  <- rollup_hierarchy(leaves, hierarchy_path)
    rolled %>%
      filter(total_attention_score != 0) %>%
      transmute(
        cause_name            = GBD.term,
        cause_id              = cause_id,
        total_attention_score = total_attention_score,
        analysis_type         = "year",
        time_strata           = yr
      )
  })

  year_data <- bind_rows(Filter(Negate(is.null), year_scores))

  if (nrow(year_data) == 0) return(year_data)

  # ── Sliding 3-year windows ─────────────────────────────────────────────
  min_yr  <- min(year_data$time_strata)
  max_yr  <- max(year_data$time_strata) - 2

  if (max_yr >= min_yr) {
    sliding <- lapply(min_yr:max_yr, function(i) {
      year_data %>%
        filter(time_strata %in% c(i, i + 1, i + 2)) %>%
        group_by(cause_name, cause_id) %>%
        summarise(
          total_attention_score = sum(total_attention_score, na.rm = TRUE),
          analysis_type         = "sliding_3yr",
          time_strata           = i,
          .groups = "drop"
        )
    })
    sliding_data <- bind_rows(sliding)
  } else {
    sliding_data <- tibble(
      cause_name = character(), cause_id = integer(),
      total_attention_score = numeric(),
      analysis_type = character(), time_strata = integer()
    )
  }

  bind_rows(year_data, sliding_data)
}

build_temporal_scores_from_master <- function(attention, master_map,
                                              exclude_causes, hierarchy_path) {
  years <- sort(unique(attention$pub_year[!is.na(attention$pub_year)]))

  year_scores <- lapply(years, function(yr) {
    message("  Year ", yr, "…")
    att_yr <- attention %>% filter(pub_year == yr)
    if (nrow(att_yr) == 0) return(NULL)
    leaves <- map_attention_to_gbd_leaves_from_master(
      att_yr, master_map, exclude_causes
    )
    rolled <- rollup_hierarchy(leaves, hierarchy_path)
    rolled %>%
      filter(total_attention_score != 0) %>%
      transmute(
        cause_name = GBD.term,
        cause_id = cause_id,
        total_attention_score = total_attention_score,
        analysis_type = "year",
        time_strata = yr
      )
  })

  year_data <- bind_rows(Filter(Negate(is.null), year_scores))
  if (nrow(year_data) == 0) return(year_data)

  min_yr <- min(year_data$time_strata)
  max_yr <- max(year_data$time_strata) - 2

  if (max_yr >= min_yr) {
    sliding <- lapply(min_yr:max_yr, function(i) {
      year_data %>%
        filter(time_strata %in% c(i, i + 1, i + 2)) %>%
        group_by(cause_name, cause_id) %>%
        summarise(
          total_attention_score = sum(total_attention_score, na.rm = TRUE),
          analysis_type = "sliding_3yr",
          time_strata = i,
          .groups = "drop"
        )
    })
    sliding_data <- bind_rows(sliding)
  } else {
    sliding_data <- tibble(
      cause_name = character(), cause_id = integer(),
      total_attention_score = numeric(),
      analysis_type = character(), time_strata = integer()
    )
  }

  bind_rows(year_data, sliding_data)
}

# ── Step 7 ─────────────────────────────────────────────────────────────────
assemble_output <- function(all_scores, temporal_scores) {
  all_time <- all_scores %>%
    transmute(
      cause_name            = GBD.term,
      cause_id              = cause_id,
      total_attention_score = total_attention_score,
      analysis_type         = "all",
      time_strata           = NA_integer_
    )

  bind_rows(all_time, temporal_scores) %>%
    arrange(cause_name, analysis_type, time_strata)
}
