library(dplyr)
library(stringr)

# ── Issue 015: Embedding Candidate Channel ─────────────────────────────────
# Adds embedding-based retrieval as an additional candidate channel.
#
# Primary backend: BioBERT via embed_server.py (HTTP, local or remote GPU).
# Fallback: TF-IDF cosine similarity (no external dependencies, always works).
#
# Start the server before running:
#   python scripts/canonical_mapping/embed_server.py
# Or on a GPU node:
#   python embed_server.py --host 0.0.0.0 --port 8000

# ── TF-IDF fallback ────────────────────────────────────────────────────────

.tokenise <- function(text) {
  text <- tolower(trimws(as.character(text)))
  text <- gsub("[^a-z0-9 ]", " ", text)
  unique(unlist(strsplit(text, "\\s+")))
}

.build_tfidf_matrix <- function(docs) {
  vocab   <- sort(unique(unlist(lapply(docs, .tokenise))))
  n       <- length(docs)
  tf_rows <- lapply(docs, function(d) {
    tokens <- .tokenise(d)
    tbl    <- table(tokens)
    vec    <- setNames(rep(0, length(vocab)), vocab)
    vec[names(tbl)] <- as.numeric(tbl) / max(length(tokens), 1)
    vec
  })
  tf_mat  <- do.call(rbind, tf_rows)

  df_vec  <- colSums(tf_mat > 0)
  idf_vec <- log((n + 1) / (df_vec + 1)) + 1

  sweep(tf_mat, 2, idf_vec, "*")
}

.cosine_sim <- function(query_vec, matrix_rows) {
  qnorm <- sqrt(sum(query_vec^2))
  if (qnorm == 0) return(rep(0, nrow(matrix_rows)))
  mnorms <- sqrt(rowSums(matrix_rows^2))
  mnorms[mnorms == 0] <- 1
  as.numeric(matrix_rows %*% query_vec) / (mnorms * qnorm)
}

# ── BioBERT server backend ─────────────────────────────────────────────────

#' Check that the embed server is reachable.
#'
#' @param url Base URL of the embed server (e.g. "http://localhost:8000").
#' @return TRUE invisibly, or stops with a diagnostic message.
check_embed_server <- function(url = "http://localhost:8000") {
  if (!requireNamespace("httr", quietly = TRUE))
    stop("Package 'httr' is required to use the embed server.")
  resp <- tryCatch(
    httr::GET(paste0(url, "/health"), httr::timeout(5)),
    error = function(e)
      stop("Cannot reach embed server at ", url, ": ", conditionMessage(e),
           "\nStart it with: python scripts/canonical_mapping/embed_server.py")
  )
  if (httr::status_code(resp) != 200L)
    stop("Embed server unhealthy (HTTP ", httr::status_code(resp), "): ", url)
  info <- httr::content(resp, as = "parsed")
  message(sprintf("Embed server OK: model=%s  device=%s", info$model, info$device))
  invisible(TRUE)
}

# Embed a character vector via the server; returns a numeric matrix (n × dim).
# Texts are sent in batches of at most 256 (server's MAX_BATCH).
.embed_via_server <- function(texts, url, batch_size = 128L) {
  if (!requireNamespace("httr",     quietly = TRUE)) stop("httr required")
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("jsonlite required")

  n      <- length(texts)
  result <- vector("list", ceiling(n / batch_size))
  idx    <- 0L

  for (start in seq(1, n, by = batch_size)) {
    idx   <- idx + 1L
    batch <- texts[start:min(start + batch_size - 1L, n)]

    resp <- tryCatch(
      httr::POST(
        url     = paste0(url, "/embed"),
        httr::add_headers("content-type" = "application/json"),
        body    = jsonlite::toJSON(list(texts = batch), auto_unbox = FALSE),
        encode  = "raw",
        httr::timeout(120)
      ),
      error = function(e) stop("Embed server request failed: ", conditionMessage(e))
    )

    if (httr::status_code(resp) != 200L)
      stop("Embed server returned HTTP ", httr::status_code(resp), " for batch ", idx)

    parsed      <- httr::content(resp, as = "parsed", encoding = "UTF-8")
    result[[idx]] <- do.call(rbind, lapply(parsed$embeddings, as.numeric))
  }

  do.call(rbind, result)
}

# ── Public interface ───────────────────────────────────────────────────────

#' Add embedding-based candidate channel to an existing candidate table.
#'
#' @param existing_candidates Data frame from generate_deterministic_candidates().
#' @param gbd_context Output of build_gbd_condition_context().
#' @param observed_term_universe Output of build_observed_term_universe().
#' @param ontology_metadata Data frame: ontology_id, label, synonyms (optional),
#'   definition (optional).
#' @param top_k Maximum number of candidate terms to retrieve per condition.
#' @param min_similarity Minimum cosine similarity threshold.
#' @param embed_server_url Base URL of the BioBERT embed server, e.g.
#'   "http://localhost:8000". When NULL, TF-IDF is used as a fallback.
#' @param embed_fn Optional legacy callback: character vector → numeric matrix.
#'   Ignored when embed_server_url is set.
#' @return Candidate data frame with new "embedding" channel rows merged in.
add_embedding_candidates <- function(existing_candidates,
                                     gbd_context,
                                     observed_term_universe,
                                     ontology_metadata,
                                     top_k            = 10L,
                                     min_similarity   = 0.15,
                                     embed_server_url = NULL,
                                     embed_fn         = NULL) {
  obs_ids <- observed_term_universe$terms$ontology_id

  obs_meta <- ontology_metadata %>%
    filter(ontology_id %in% obs_ids) %>%
    mutate(
      text_for_embedding = paste(
        label,
        if ("synonyms"   %in% names(.)) coalesce(synonyms, "")   else "",
        if ("definition" %in% names(.)) coalesce(definition, "") else "",
        sep = " "
      )
    )

  if (nrow(obs_meta) == 0) return(existing_candidates)

  cond_docs <- gbd_context %>%
    mutate(
      query_text = paste(
        condition_name,
        coalesce(alias, ""),
        coalesce(scope_note, ""),
        sep = " "
      )
    ) %>%
    select(condition_name, query_text)

  term_texts  <- obs_meta$text_for_embedding
  term_ids    <- obs_meta$ontology_id
  term_labels <- obs_meta$label

  # ── Build similarity function ────────────────────────────────────────────

  if (!is.null(embed_server_url)) {
    message(sprintf("Embedding %d texts via BioBERT server: %s",
                    nrow(cond_docs) + length(term_texts), embed_server_url))
    all_texts  <- c(cond_docs$query_text, term_texts)
    all_embeds <- .embed_via_server(all_texts, embed_server_url)
    n_cond     <- nrow(cond_docs)
    cond_mat   <- all_embeds[seq_len(n_cond), , drop = FALSE]
    term_mat   <- all_embeds[seq(n_cond + 1L, nrow(all_embeds)), , drop = FALSE]
    sim_fn     <- function(i) .cosine_sim(cond_mat[i, ], term_mat)

  } else if (!is.null(embed_fn)) {
    all_texts  <- c(cond_docs$query_text, term_texts)
    all_embeds <- embed_fn(all_texts)
    n_cond     <- nrow(cond_docs)
    cond_mat   <- all_embeds[seq_len(n_cond), , drop = FALSE]
    term_mat   <- all_embeds[seq(n_cond + 1L, nrow(all_embeds)), , drop = FALSE]
    sim_fn     <- function(i) .cosine_sim(cond_mat[i, ], term_mat)

  } else {
    message("embed_server_url not set — using TF-IDF cosine similarity as fallback.")
    all_texts  <- c(cond_docs$query_text, term_texts)
    tfidf      <- .build_tfidf_matrix(all_texts)
    n_cond     <- nrow(cond_docs)
    cond_mat   <- tfidf[seq_len(n_cond), , drop = FALSE]
    term_mat   <- tfidf[seq(n_cond + 1L, nrow(tfidf)), , drop = FALSE]
    sim_fn     <- function(i) .cosine_sim(cond_mat[i, ], term_mat)
  }

  # ── Retrieve top-k per condition ─────────────────────────────────────────

  emb_rows <- lapply(seq_len(nrow(cond_docs)), function(i) {
    sims <- sim_fn(i)
    keep <- which(sims >= min_similarity)
    if (length(keep) == 0) return(NULL)

    ranked <- keep[order(sims[keep], decreasing = TRUE)]
    top    <- head(ranked, top_k)

    data.frame(
      gbd_condition  = cond_docs$condition_name[i],
      ontology_id    = term_ids[top],
      channel        = "embedding",
      channel_detail = paste0(
        "sim=", round(sims[top], 4),
        " rank=", seq_along(top),
        " label='", term_labels[top], "'"
      ),
      stringsAsFactors = FALSE
    )
  })

  new_rows <- bind_rows(Filter(Negate(is.null), emb_rows))
  if (nrow(new_rows) == 0) return(existing_candidates)

  combined <- bind_rows(existing_candidates, new_rows)
  combined %>%
    group_by(gbd_condition, ontology_id) %>%
    summarise(
      channels        = paste(sort(unique(channel)), collapse = "|"),
      channel_details = paste(sort(unique(channel_detail)), collapse = " ;; "),
      .groups = "drop"
    )
}
