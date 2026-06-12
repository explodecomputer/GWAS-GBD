library(here)
library(dplyr)
library(httr)
library(jsonlite)

# ── LLM Batch Review ───────────────────────────────────────────────────────
# Sends each row of the evidence package to Claude Haiku for automated review.
# Results are written to 04_evidence_package_reviewed.csv.
#
# Usage:
#   ANTHROPIC_API_KEY=sk-ant-... Rscript scripts/canonical_mapping/llm_batch_review.R
#
# Resumable: rows already filled (model_recommendation not NA) are skipped.
# Set REVIEW_LIMIT env var to process only N rows (useful for testing):
#   REVIEW_LIMIT=5 Rscript scripts/canonical_mapping/llm_batch_review.R

for (f in list.files(here("scripts/canonical_mapping/R"), pattern = "\\.R$",
                     full.names = TRUE)) {
  source(f)
}

# ── Configuration ──────────────────────────────────────────────────────────

evidence_pkg_path  <- here("outputs/canonical_mapping/04_evidence_package.csv")
output_path        <- here("outputs/canonical_mapping/04_evidence_package_reviewed.csv")

llm_model          <- "claude-haiku-4-5-20251001"
llm_max_tokens     <- 512L
requests_per_min   <- 50L          # stay under Haiku tier-1 RPM limit
retry_max          <- 3L
retry_wait_secs    <- 10L

api_key  <- Sys.getenv("ANTHROPIC_API_KEY")
limit    <- suppressWarnings(as.integer(Sys.getenv("REVIEW_LIMIT", "")))

if (nchar(api_key) == 0) stop("ANTHROPIC_API_KEY environment variable not set.")
if (is.na(limit)) limit <- Inf

# ── Load evidence package ──────────────────────────────────────────────────

if (!file.exists(evidence_pkg_path)) {
  stop("Evidence package not found: ", evidence_pkg_path,
       "\nRun workflow.R steps 1-4 first.")
}

pkg <- read.csv(evidence_pkg_path, stringsAsFactors = FALSE, na.strings = c("", "NA"))

# Resume: skip rows already reviewed
n_total    <- nrow(pkg)
todo_idx   <- which(is.na(pkg$model_recommendation))
n_todo     <- min(length(todo_idx), limit)

message(sprintf("Evidence package: %d rows total, %d need review", n_total, length(todo_idx)))
if (is.finite(limit)) message(sprintf("REVIEW_LIMIT=%d: processing first %d", limit, n_todo))
if (n_todo == 0) { message("Nothing to do."); quit(save = "no") }

# ── API call ───────────────────────────────────────────────────────────────

.call_haiku <- function(system_prompt, user_text, model, max_tokens,
                        api_key, retry_max, retry_wait_secs) {
  body <- list(
    model      = model,
    max_tokens = max_tokens,
    system     = system_prompt,
    messages   = list(list(role = "user", content = user_text))
  )

  for (attempt in seq_len(retry_max)) {
    resp <- tryCatch(
      httr::POST(
        url     = "https://api.anthropic.com/v1/messages",
        httr::add_headers(
          "x-api-key"         = api_key,
          "anthropic-version" = "2023-06-01",
          "content-type"      = "application/json"
        ),
        body   = jsonlite::toJSON(body, auto_unbox = TRUE),
        encode = "raw"
      ),
      error = function(e) list(.error = conditionMessage(e))
    )

    if (!is.null(resp$.error)) {
      if (attempt < retry_max) { Sys.sleep(retry_wait_secs); next }
      return(list(text = NULL, error = resp$.error))
    }

    status <- httr::status_code(resp)

    # Rate limited or server error — back off and retry
    if (status %in% c(429L, 529L, 500L, 503L)) {
      wait <- retry_wait_secs * attempt
      message(sprintf("    HTTP %d — waiting %ds (attempt %d/%d)", status, wait, attempt, retry_max))
      Sys.sleep(wait)
      next
    }

    if (status != 200L) {
      return(list(text = NULL, error = sprintf("HTTP %d: %s",
        status, substr(httr::content(resp, as = "text", encoding = "UTF-8"), 1, 200))))
    }

    parsed <- tryCatch(
      httr::content(resp, as = "parsed", encoding = "UTF-8"),
      error = function(e) NULL
    )

    text <- tryCatch(parsed$content[[1]]$text, error = function(e) NULL)
    return(list(text = text, error = NULL))
  }

  list(text = NULL, error = "max_retries_exceeded")
}

# ── Main loop ─────────────────────────────────────────────────────────────

interval_secs <- 60 / requests_per_min
n_done  <- 0L
n_error <- 0L

for (i in seq_len(n_todo)) {
  idx <- todo_idx[i]
  row <- pkg[idx, , drop = FALSE]

  prompt <- format_llm_review_prompt(row)

  t0   <- proc.time()[["elapsed"]]
  api  <- .call_haiku(prompt$system, prompt$user, llm_model, llm_max_tokens,
                      api_key, retry_max, retry_wait_secs)
  elapsed <- proc.time()[["elapsed"]] - t0

  rec <- parse_llm_review_response(api$text)

  if (!is.null(api$error) || !is.na(rec$parse_error)) {
    err_msg <- coalesce(api$error, rec$parse_error)
    message(sprintf("[%d/%d] row_id=%s  ERROR: %s", i, n_todo, row$row_id, err_msg))
    pkg$model_recommendation[idx] <- NA_character_
    n_error <- n_error + 1L
  } else {
    pkg$model_recommendation[idx] <- rec$recommendation
    pkg$model_relationship[idx]   <- rec$relationship_label
    pkg$model_rationale[idx]      <- rec$rationale
    pkg$model_confidence[idx]     <- rec$confidence
    n_done <- n_done + 1L
    message(sprintf("[%d/%d] row_id=%-60s  %s (%.2f)",
                    i, n_todo, row$row_id, rec$recommendation, rec$confidence))
  }

  # Save after every row so the run is resumable
  write.csv(pkg, output_path, row.names = FALSE, na = "")

  # Rate limiting: wait out the remainder of the per-request interval
  spent <- proc.time()[["elapsed"]] - t0
  pause <- interval_secs - spent
  if (pause > 0) Sys.sleep(pause)
}

# ── Summary ────────────────────────────────────────────────────────────────

message(sprintf("\nDone. %d reviewed, %d errors. Output: %s", n_done, n_error, output_path))

rec_tbl <- table(pkg$model_recommendation[todo_idx[seq_len(n_todo)]], useNA = "ifany")
print(rec_tbl)
