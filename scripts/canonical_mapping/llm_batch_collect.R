library(here)
library(dplyr)
library(httr)
library(jsonlite)

# ── LLM Batch Collect ──────────────────────────────────────────────────────
# Checks the status of a submitted Anthropic batch and, when complete,
# downloads results and writes them back into the evidence package.
#
# Usage:
#   ANTHROPIC_API_KEY=sk-ant-... Rscript scripts/canonical_mapping/llm_batch_collect.R
#
# Safe to re-run: exits cleanly if still processing; writes output only when
# the batch has ended. Reads batch_state.json written by llm_batch_submit.R.

for (f in list.files(here("scripts/canonical_mapping/R"), pattern = "\\.R$",
                     full.names = TRUE)) {
  source(f)
}

# ── Configuration ──────────────────────────────────────────────────────────

state_path <- here("outputs/canonical_mapping/batch_state.json")

api_key <- Sys.getenv("ANTHROPIC_API_KEY")
if (nchar(api_key) == 0) stop("ANTHROPIC_API_KEY environment variable not set.")

# ── Load state ─────────────────────────────────────────────────────────────

if (!file.exists(state_path)) {
  stop("No batch state found at: ", state_path,
       "\nRun llm_batch_submit.R first.")
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

state <- jsonlite::fromJSON(state_path)
batch_id          <- state$batch_id
evidence_pkg_path <- state$evidence_pkg_path
output_path       <- state$output_path

message(sprintf("Checking batch: %s  (submitted %s)", batch_id, state$submitted_at))

# ── Check batch status ─────────────────────────────────────────────────────

status_resp <- httr::GET(
  url = sprintf("https://api.anthropic.com/v1/messages/batches/%s", batch_id),
  httr::add_headers(
    "x-api-key"         = api_key,
    "anthropic-version" = "2023-06-01",
    "anthropic-beta"    = "message-batches-2024-09-24"
  )
)

if (httr::status_code(status_resp) != 200L) {
  stop(sprintf("Status check failed (HTTP %d): %s",
               httr::status_code(status_resp),
               httr::content(status_resp, as = "text", encoding = "UTF-8")))
}

batch <- httr::content(status_resp, as = "parsed", encoding = "UTF-8")
counts <- batch$request_counts

message(sprintf("Status: %s  |  processing=%d  succeeded=%d  errored=%d  expired=%d",
                batch$processing_status,
                counts$processing %||% 0L,
                counts$succeeded  %||% 0L,
                counts$errored    %||% 0L,
                counts$expired    %||% 0L))

if (batch$processing_status != "ended") {
  message("Batch not yet complete. Re-run this script later to collect results.")
  quit(save = "no")
}

# ── Download results ───────────────────────────────────────────────────────

message("Batch complete. Downloading results...")

results_url <- batch$results_url %||%
  sprintf("https://api.anthropic.com/v1/messages/batches/%s/results", batch_id)

results_resp <- httr::GET(
  url = results_url,
  httr::add_headers(
    "x-api-key"         = api_key,
    "anthropic-version" = "2023-06-01",
    "anthropic-beta"    = "message-batches-2024-09-24"
  )
)

if (httr::status_code(results_resp) != 200L) {
  stop(sprintf("Results download failed (HTTP %d): %s",
               httr::status_code(results_resp),
               httr::content(results_resp, as = "text", encoding = "UTF-8")))
}

results_text <- httr::content(results_resp, as = "text", encoding = "UTF-8")
result_lines <- Filter(nchar, strsplit(results_text, "\n")[[1]])
message(sprintf("Downloaded %d result lines.", length(result_lines)))

# ── Parse results ──────────────────────────────────────────────────────────

pkg <- read.csv(evidence_pkg_path, stringsAsFactors = FALSE, na.strings = c("", "NA"))

n_applied <- 0L
n_error   <- 0L
n_skipped <- 0L

for (line in result_lines) {
  item <- tryCatch(jsonlite::fromJSON(line, simplifyVector = FALSE),
                   error = function(e) NULL)
  if (is.null(item)) { n_error <- n_error + 1L; next }

  # Recover the package row index from the custom_id ("pkg_<idx>")
  idx <- suppressWarnings(as.integer(sub("^pkg_", "", item$custom_id)))
  if (is.na(idx) || idx < 1L || idx > nrow(pkg)) {
    message("  Unrecognised custom_id: ", item$custom_id)
    n_skipped <- n_skipped + 1L
    next
  }

  result_type <- item$result$type

  if (result_type != "succeeded") {
    message(sprintf("  Row %d: result type=%s — skipping", idx, result_type))
    n_error <- n_error + 1L
    next
  }

  response_text <- tryCatch(
    item$result$message$content[[1]]$text,
    error = function(e) NULL
  )

  rec <- parse_llm_review_response(response_text)

  if (!is.na(rec$parse_error)) {
    message(sprintf("  Row %d: parse error — %s", idx, rec$parse_error))
    n_error <- n_error + 1L
    next
  }

  pkg$model_recommendation[idx] <- rec$recommendation
  pkg$model_relationship[idx]   <- rec$relationship_label
  pkg$model_rationale[idx]      <- rec$rationale
  pkg$model_confidence[idx]     <- rec$confidence
  n_applied <- n_applied + 1L
}

# ── Write output ───────────────────────────────────────────────────────────

write.csv(pkg, output_path, row.names = FALSE, na = "")

message(sprintf("\nApplied: %d  Errors/skipped: %d  Output: %s",
                n_applied, n_error + n_skipped, output_path))

rec_tbl <- table(pkg$model_recommendation, useNA = "ifany")
print(rec_tbl)
