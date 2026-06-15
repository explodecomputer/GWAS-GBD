library(here)
library(dplyr)
library(httr)
library(jsonlite)
source(here("scripts/config.R"))

# ── LLM Batch Submit ───────────────────────────────────────────────────────
# Submits all unreviewed evidence package rows to the Anthropic Message
# Batches API in a single request (~50% cheaper than real-time, no rate limit).
# Results are retrieved later with llm_batch_collect.R.
#
# Usage:
#   ANTHROPIC_API_KEY=sk-ant-... Rscript scripts/canonical_mapping/llm_batch_submit.R
#
# The batch ID is saved to outputs/canonical_mapping/batch_state.json so
# llm_batch_collect.R can find it.

for (f in list.files(here("scripts/canonical_mapping/R"), pattern = "\\.R$",
                     full.names = TRUE)) {
  source(f)
}

# ── Configuration ──────────────────────────────────────────────────────────

cfg <- load_project_config()
evidence_pkg_path <- cfg_path(cfg_get(cfg, "canonical_mapping.evidence_package"), cfg)
output_path       <- cfg_path(cfg_get(cfg, "canonical_mapping.reviewed_evidence_package"), cfg)
state_path        <- cfg_path(cfg_get(cfg, "canonical_mapping.batch_state"), cfg)

llm_model      <- cfg_get(cfg, "services.llm_model")
llm_max_tokens <- 512L

api_key <- Sys.getenv("ANTHROPIC_API_KEY")
if (nchar(api_key) == 0) stop("ANTHROPIC_API_KEY environment variable not set.")

# ── Load evidence package ──────────────────────────────────────────────────

if (!file.exists(evidence_pkg_path)) {
  stop("Evidence package not found: ", evidence_pkg_path)
}

pkg      <- read.csv(evidence_pkg_path, stringsAsFactors = FALSE,
                     na.strings = c("", "NA"))
todo_idx <- which(is.na(pkg$model_recommendation))

message(sprintf("Evidence package: %d rows total, %d need review", nrow(pkg), length(todo_idx)))
if (length(todo_idx) == 0) { message("Nothing to submit."); quit(save = "no") }

# ── Build batch requests ───────────────────────────────────────────────────
# custom_id encodes the row number so collect can write results back without
# a separate lookup table. Format: "pkg_<row_number>" (alphanumeric + underscore).

message(sprintf("Building %d batch requests...", length(todo_idx)))

requests <- lapply(todo_idx, function(idx) {
  row    <- pkg[idx, , drop = FALSE]
  prompt <- format_llm_review_prompt(row)
  list(
    custom_id = paste0("pkg_", idx),
    params    = list(
      model    = llm_model,
      max_tokens = llm_max_tokens,
      system   = prompt$system,
      messages = list(list(role = "user", content = prompt$user))
    )
  )
})

# ── Submit to Anthropic Batches API ───────────────────────────────────────

message("Submitting batch to Anthropic API...")

resp <- httr::POST(
  url  = "https://api.anthropic.com/v1/messages/batches",
  httr::add_headers(
    "x-api-key"         = api_key,
    "anthropic-version" = "2023-06-01",
    "anthropic-beta"    = "message-batches-2024-09-24",
    "content-type"      = "application/json"
  ),
  body   = jsonlite::toJSON(list(requests = requests), auto_unbox = TRUE),
  encode = "raw"
)

if (httr::status_code(resp) != 200L) {
  stop(sprintf("Batch submission failed (HTTP %d): %s",
               httr::status_code(resp),
               httr::content(resp, as = "text", encoding = "UTF-8")))
}

batch <- httr::content(resp, as = "parsed", encoding = "UTF-8")

message(sprintf("Batch submitted: id=%s  status=%s  requests=%d",
                batch$id, batch$processing_status, length(requests)))

# ── Save state ─────────────────────────────────────────────────────────────

state <- list(
  batch_id          = batch$id,
  submitted_at      = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  n_requests        = length(requests),
  evidence_pkg_path = evidence_pkg_path,
  output_path       = output_path,
  llm_model         = llm_model
)
write(jsonlite::toJSON(state, auto_unbox = TRUE, pretty = TRUE), state_path)

message(sprintf("State saved to: %s", state_path))
message("Run llm_batch_collect.R when the batch is complete (usually minutes to hours).")
