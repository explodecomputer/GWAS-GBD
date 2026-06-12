library(here)
library(dplyr)

# ── Canonical Mapping Workflow ─────────────────────────────────────────────
# End-to-end orchestration for one GWAS Catalog release.
# See issues/prd.md and issues/023-mapping-workflow-documentation.md.

for (f in list.files(here("scripts/canonical_mapping/R"), pattern = "\\.R$",
                     full.names = TRUE)) {
  source(f)
}

# ── Configuration ──────────────────────────────────────────────────────────
# Edit paths here or pass as environment variables.

gwas_catalog_path    <- here("Data/gwas_catalog_v1.0.2.1-studies_r2026-06-01.tsv")
hierarchy_path       <- here("Data/IHME_GBD_2023_HIERARCHIES_Y2025M10D23.XLSX")
first_part_path      <- here("Data/First_part_GBD.xlsx")
second_part_path     <- here("Data/Second_part_GBD.xlsx")
efo_obo_path         <- here("Data/efo.obo")
output_dir           <- here("outputs/canonical_mapping")
catalog_release      <- "2026-06-01"

# Set to NULL to fall back to TF-IDF; or "http://<gpu-host>:8000" for remote.
embed_server_url     <- Sys.getenv("EMBED_SERVER_URL", unset = "http://localhost:8000")

sentinel_conditions <- c(
  "Type 1 diabetes mellitus",
  "Type 2 diabetes mellitus",
  "Breast cancer"
)

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# ── Step 1: Observed catalog term universe (Issue 012) ────────────────────
message("Step 1: Building observed term universe...")
universe <- build_observed_term_universe(gwas_catalog_path,
                                          catalog_release = catalog_release)
message(sprintf("  Observed terms: %d", universe$metadata$n_observed_terms))
message(sprintf("  Blank URIs excluded: %d", universe$metadata$n_blank_uri_rows))
write.csv(universe$terms,
          file.path(output_dir, "01_observed_term_universe.csv"),
          row.names = FALSE)

# ── Step 2: GBD condition context (Issue 013) ─────────────────────────────
message("Step 2: Building GBD condition context...")
.pf <- new.env(parent = baseenv())
source(here("scripts/pipeline_functions.R"), local = .pf)
gbd_context <- build_gbd_condition_context(hierarchy_path,
                                            exclude_causes = .pf$EXCLUDE_CAUSES)
rm(.pf)
message(sprintf("  Conditions: %d (%d residual)",
                nrow(gbd_context), sum(gbd_context$is_residual)))
write.csv(gbd_context,
          file.path(output_dir, "02_gbd_condition_context.csv"),
          row.names = FALSE)

# ── Step 3: Deterministic candidate generation (Issue 014) ────────────────
message("Step 3: Generating deterministic candidates...")

# Load ontology labels from OBO if available
ontology_labels <- NULL
if (file.exists(efo_obo_path)) {
  message("  Parsing EFO OBO for ontology labels...")
  ont <- ontologyIndex::get_OBO(efo_obo_path)
  ontology_labels <- data.frame(
    ontology_id = toupper(gsub("^efo:", "", ont$id, ignore.case = TRUE)),
    label       = coalesce(ont$name, ont$id),
    stringsAsFactors = FALSE
  ) %>%
    mutate(ontology_id = gsub("_", ":", ontology_id))
  # Also add synonyms if available
  if (!is.null(ont$synonym)) {
    syn_df <- lapply(names(ont$synonym), function(id) {
      syns <- ont$synonym[[id]]
      if (length(syns) == 0) return(NULL)
      data.frame(
        ontology_id = toupper(gsub("_", ":", gsub("^efo:", "", id, ignore.case = TRUE))),
        label       = syns,
        stringsAsFactors = FALSE
      )
    })
    ontology_labels <- bind_rows(ontology_labels, bind_rows(Filter(Negate(is.null), syn_df)))
  }
}

# Load raw GWAS catalog for trait-label channel
gwas_raw <- tryCatch(
  data.table::fread(gwas_catalog_path, sep = "\t", quote = "",
                    data.table = FALSE, colClasses = "character"),
  error = function(e) NULL
)

candidates <- generate_deterministic_candidates(
  observed_term_universe = universe,
  gbd_context            = gbd_context,
  gwas_catalog_raw       = gwas_raw,
  ontology_labels        = ontology_labels,
  first_part_path        = first_part_path,
  second_part_path       = second_part_path
)
message(sprintf("  Candidates: %d (gbd_condition × ontology_id pairs)",
                nrow(candidates)))
write.csv(candidates,
          file.path(output_dir, "03_deterministic_candidates.csv"),
          row.names = FALSE)

# ── Step 3b: Embedding candidates (Issue 015) ─────────────────────────────
# Requires the BioBERT embed server to be running. Falls back to TF-IDF when
# the server is unreachable — set embed_server_url <- NULL to skip explicitly.
if (!is.null(ontology_labels) && !is.null(embed_server_url)) {
  message("Step 3b: Adding embedding candidates via BioBERT server...")
  server_ok <- tryCatch(
    { check_embed_server(embed_server_url); TRUE },
    error = function(e) { message("  ", conditionMessage(e)); FALSE }
  )
  if (server_ok) {
    ont_meta_emb <- ontology_labels %>%
      distinct(ontology_id, label) %>%
      rename(synonyms = label)   # ontology_labels has one row per (id, label/synonym)
    candidates <- add_embedding_candidates(
      existing_candidates    = candidates,
      gbd_context            = gbd_context,
      observed_term_universe = universe,
      ontology_metadata      = ont_meta_emb,
      top_k                  = 10L,
      min_similarity         = 0.70,   # BioBERT embeddings are normalised; cosine ≥ 0.70
      embed_server_url       = embed_server_url
    )
    message(sprintf("  After embedding: %d candidates", nrow(candidates)))
    write.csv(candidates,
              file.path(output_dir, "03b_embedding_candidates.csv"),
              row.names = FALSE)
  }
} else if (is.null(embed_server_url)) {
  message("Step 3b: embed_server_url is NULL — skipping embedding channel.")
}

# ── Step 4: Evidence package export (Issue 016) ───────────────────────────
message("Step 4: Building evidence package...")

# Ontology metadata for evidence package (label, definition, is_obsolete)
ont_metadata <- NULL
if (!is.null(ontology_labels) && !is.null(ont)) {
  obs_ids <- universe$terms$ontology_id
  ont_metadata <- data.frame(
    ontology_id = toupper(gsub("_", ":", gsub("^efo:", "", ont$id, ignore.case = TRUE))),
    label       = coalesce(ont$name, ""),
    is_obsolete = coalesce(ont$obsolete, FALSE),
    stringsAsFactors = FALSE
  ) %>%
    filter(ontology_id %in% obs_ids) %>%
    distinct(ontology_id, .keep_all = TRUE)
}

pkg <- build_evidence_package(
  candidates             = candidates,
  observed_term_universe = universe,
  gbd_context            = gbd_context,
  ontology_metadata      = ont_metadata
)
message(sprintf("  Evidence package rows: %d", nrow(pkg)))
write.csv(pkg,
          file.path(output_dir, "04_evidence_package.csv"),
          row.names = FALSE)

message(sprintf("\nEvidence package written to: %s/04_evidence_package.csv", output_dir))
message("Next step: Review the evidence package, fill human_decision column,")
message("  then run compile_human_review() to produce the canonical mapping.")

# ── Steps 5-6: Human review compilation and quality gates (Issues 017, 019) -
# These run after human review. Uncomment when human_decision is filled.

# reviewed_pkg <- read.csv(file.path(output_dir, "04_evidence_package_reviewed.csv"))
# compiled <- compile_human_review(
#   reviewed_pkg, universe, gbd_context, sentinel_conditions
# )
# message(compiled$summary$gate_report)
# if (compiled$summary$gates_passed) {
#   write.csv(compiled$canonical,
#             file.path(output_dir, "05_canonical_mapping.csv"), row.names = FALSE)
#   message("Canonical mapping written.")
# }

# ── Step 7: Attention scoring (Issue 020) ─────────────────────────────────
# canonical <- read.csv(file.path(output_dir, "05_canonical_mapping.csv"))
# scores  <- score_canonical_attention(canonical, gwas_catalog_path)
# temporal <- build_canonical_temporal_scores(canonical, gwas_catalog_path)
# rolled  <- rollup_canonical_hierarchy(scores, hierarchy_path)

# ── Step 8: Diagnostic report (Issue 021) ─────────────────────────────────
# diag <- generate_diagnostic_report(canonical, scores, temporal)
# print(diag)
