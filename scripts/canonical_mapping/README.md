# Canonical Mapping Workflow

This directory implements the canonical GBD-EFO mapping workflow described in `issues/prd.md` (issues 012–024). It replaces automatic ontology hierarchy expansion with an auditable, catalog-release-specific, human-reviewed mapping.

## Problem it solves

The previous pipeline used automatic ontology hierarchy expansion for scoring, which produced amorphous condition mappings, distorted attention-burden alignment, and could not be cleanly reproduced. This workflow instead:

1. Limits accepted mappings to **observed** GWAS Catalog ontology terms.
2. Requires **human acceptance** before any term contributes to attention scores.
3. Produces a **canonical mapping versioned by GWAS Catalog release**.
4. Separates candidate generation, review, and scoring into distinct artifacts.

## Key concepts

See `CONTEXT.md` for the project glossary. The most important distinctions:

| Term | Meaning |
|------|---------|
| **Observed term** | An ontology ID present in a specific GWAS Catalog release's `MAPPED_TRAIT_URI` column. |
| **Candidate term** | A proposed mapping found by a channel (lexical, embedding, legacy, etc.). Not yet accepted. |
| **Canonical mapping** | Human-accepted `(GBD condition × ontology term)` pairs for one release. The scoring source of truth. |
| **Reference dataset** | `_updated6` or similar — used for comparison, never as truth. |

## Why no automatic hierarchy expansion for scoring

Hierarchy expansion adds every descendant of a root ontology term to a GBD condition's mapping. This creates over-broad mappings: a single root EFO term can match thousands of catalog studies, inflating attention for conditions that map to general categories. The canonical workflow uses hierarchy only to **propose candidates** for human review; hierarchy relationships cannot create accepted mappings or scores.

Parent GBD condition scores are produced by **GBD hierarchy rollup** (summing child scores), not by duplicate direct ontology mappings.

## Residual exclusivity and specificity conflicts

A **residual GBD condition** (name starts with "Other …") must not accept any ontology term already accepted by a more specific sibling condition in the same GBD group. This prevents the residual from absorbing attention that belongs to named conditions.

A **specificity conflict** occurs when the same term would be accepted for both a parent and a child GBD condition. The child wins by default; the parent's score comes from rollup.

## Directory structure

```
R/
  observed_term_universe.R    # Issue 012
  gbd_condition_context.R     # Issue 013
  candidate_generator.R       # Issue 014
  embedding_candidate.R       # Issue 015
  evidence_package.R          # Issue 016
  quality_gates.R             # Issue 017
  llm_review_adapter.R        # Issue 018
  human_review_compiler.R     # Issue 019
  attention_scorer.R          # Issue 020
  diagnostic_report.R         # Issue 021
  drift_report.R              # Issue 022
tests/
  testthat/                   # One test file per module
  testthat.R                  # Test runner
workflow.R                    # End-to-end orchestration script
README.md                     # This file (Issue 023)
```

## End-to-end workflow

### 1. Build the observed term universe (Issue 012)

```r
source("scripts/canonical_mapping/R/observed_term_universe.R")
universe <- build_observed_term_universe(
  gwas_catalog_path = "Data/december2025/gwas_catalog.tsv",
  catalog_release   = "december2025"
)
# universe$terms: one row per observed ontology ID
# universe$metadata: counts of rows, blank URIs, malformed IDs
```

### 2. Build GBD condition context (Issue 013)

```r
source("scripts/canonical_mapping/R/gbd_condition_context.R")
gbd_context <- build_gbd_condition_context("Data/IHME_GBD_2023_HIERARCHIES.XLSX")
# Editable columns: alias, excluded_alias, scope_note
# These improve candidate recall but do not create accepted mappings directly
```

### 3. Generate deterministic candidates (Issue 014)

```r
source("scripts/canonical_mapping/R/candidate_generator.R")
candidates <- generate_deterministic_candidates(
  observed_term_universe = universe,
  gbd_context            = gbd_context,
  gwas_catalog_raw       = gwas_raw_df,        # for gwas_trait channel
  ontology_labels        = ont_labels_df,      # label, ontology_id columns
  first_part_path        = "Data/First_part_GBD.xlsx",
  second_part_path       = "Data/Second_part_GBD.xlsx"
)
# One row per (gbd_condition × ontology_id): channels, channel_details
```

### 4. Add embedding candidates (Issue 015, optional)

```r
source("scripts/canonical_mapping/R/embedding_candidate.R")
candidates <- add_embedding_candidates(
  existing_candidates    = candidates,
  gbd_context            = gbd_context,
  observed_term_universe = universe,
  ontology_metadata      = ont_metadata_df   # label, synonyms, definition
)
```

### 5. Build evidence package (Issue 016)

```r
source("scripts/canonical_mapping/R/evidence_package.R")
pkg <- build_evidence_package(candidates, universe, gbd_context, ont_metadata_df)
write.csv(pkg, "outputs/canonical_mapping/04_evidence_package.csv", row.names = FALSE)
```

The evidence package has one row per candidate decision with empty `human_decision`, `human_relationship`, `human_notes` columns ready for filling.

### 6. LLM-assisted review (Issue 018, optional)

```r
source("scripts/canonical_mapping/R/llm_review_adapter.R")
# For each row in pkg:
prompt <- format_llm_review_prompt(pkg[i, ])
# Send prompt$system + prompt$user to Claude API
# response_text <- call_claude(prompt)
rec <- parse_llm_review_response(response_text)
# Recommendations are stored in model_recommendation; they are NOT canonical.
pkg <- apply_llm_recommendations(pkg, recommendations_list)
```

### 7. Human review

Open `04_evidence_package.csv` in a spreadsheet editor. For each row, fill:
- `human_decision`: `accept`, `reject`, or `unsure`
- `human_relationship`: `exact`, `subtype`, `broader_grouping`, `narrower_grouping`, `association_only`, or `unrelated`
- `human_notes`: free text
- `reviewer_id`: your identifier

LLM recommendations in `model_recommendation` are suggestions only — they are never automatically canonical.

### 8. Compile human review (Issue 019)

```r
source("scripts/canonical_mapping/R/human_review_compiler.R")
reviewed_pkg <- read.csv("outputs/canonical_mapping/04_evidence_package_reviewed.csv")
compiled <- compile_human_review(reviewed_pkg, universe, gbd_context,
                                  sentinel_conditions = c("Type 1 diabetes", ...))
# compiled$canonical: scoring-ready accepted mappings
# compiled$audit_trail: full package with decision statuses
# compiled$summary: counts + gate report
```

### 9. Quality gates (Issue 017)

Quality gates run automatically inside `compile_human_review`. They also run standalone:

```r
source("scripts/canonical_mapping/R/quality_gates.R")
gate_result <- run_mapping_quality_gates(canonical, universe, gbd_context)
cat(gate_result$report)
```

Hard gates (FAIL = canonical cannot be used for scoring):
- `required_fields`: `gbd_condition` and `ontology_id` must be present
- `observed_only`: accepted terms must be in the observed universe
- `no_duplicate_edges`: no repeated `(condition × term)` decisions
- `residual_exclusivity`: "Other …" conditions cannot share terms with specific siblings
- `sentinel_non_zero`: configured known-active conditions must have ≥ 1 accepted mapping

Warning gates (WARN = human should inspect, scoring can proceed):
- `specificity_conflicts`: same term accepted for both parent and child condition

### 10. Score GWAS attention (Issue 020)

```r
source("scripts/canonical_mapping/R/attention_scorer.R")
canonical <- canonical_mapping_for_scoring(compiled)   # errors if gates fail
scores  <- score_canonical_attention(canonical, gwas_catalog_path)
temporal <- build_canonical_temporal_scores(canonical, gwas_catalog_path)
rolled   <- rollup_canonical_hierarchy(scores, hierarchy_path)
```

### 11. Diagnostic QC report (Issue 021)

```r
source("scripts/canonical_mapping/R/diagnostic_report.R")
diag <- generate_diagnostic_report(canonical, scores, temporal,
                                    reference_scores = updated6_scores)
print(diag)
```

Diagnostic checks are separate from quality gates and do not block scoring:
- Conditions rescued from zero (new vs `_updated6`)
- Conditions that lose attention
- Broad-term dominance (inflation warning)
- Temporal attention trend
- SDI gradient (High vs Low SDI)

### 12. Release drift report (Issue 022)

When upgrading to a new GWAS Catalog release:

```r
source("scripts/canonical_mapping/R/drift_report.R")
new_universe <- build_observed_term_universe(new_catalog_path, "january2026")
report <- generate_drift_report(old_universe, new_universe, old_canonical)
cat(report$report)
# report$mapping_drift: carry_forward / review_replacement / needs_review per accepted mapping
```

## Running the tests

```r
Rscript scripts/canonical_mapping/tests/testthat.R
```

## Running the full workflow

```r
Rscript scripts/canonical_mapping/workflow.R
```

Edit the configuration section at the top of `workflow.R` to set paths and release name.
