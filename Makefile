# ── GWAS-GBD pipeline ─────────────────────────────────────────────────────
# File-based targets: Make skips a step when its output is newer than all inputs.
#
# Normal run (after canonical mapping + LLM review are done):
#   make
#
# Full pipeline from scratch:
#   make canonical          # build evidence package (slow — EFO OBO + GWAS catalog)
#   # manually run LLM batch review (see below)
#   make                    # export → pipeline → reports
#
# LLM review (async, manual steps):
#   make llm-submit         # submit batch to Anthropic API
#   make llm-collect        # collect results once batch is complete
#
# Site and tests:
#   make artifacts          # build static JSON for the website
#   make test               # run all unit tests

RSCRIPT = Rscript --no-save --no-restore

# ── Source files ───────────────────────────────────────────────────────────
GWAS_CATALOG  = Data/gwas_catalog_v1.0.2.1-studies_r2026-06-01.tsv
HIERARCHY     = Data/IHME_GBD_2023_HIERARCHIES_Y2025M10D23.XLSX
FIRST_PART    = Data/First_part_GBD.xlsx
SECOND_PART   = Data/Second_part_GBD.xlsx
EFO_OBO       = Data/efo.obo

CANONICAL_R_SOURCES = $(wildcard scripts/canonical_mapping/R/*.R)
PIPELINE_FUNCTIONS  = scripts/pipeline_functions.R

# ── Intermediate and final outputs ────────────────────────────────────────
EVIDENCE_PKG      = outputs/canonical_mapping/04_evidence_package.csv
EVIDENCE_REVIEWED = outputs/canonical_mapping/04_evidence_package_reviewed.csv
MASTER_MAPPING    = Data/gbd_efo_master_mapping.tsv
ATTENTION_SCORES  = Data/merged_dataset_exclude_Injuries.csv
COMPARISON_HTML   = scripts/attention-score-pipeline-comparison.html
REGIONAL_CI_HTML  = scripts/regional_concentration_index/regional_concentration_index.html

# ── Default target ─────────────────────────────────────────────────────────
.PHONY: all
all: $(COMPARISON_HTML) $(REGIONAL_CI_HTML)

# ── Step 1: Evidence package (canonical mapping workflow) ──────────────────
# Runs workflow.R steps 1–4: observed term universe, GBD context, candidate
# generation, evidence package export. Slow (parses EFO OBO + GWAS catalog).
# Re-runs only when the GWAS catalog, hierarchy, reference files, or R sources change.
$(EVIDENCE_PKG): scripts/canonical_mapping/workflow.R $(CANONICAL_R_SOURCES) \
                 $(GWAS_CATALOG) $(HIERARCHY) $(FIRST_PART) $(SECOND_PART) $(EFO_OBO)
	$(RSCRIPT) scripts/canonical_mapping/workflow.R

.PHONY: canonical
canonical: $(EVIDENCE_PKG)

# ── Step 2: LLM batch review (async — manual steps) ───────────────────────
# Submit once; collect after Anthropic finishes (usually minutes to a few hours).
# These do NOT update $(EVIDENCE_REVIEWED) automatically; run them manually.
.PHONY: llm-submit
llm-submit: $(EVIDENCE_PKG)
	$(RSCRIPT) scripts/canonical_mapping/llm_batch_submit.R

.PHONY: llm-collect
llm-collect:
	$(RSCRIPT) scripts/canonical_mapping/llm_batch_collect.R

# ── Step 3: Export to master mapping ──────────────────────────────────────
# Converts accepted rows from the reviewed evidence package into the TSV format
# consumed by pipeline.R. Re-runs when the reviewed package or export script changes.
$(MASTER_MAPPING): scripts/canonical_mapping/export_to_master_mapping.R \
                   $(EVIDENCE_REVIEWED) $(CANONICAL_R_SOURCES)
	$(RSCRIPT) scripts/canonical_mapping/export_to_master_mapping.R

# ── Step 4: Attention score pipeline ──────────────────────────────────────
# Maps canonical EFO terms → GBD conditions, rolls up hierarchy, builds
# temporal (year + sliding_3yr) scores. Re-runs when master mapping, GWAS
# catalog, hierarchy, or pipeline functions change.
$(ATTENTION_SCORES): scripts/pipeline.R $(PIPELINE_FUNCTIONS) \
                     $(MASTER_MAPPING) $(GWAS_CATALOG) $(HIERARCHY)
	$(RSCRIPT) scripts/pipeline.R

.PHONY: pipeline
pipeline: $(ATTENTION_SCORES)

# ── Step 5: Comparison report ─────────────────────────────────────────────
$(COMPARISON_HTML): scripts/attention-score-pipeline-comparison.qmd $(ATTENTION_SCORES)
	cd scripts && quarto render attention-score-pipeline-comparison.qmd

# ── Step 6: Regional concentration index report ───────────────────────────
REGIONAL_R_SOURCES = $(wildcard scripts/regional_concentration_index/R/*.R)

$(REGIONAL_CI_HTML): scripts/regional_concentration_index/regional_concentration_index.qmd \
                     $(REGIONAL_R_SOURCES) $(ATTENTION_SCORES)
	cd scripts/regional_concentration_index && quarto render regional_concentration_index.qmd

.PHONY: reports
reports: $(COMPARISON_HTML) $(REGIONAL_CI_HTML)

# ── Site artifacts ─────────────────────────────────────────────────────────
.PHONY: artifacts
artifacts: $(ATTENTION_SCORES)
	$(RSCRIPT) site/R/build.R

.PHONY: build
build:
	cd site && npm run build

.PHONY: preview
preview:
	cd site && npm run preview

.PHONY: dev
dev:
	cd site && npm run dev

# ── Tests ──────────────────────────────────────────────────────────────────
.PHONY: test-canonical
test-canonical:
	$(RSCRIPT) scripts/canonical_mapping/tests/testthat.R

.PHONY: test-pipeline
test-pipeline:
	$(RSCRIPT) scripts/tests/testthat.R

.PHONY: test-r
test-r:
	$(RSCRIPT) site/tests/testthat.R

.PHONY: test-e2e
test-e2e:
	cd site && npx playwright test

.PHONY: test
test: test-canonical test-pipeline test-r

# ── Utilities ──────────────────────────────────────────────────────────────
.PHONY: help
help:
	@echo "Targets:"
	@echo "  all              (default) comparison + regional CI reports"
	@echo "  canonical        build evidence package from GWAS catalog + EFO OBO"
	@echo "  llm-submit       submit LLM batch review job to Anthropic"
	@echo "  llm-collect      collect LLM batch results when complete"
	@echo "  pipeline         rebuild attention scores from master mapping"
	@echo "  reports          render both comparison and regional CI HTML reports"
	@echo "  artifacts        build static JSON for the website"
	@echo "  test             run all R unit tests"
	@echo "  help             show this message"
