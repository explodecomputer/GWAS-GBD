# Active File and Artifact Inventory

Status: active cleanup record.

This inventory classifies the active GWAS-GBD workflow after the Snakemake/config
cleanup. Deprecated files that have already been moved are recorded below with
their archive destinations.

## Active Source

| Path | Owner | Classification | Notes |
| --- | --- | --- | --- |
| `workflow/Snakefile` | Workflow spine | Active source | Authoritative DAG for mapping, scoring, reports, and site artifacts. |
| `config/config.yaml` | Configuration | Active source | Committed defaults for paths, releases, parameters, and service URLs. |
| `scripts/attention_scores/config.R` | Configuration | Active source | R config loader and local override merger. |
| `scripts/canonical_mapping/R/*.R` | Canonical mapping | Active source | Observed universe, GBD context, candidates, review compiler, gates, diagnostics. |
| `scripts/canonical_mapping/workflow.R` | Canonical mapping | Active source | Builds canonical mapping review artifacts. |
| `scripts/canonical_mapping/export_to_master_mapping.R` | Canonical mapping | Active source | Exports compiler-approved canonical mappings to the attention contract. |
| `scripts/pipeline.R` | Attention scoring | Active source | CLI entrypoint for configured attention score generation. |
| `scripts/attention_scores/attention_score_pipeline.R` | Attention scoring | Active source | Pipeline boundary from configured inputs to declared outputs. |
| `scripts/attention_scores/pipeline_functions.R` | Attention scoring | Active source | Lower-level scoring, hierarchy, and temporal helpers. |
| `scripts/attention_score_report/attention-score-pipeline-comparison.qmd` | Pipeline checking | Active source | Comparison report; writes declared zero-attention TODO artifact. |
| `scripts/regional_analysis/R/*.R` | Regional analysis | Active source | Config-driven loaders, metrics, analysis sets, and plotting helpers. |
| `scripts/regional_analysis/regional_concentration_index.qmd` | Regional analysis | Active source | Config-driven regional report and declared figure exports. |
| `site/R/*.R` | Site artifacts | Active source | Builds and validates static JSON artifacts. |
| `site/src/**` | Frontend | Active source | Vue application. |
| `site/tests/**`, `scripts/attention_scores/tests/**`, `scripts/canonical_mapping/tests/**` | Tests | Active source | Unit, fixture, contract, and smoke tests. |

## Raw Inputs

| Path | Classification | Notes |
| --- | --- | --- |
| `data/gwas_catalog_v1.0.2.1-studies_r2026-06-01.tsv` | Raw input | Current configured GWAS Catalog release. |
| `data/IHME_GBD_2023_HIERARCHIES_Y2025M10D23.XLSX` | Raw input | Current configured GBD hierarchy workbook. |
| `data/First_part_GBD.xlsx`, `data/Second_part_GBD.xlsx` | Raw input | Legacy/manual GBD-EFO source workbooks for candidate generation. |
| `data/efo.obo` | Raw input | Ontology source for candidate generation and drift/obsolete checks. |
| `data/manual_zero_attention_mapping.csv` | Raw input | Manual material input to attention scoring. |
| `data/december2025/gbd_gwas_paper_data_*.csv` | Raw input | Current configured burden extracts for regional and site artifacts. |

Note: the project config uses lowercase `data/` paths. On the current macOS
checkout, `Data/` and `data/` resolve to the same directory; the policy is still
that this directory should contain raw/reference inputs only.

## Generated Intermediates

| Path | Classification | Notes |
| --- | --- | --- |
| `outputs/canonical_mapping/01_observed_term_universe.csv` | Generated intermediate | Produced by canonical mapping workflow. |
| `outputs/canonical_mapping/02_gbd_condition_context.csv` | Generated intermediate | Editable review context; active source after human edits. |
| `outputs/canonical_mapping/03_deterministic_candidates.csv` | Generated intermediate | Candidate review input. |
| `outputs/canonical_mapping/03b_embedding_candidates.csv` | Generated intermediate | Optional embedding candidate channel output. |
| `outputs/canonical_mapping/04_evidence_package.csv` | Generated intermediate | Unreviewed candidate evidence package. |
| `outputs/canonical_mapping/batch_state.json` | Generated intermediate | LLM batch state; local/run specific unless intentionally shared. |
| `outputs/pipeline_checks/zero_attention_todo.csv` | Generated intermediate | Declared comparison-report output for manual mapping follow-up. |

## Final Analysis Outputs

| Path | Classification | Notes |
| --- | --- | --- |
| `outputs/canonical_mapping/04_evidence_package_reviewed.csv` | Final review input | Human-reviewed canonical decision table. |
| `outputs/canonical_mapping/gbd_efo_master_mapping.tsv` | Final mapping output | Compiler-derived attention-score mapping contract. |
| `outputs/attention_scores/gbd_universe.csv` | Final scoring output | Configured GBD analysis universe. |
| `outputs/attention_scores/all_time_attention_scores.csv` | Final scoring output | All-time detailed attention metrics. |
| `outputs/attention_scores/temporal_attention_scores.csv` | Final scoring output | All-time, yearly, and sliding-window attention output. |

## Report Artifacts

| Path | Classification | Notes |
| --- | --- | --- |
| `scripts/attention-score-pipeline-comparison.html` | Report artifact | Declared Snakemake output of comparison report. |
| `scripts/regional_concentration_index/regional_concentration_index.html` | Report artifact | Declared Snakemake output of regional report. |
| `figures/*` from `regional_analysis.figure_outputs` | Report artifact | Declared figure exports from regional report. |

## Site Artifacts

| Path | Classification | Notes |
| --- | --- | --- |
| `site/public/data/*.json` | Generated site input | Static JSON generated by `site/R/build.R`; validated by contract tests. |
| `site/public/data/country/*.json` | Generated site input | Per-country data artifacts. |
| `docs/**` | Committed release artifact | Vite production build used for GitHub Pages publishing. |

## Local-Only Artifacts

| Path | Classification | Notes |
| --- | --- | --- |
| `.snakemake/`, `.quarto/`, `.Rproj.user/` | Local-only artifact | Ignored runtime state. |
| `site/node_modules/`, `site/test-results/`, `site/playwright-report/` | Local-only artifact | Ignored frontend dependencies/test output. |
| `*_cache/`, `*_files/` | Local-only/report render artifact | Ignored Quarto cache/support files. |
| `Rplots.pdf`, `.DS_Store`, `.Rhistory` | Local-only artifact | Should not be committed. |

## Archived Deprecated Files

These files have been moved out of the active code path after inventory review.

| Archived path | Original path | Reason |
| --- | --- | --- |
| `archive/deprecated/regional_concentration_index/data_loaders6.R` | `scripts/regional_concentration_index/R/data_loaders6.R` | Duplicate hardcoded regional loader superseded by config-driven `data_loaders.R`. |
| `archive/deprecated/regional_concentration_index/regional_concentration_index6.qmd` | `scripts/regional_concentration_index/regional_concentration_index6.qmd` | Duplicate regional report variant superseded by config-driven active report. |
| `archive/deprecated/regional_concentration_index/rendered/regional_concentration_index6.html` | `scripts/regional_concentration_index/regional_concentration_index6.html` | Rendered artifact for deprecated duplicate report. |
| `archive/deprecated/attention_pipeline/build_gbd_efo_master_mapping.R` | `scripts/build_gbd_efo_master_mapping.R` | Legacy master-mapping entrypoint superseded by canonical mapping workflow and compiler export. |
| `archive/deprecated/attention_pipeline/legacy_scripts/` | `scripts/build_ols_candidate_mappings.R`, `scripts/generate_attention_scores.r`, `scripts/map_gwas_attention_score_gbd_terms_step1.r`, `scripts/map_gwas_attention_score_gbd_terms_step2.r`, `scripts/harmonise_data.r`, `scripts/check-dec-data.r`, `scripts/checking_attention_by_year.r`, `scripts/bangladesh.ipynb`, `scripts/gbd-gwas-simulation.ipynb`, `scripts/gwas_zero_attention_traits.txt` | Pre-Snakemake scripts/notebooks tied to archived `Data/` artifacts and superseded by the config-driven pipeline. |
| `archive/deprecated/reports/gwas-attention-comparison.rmd`, `archive/deprecated/reports/gwas-attention-comparison.html` | `scripts/gwas-attention-comparison.rmd`, `scripts/gwas-attention-comparison.html` | Older comparison report superseded by configured Quarto report. |
| `archive/deprecated/regional_concentration_index/regional-concentration-index.r`, `archive/deprecated/regional_concentration_index/regional-concentration-index.ipynb` | `scripts/regional-concentration-index.r`, `scripts/regional-concentration-index.ipynb` | Pre-migration regional analysis variants superseded by modular Quarto report. |
| `archive/deprecated/reports/concentration_curves.Rmd`, `archive/deprecated/reports/concentration_curves.nb.html` | `concentration_curves.Rmd`, `concentration_curves.nb.html` | Historical notebook output outside active workflow. |
| `archive/deprecated/rayan/script- Rayan/` | `script- Rayan/` | Historical/manual scripts outside active Snakemake path. |
| `archive/deprecated/figures/` | `map1990.pdf`, `map2023.pdf` | Destination reserved for duplicate root figure artifacts; files were already absent during this archive pass. |

## Archived Deprecated Data Artifacts

These files and directories have been moved out of `Data/` so the data directory
contains only configured raw/reference inputs.

| Archived path | Original path | Reason |
| --- | --- | --- |
| `archive/deprecated/data/legacy_inputs/IHME-GBD_2021_DATA-113ae32d-1990.csv` | `Data/IHME-GBD_2021_DATA-113ae32d-1990.csv` | Superseded GBD 2021 extract outside the active configured release. |
| `archive/deprecated/data/legacy_inputs/IHME-GBD_2021_DATA-120ebfcd-1.csv` | `Data/IHME-GBD_2021_DATA-120ebfcd-1.csv` | Superseded GBD 2021 extract outside the active configured release. |
| `archive/deprecated/data/legacy_inputs/IHME_GBD_2021_A1_HIERARCHIES_Y2024M05D15.XLSX` | `Data/IHME_GBD_2021_A1_HIERARCHIES_Y2024M05D15.XLSX` | Superseded hierarchy workbook; active config uses the GBD 2023 hierarchy. |
| `archive/deprecated/data/legacy_inputs/gwas_catalog_v1.0.2.1-studies_r2024-06-07.xlsx` | `Data/gwas_catalog_v1.0.2.1-studies_r2024-06-07.xlsx` | Superseded GWAS Catalog release; active config uses the 2026-06-01 TSV. |
| `archive/deprecated/data/legacy_inputs/gbd-gwascat-20260402.xlsx` | `Data/gbd-gwascat-20260402.xlsx` | Legacy combined workbook outside the active Snakemake path. |
| `archive/deprecated/data/legacy_inputs/Manually curated GBD conditions with zero attention.xlsx` | `Data/Manually curated GBD conditions with zero attention.xlsx` | Legacy manual workbook superseded by configured CSV manual mapping input. |
| `archive/deprecated/data/legacy_inputs/april2025/` | `Data/april2025/` | Superseded burden release; active config uses `data/december2025/`. |
| `archive/deprecated/data/legacy_inputs/october2025/` | `Data/october2025/` | Superseded burden release; active config uses `data/december2025/`. |
| `archive/deprecated/data/attention_outputs/` | `Data/GBD_combined_dataset_*`, `Data/Ncase_*`, `Data/gbd_universe_275.csv`, `Data/merged_dataset_*`, `Data/traits_zero_attention_score.csv`, `Data/zero_attention_todo.csv` | Legacy generated attention-score outputs now owned by `outputs/attention_scores/` and `outputs/pipeline_checks/`. |
| `archive/deprecated/data/canonical_mapping_outputs/` | `Data/EFO_merged_dataset_exclude_Injuries.csv`, `Data/gbd_efo_master_mapping*`, `Data/gbd_gwascat_20260402_*`, `Data/mapping_to_be_pruned*`, `Data/remaining_zero_mismatch_diagnostics*`, OLS cache/results | Legacy canonical mapping outputs and caches now owned by `outputs/canonical_mapping/`. |
| `archive/deprecated/data/local_artifacts/` | `Data/.DS_Store`, `Data/~$IHME_GBD_2023_HIERARCHIES_Y2025M10D23.XLSX` | Local Finder/spreadsheet artifacts removed from the active data surface. |

## Human Review Checklist

- Confirm new archive candidates are not referenced by `workflow/Snakefile`, `config/config.yaml`, or active documentation.
- Confirm each new candidate has a replacement path or is historical-only.
- Confirm any rendered artifacts being archived can be regenerated or are intentionally preserved for provenance.
- After approval, perform future moves in a dedicated cleanup commit and run a Snakemake dry-run plus relevant tests.
