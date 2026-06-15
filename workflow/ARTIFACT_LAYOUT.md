# Data and Artifact Layout Contract

This contract defines where new project files belong. The workflow should add
new outputs only in these categories unless the contract is updated.

## Categories

| Category | Location | Rule |
| --- | --- | --- |
| Raw inputs | `data/` and configured release subdirectories | External/manual source files. Never overwritten by scripts. |
| Generated intermediates | `outputs/` and declared temporary review artifacts | Rebuildable workflow state. May be deleted and regenerated. |
| Final analysis outputs | `outputs/canonical_mapping/` and `outputs/attention_scores/` | Stable outputs consumed by reports, analysis, and site generation. |
| Report artifacts | Configured report HTML paths and `regional_analysis.figure_outputs` | Rebuildable rendered reports and paper figures. |
| Site generated inputs | `site/public/data/` | JSON files generated from final analysis outputs. Validated before publish. |
| Site release build | `docs/` | Committed production build for GitHub Pages. Do not use for human docs. |
| Local-only artifacts | `.snakemake/`, `.quarto/`, caches, `node_modules`, test reports | Ignored. Must not be required by the workflow. |
| Archive | `archive/deprecated/` | Historical files removed from the active workflow after inventory approval. |

## Active Workflow Outputs

| Snakemake rule | Output category |
| --- | --- |
| `canonical_evidence_package` | Generated intermediate |
| `export_master_mapping` | Final analysis output |
| `attention_scores` | Final analysis output |
| `comparison_report` | Report artifact plus generated manual-review TODO |
| `regional_report` | Report artifact plus declared figures |
| `site_artifacts` | Site generated inputs |
| `site_build` | Site release build |

## Reference Datasets

Reference or comparison datasets should be named and configured under
`inputs.reference_*` or documented in `workflow/ARTIFACT_INVENTORY.md`. They are
comparison baselines, not canonical scoring sources.

## Review Checklist for New Outputs

- Is the output declared in `workflow/Snakefile`?
- Is the output path set or derivable from `config/config.yaml`?
- Does the output fit one category above?
- If it is a report side effect, should it be promoted to a declared output?
- If it is local-only, is it covered by `.gitignore`?
- If it is a site JSON artifact, does `scripts/check_site_artifacts.R` validate it?
