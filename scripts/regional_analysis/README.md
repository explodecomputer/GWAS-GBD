# Regional concentration index Quarto analysis

This directory is the active config-driven regional concentration index analysis.
It supersedes the older notebook/script variants and the `_6` duplicate report.

## Files

- `regional_concentration_index.qmd`: main HTML report.
- `R/packages.R`: package imports and shared constants.
- `R/data_loaders.R`: config-driven input file readers and join helpers.
- `R/metrics.R`: concentration index, attention Gini, alignment ratio, and related statistics.
- `R/analysis_sets.R`: analysis dataset builders.
- `R/plots.R`: reusable plot functions.
- `outputs/`: optional render destination for exploratory local renders.

Archived deprecated variants:

- `archive/deprecated/regional_concentration_index/data_loaders6.R`
- `archive/deprecated/regional_concentration_index/regional_concentration_index6.qmd`
- `archive/deprecated/regional_concentration_index/rendered/regional_concentration_index6.html`

## Render

From the repository root:

```bash
quarto render scripts/regional_analysis/regional_concentration_index.qmd --output-dir outputs
```

The rendered file will be written under `scripts/regional_analysis/outputs/`.

The authoritative workflow render is:

```bash
snakemake --snakefile workflow/Snakefile --cores 1 regional_report
```

Inputs come from `regional_analysis.burden_files.*` and
`attention_scores.temporal_output` in `config/config.yaml`. Figure exports are
declared in `regional_analysis.figure_outputs`.

## Preview while iterating

From the repository root:

```bash
quarto preview scripts/regional_analysis/regional_concentration_index.qmd --no-browser --host 127.0.0.1 --port 4321
```

The report has Quarto execution caching enabled, so unchanged chunks should not be recomputed on every preview refresh.

## Main metric

The migrated report retains the notebook's concentration-index approach:

```text
concentration_index = CI(GWAS attention ranked by DALY burden)
```

It also reports a normalized alignment ratio:

```text
alignment_ratio = concentration_index / attention_gini
```

This makes the paper's interpretation more direct: the ratio approximates the share of GWAS attention inequality that is aligned with disease-burden ranking.
