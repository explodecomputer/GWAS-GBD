# Regional concentration index Quarto analysis

This directory is a plain-text migration of `scripts/regional-concentration-index.ipynb`.

## Files

- `regional_concentration_index.qmd`: main HTML report.
- `R/packages.R`: package imports and shared constants.
- `R/data_loaders.R`: input file readers and join helpers.
- `R/metrics.R`: concentration index, attention Gini, alignment ratio, and related statistics.
- `R/analysis_sets.R`: analysis dataset builders.
- `R/plots.R`: reusable plot functions.
- `outputs/`: suggested render destination.

## Render

From the repository root:

```bash
quarto render scripts/regional_concentration_index/regional_concentration_index.qmd --output-dir outputs
```

The rendered file will be written under `scripts/regional_concentration_index/outputs/`.

## Preview while iterating

From the repository root:

```bash
quarto preview scripts/regional_concentration_index/regional_concentration_index.qmd --no-browser --host 127.0.0.1 --port 4321
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
