# Snakemake workflow

Snakemake is the authoritative workflow runner for the active GWAS-GBD pipeline.
Path and parameter ownership live in committed `config/config.yaml`, with
private overrides in ignored `config/config.local.yaml`.

Create the environment:

```bash
conda env create -f environment.yml
conda activate gwas-gbd
which Rscript
Rscript scripts/install_r_deps.R
```

`rineq` and `LorenzRegression` are installed from CRAN by the R bootstrap
script because they are not available as conda packages. They are still
installed into the active `gwas-gbd` conda environment. If `which Rscript`
does not point inside the conda env, reactivate the environment before running
the bootstrap script.

Run the default report workflow:

```bash
snakemake --snakefile workflow/Snakefile --cores 1
```

Validate the DAG without expensive computation:

```bash
snakemake --snakefile workflow/Snakefile --cores 1 --dry-run
```

CI can validate the DAG shape without full data by using the fixture override:

```bash
GWAS_GBD_LOCAL_CONFIG=workflow/fixtures/config.ci.yaml \
  snakemake --snakefile workflow/Snakefile --cores 1 --dry-run
```

Useful targets:

```bash
snakemake --snakefile workflow/Snakefile --cores 1 canonical_evidence_package
snakemake --snakefile workflow/Snakefile --cores 1 attention_scores
snakemake --snakefile workflow/Snakefile --cores 1 comparison_report
snakemake --snakefile workflow/Snakefile --cores 1 regional_report
snakemake --snakefile workflow/Snakefile --cores 1 site_build
```

Use `config/config.local.yaml` for private local overrides. It is ignored by
git and merged automatically when present.

Check the active R executable and library isolation:

```bash
R_LIBS='' \
R_LIBS_USER="$CONDA_PREFIX/lib/R/library" \
R_LIBS_SITE="$CONDA_PREFIX/lib/R/library" \
Rscript -e 'cat(R.home(), "\n"); print(.libPaths())'
```

Operational references:

- `workflow/ARTIFACT_INVENTORY.md`: active files, generated artifacts, and archive candidates.
- `workflow/ARTIFACT_LAYOUT.md`: approved data/report/site artifact locations.
- `workflow/ARCHIVE_POLICY.md`: rules for moving deprecated files out of the active path.
