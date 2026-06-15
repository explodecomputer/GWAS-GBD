# Project configuration

`config.yaml` is the committed source of truth for project defaults: release
labels, relative paths, output locations, report parameters, and non-secret
service defaults.

Use `config.local.yaml` for private machine-specific overrides. It is ignored
by git and is merged over `config.yaml` by both R scripts and Snakemake.

Example local override:

```yaml
inputs:
  efo_obo: "/absolute/path/to/efo.obo"

services:
  embed_server_url: "http://gpu-host.internal:8000"
```

Keep API keys in environment variables or `secrets.env`; do not put them in
YAML.

The committed config should contain portable defaults and relative paths where
possible. Use local overrides for absolute paths, private service URLs, or
machine-specific settings.

By convention, `data/` contains raw or manually curated inputs only. Generated
mapping, attention-score, report-check, and site artifacts should be written
under `outputs/`, `figures/`, `site/public/data/`, or `docs/` as declared in
`config.yaml` and `workflow/Snakefile`.

To use a different config path without editing files:

```bash
GWAS_GBD_CONFIG=config/config.yaml \
GWAS_GBD_LOCAL_CONFIG=config/config.local.yaml \
snakemake --snakefile workflow/Snakefile --cores 1
```
