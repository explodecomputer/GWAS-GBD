# Archive Policy

No active source, data input, or generated output should be deleted during
cleanup. Deprecated files move to `archive/deprecated/` only after the inventory
in `workflow/ARTIFACT_INVENTORY.md` has been reviewed.

## Move Criteria

A file is eligible for archive when all of these are true:

- It is listed as an archive candidate in the inventory.
- It is not referenced by `workflow/Snakefile`, `config/config.yaml`, or active
  documentation.
- The active replacement is named in the inventory or the file is historical
  provenance only.
- Snakemake dry-run and relevant tests pass after the move.

## Destination Layout

| Destination | Use |
| --- | --- |
| `archive/deprecated/regional_concentration_index/` | Old regional reports, loaders, and notebooks. |
| `archive/deprecated/attention_pipeline/` | Superseded scoring or mapping entrypoints. |
| `archive/deprecated/reports/` | Superseded rendered reports and notebooks. |
| `archive/deprecated/figures/` | Duplicate figure artifacts outside configured figure outputs. |
| `archive/deprecated/rayan/` | Historical manually-run script collections. |

Each archive subdirectory should include a short README if files are moved there.
