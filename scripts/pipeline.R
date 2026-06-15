#!/usr/bin/env Rscript
# Usage: Rscript scripts/pipeline.R
# Regenerates configured attention score outputs from source inputs.

library(here)
source(here("scripts/config.R"))
source(here("scripts/pipeline_functions.R"))
source(here("scripts/attention_score_pipeline.R"))

cfg <- load_project_config()
run_attention_score_pipeline(
  inputs = attention_pipeline_inputs_from_config(cfg),
  outputs = attention_pipeline_outputs_from_config(cfg)
)

message("\nDone.")
