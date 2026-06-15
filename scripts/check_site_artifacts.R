#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(here)
})

source(here("scripts/config.R"))
source(here("site/R/validate.R"))

cfg <- load_project_config()
data_dir <- cfg_path(cfg_get(cfg, "site.public_data_dir"), cfg)
years <- as.integer(unlist(cfg_get(cfg, "site.years")))

validate_site_artifact_files(
  data_dir = data_dir,
  expected_years = years
)

message("Site artifact contract passed: ", data_dir)
