#!/usr/bin/env Rscript
# Run all R unit tests for the metrics module.
# Usage: Rscript site/tests/testthat.R

if (!requireNamespace("testthat", quietly = TRUE)) {
  install.packages("testthat", repos = "https://cloud.r-project.org")
}

library(testthat)
library(here)

source(here("site/R/metrics.R"))

test_dir(here("site/tests/testthat"))
