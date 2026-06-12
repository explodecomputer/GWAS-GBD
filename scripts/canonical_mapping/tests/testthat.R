library(testthat)
library(here)
library(dplyr)
library(tidyr)

# Source all canonical mapping modules
module_dir <- here("scripts/canonical_mapping/R")
for (f in list.files(module_dir, pattern = "\\.R$", full.names = TRUE)) {
  source(f)
}

# Run all canonical mapping tests
test_files <- list.files(
  here("scripts/canonical_mapping/tests/testthat"),
  pattern = "^test-.*[.][Rr]$",
  full.names = TRUE
)

for (test_file_path in test_files) {
  test_file(test_file_path)
}
