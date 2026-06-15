library(testthat)
library(here)
library(dplyr)
library(tidyr)

# Source pipeline functions (without readxl/ontologyIndex side-effects)
# Individual test files stub the functions they need if those packages are absent.
tryCatch(
  source(here("scripts/pipeline_functions.R")),
  error = function(e) {
    message("Note: some packages unavailable, sourcing with suppressPackageStartupMessages")
    suppressWarnings(source(here("scripts/pipeline_functions.R")))
  }
)

test_files <- list.files(
  here("scripts/tests"),
  pattern = "^test-.*[.][Rr]$",
  full.names = TRUE
)

for (test_file_path in test_files) {
  test_file(test_file_path)
}
