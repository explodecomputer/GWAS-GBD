#!/usr/bin/env Rscript

# Install CRAN packages that are not available through conda-forge.
# Run this inside the activated conda environment:
#   conda activate gwas-gbd
#   Rscript scripts/install_r_deps.R

cran_repo <- Sys.getenv("CRAN_REPO", unset = "https://cloud.r-project.org")

cran_only_packages <- c(
  "rineq",
  "LorenzRegression"
)

conda_prefix <- Sys.getenv("CONDA_PREFIX", unset = "")
if (!nzchar(conda_prefix)) {
  # Conda R lives at <env>/lib/R. Derive <env> even if CONDA_PREFIX is unset.
  conda_prefix <- normalizePath(file.path(R.home(), "..", ".."), mustWork = FALSE)
}

conda_r_lib <- normalizePath(file.path(conda_prefix, "lib", "R", "library"),
                             mustWork = FALSE)
r_home <- normalizePath(R.home(), mustWork = FALSE)

if (!dir.exists(conda_r_lib) || !startsWith(r_home, conda_prefix)) {
  stop(
    "This installer must be run with the conda environment's Rscript.\n",
    "Current R.home(): ", R.home(), "\n",
    "Detected conda prefix: ", conda_prefix, "\n",
    "Expected R library: ", conda_r_lib, "\n",
    "Run:\n",
    "  conda activate gwas-gbd\n",
    "  which Rscript\n",
    "  Rscript scripts/install_r_deps.R",
    call. = FALSE
  )
}

dir.create(conda_r_lib, recursive = TRUE, showWarnings = FALSE)
Sys.setenv(
  R_LIBS = "",
  R_LIBS_USER = conda_r_lib,
  R_LIBS_SITE = conda_r_lib
)
.libPaths(conda_r_lib)

installed <- rownames(installed.packages())
missing <- setdiff(cran_only_packages, installed)

if (length(missing) == 0) {
  message("All CRAN-only R dependencies are already installed.")
  quit(save = "no")
}

message("Using R: ", file.path(R.home("bin"), "Rscript"))
message("Installing CRAN-only R dependencies into: ", conda_r_lib)
install.packages(
  missing,
  lib = conda_r_lib,
  repos = cran_repo,
  dependencies = c("Depends", "Imports", "LinkingTo")
)

still_missing <- setdiff(cran_only_packages, rownames(installed.packages()))
if (length(still_missing) > 0) {
  stop(
    "Failed to install required CRAN package(s): ",
    paste(still_missing, collapse = ", "),
    call. = FALSE
  )
}

message("Installed CRAN-only R dependencies: ", paste(missing, collapse = ", "))
