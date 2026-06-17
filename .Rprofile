# ~/.Rprofile
# When a conda environment is active (CONDA_PREFIX is set) and it contains an
# R library, override .libPaths() to use only that library.  This prevents R
# from mixing the conda env's aarch64 packages with the user library's x86_64
# packages, which causes segfaults at dyn.load().
local({
  prefix <- Sys.getenv("CONDA_PREFIX")
  if (!nzchar(prefix)) return()
  conda_lib <- file.path(prefix, "lib", "R", "library")
  if (!dir.exists(conda_lib)) return()
  .libPaths(conda_lib)
})
