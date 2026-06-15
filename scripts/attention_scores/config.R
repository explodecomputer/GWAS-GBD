suppressPackageStartupMessages({
  library(here)
  library(yaml)
})

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

merge_config <- function(base, override) {
  if (is.null(override)) {
    return(base)
  }
  for (name in names(override)) {
    if (is.list(base[[name]]) && is.list(override[[name]]) &&
        !is.data.frame(base[[name]]) && !is.data.frame(override[[name]])) {
      base[[name]] <- merge_config(base[[name]], override[[name]])
    } else {
      base[[name]] <- override[[name]]
    }
  }
  base
}

load_project_config <- function(
    config_path = Sys.getenv("GWAS_GBD_CONFIG", unset = here("config", "config.yaml")),
    local_path = Sys.getenv("GWAS_GBD_LOCAL_CONFIG", unset = here("config", "config.local.yaml"))) {
  if (!file.exists(config_path)) {
    stop("Project config not found: ", config_path, call. = FALSE)
  }

  config <- yaml::read_yaml(config_path)
  if (file.exists(local_path)) {
    config <- merge_config(config, yaml::read_yaml(local_path))
  }

  attr(config, "root") <- normalizePath(here(), mustWork = TRUE)
  config
}

cfg_get <- function(config, key, default = NULL, required = TRUE) {
  value <- config
  for (part in strsplit(key, ".", fixed = TRUE)[[1]]) {
    value <- value[[part]]
    if (is.null(value)) {
      if (required) {
        stop("Missing required config key: ", key, call. = FALSE)
      }
      return(default)
    }
  }
  value
}

cfg_path <- function(path, config = NULL) {
  if (is.null(path) || is.na(path) || !nzchar(path)) {
    return(path)
  }
  path <- as.character(path)
  if (grepl("^(/|[A-Za-z]:[/\\\\])", path)) {
    return(path)
  }
  root <- attr(config, "root") %||% normalizePath(here(), mustWork = TRUE)
  file.path(root, path)
}
