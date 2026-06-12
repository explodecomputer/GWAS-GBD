library(dplyr)
library(stringr)
library(readxl)

# ── Issue 013: GBD Condition Review Context ────────────────────────────────
# Builds the per-condition review context needed for candidate generation and
# curator review: hierarchy relationships, sibling context, residual flags,
# and editable alias/scope fields.

.load_gbd_hierarchy <- function(hierarchy_path) {
  sheet_names <- excel_sheets(hierarchy_path)
  cause_sheet <- grep("cause hierarchy", sheet_names, ignore.case = TRUE, value = TRUE)[1]
  if (is.na(cause_sheet)) {
    stop("Could not find a Cause Hierarchy sheet in: ", hierarchy_path)
  }
  h <- read_xlsx(hierarchy_path, sheet = cause_sheet, col_types = "text")
  names(h) <- trimws(names(h))

  cause_id_col   <- grep("^[Cc]ause.?[Ii][Dd]$",   names(h), value = TRUE)[1]
  cause_name_col <- grep("^[Cc]ause.?[Nn]ame$",     names(h), value = TRUE)[1]
  parent_col     <- grep("^[Pp]arent.?[Nn]ame$",    names(h), value = TRUE)[1]
  level_col      <- grep("^[Ll]evel$",               names(h), value = TRUE)[1]
  outline_col    <- grep("^[Cc]ause.?[Oo]utline$",  names(h), value = TRUE)[1]

  for (col in c(cause_name_col, parent_col, level_col)) {
    if (is.na(col)) stop("GBD hierarchy missing required column near: ", col)
  }

  h %>%
    rename(
      cause_name  = all_of(cause_name_col),
      parent_name = all_of(parent_col),
      gbd_level   = all_of(level_col)
    ) %>%
    mutate(
      cause_id  = if (!is.na(cause_id_col)) .data[[cause_id_col]] else NA_character_,
      outline   = if (!is.na(outline_col))  .data[[outline_col]]  else NA_character_,
      gbd_level = suppressWarnings(as.integer(gbd_level))
    ) %>%
    select(cause_name, cause_id, parent_name, gbd_level, outline) %>%
    filter(!is.na(cause_name), trimws(cause_name) != "")
}

#' Build GBD condition review context.
#'
#' @param hierarchy_path Path to IHME GBD hierarchy Excel file.
#' @param exclude_causes Character vector of cause names to exclude.
#' @param include_levels Integer vector of GBD levels to include.
#'   Default c(3L, 4L) matches the analysis set used elsewhere.
#' @return Data frame with one row per condition:
#'   condition_name, gbd_level, parent_name, siblings, n_siblings,
#'   is_residual, cause_id, alias, excluded_alias, scope_note
build_gbd_condition_context <- function(hierarchy_path,
                                         exclude_causes = character(0),
                                         include_levels = c(3L, 4L)) {
  h <- .load_gbd_hierarchy(hierarchy_path)

  h <- h %>%
    filter(
      (is.null(include_levels) | gbd_level %in% include_levels),
      !cause_name %in% exclude_causes
    )

  # Siblings: all conditions sharing the same parent (excluding self)
  sibling_map <- h %>%
    filter(!is.na(parent_name), parent_name != "") %>%
    group_by(parent_name) %>%
    summarise(
      sibling_list = list(sort(unique(cause_name))),
      .groups = "drop"
    )

  context <- h %>%
    left_join(sibling_map, by = "parent_name") %>%
    mutate(
      siblings = mapply(
        function(s, cn) {
          if (is.null(s)) return(NA_character_)
          paste(setdiff(s, cn), collapse = " | ")
        },
        sibling_list, cause_name,
        SIMPLIFY = TRUE, USE.NAMES = FALSE
      ),
      n_siblings = vapply(sibling_list, function(s) {
        if (is.null(s)) return(0L)
        as.integer(length(s) - 1L)
      }, integer(1)),
      is_residual = grepl("^[Oo]ther\\b", trimws(cause_name)),
      alias          = NA_character_,
      excluded_alias = NA_character_,
      scope_note     = NA_character_
    ) %>%
    select(
      condition_name = cause_name,
      cause_id,
      gbd_level,
      parent_name,
      siblings,
      n_siblings,
      is_residual,
      alias,
      excluded_alias,
      scope_note
    ) %>%
    arrange(gbd_level, condition_name)

  context
}

#' Return sibling condition names for a given condition.
#'
#' @param context Output of build_gbd_condition_context.
#' @param condition_name Condition to look up.
get_siblings <- function(context, condition_name) {
  row <- context %>% filter(condition_name == .env$condition_name)
  if (nrow(row) == 0 || is.na(row$siblings)) return(character(0))
  unlist(strsplit(row$siblings, " | ", fixed = TRUE))
}

#' Return all residual condition names.
residual_conditions <- function(context) {
  context %>% filter(is_residual) %>% pull(condition_name)
}
