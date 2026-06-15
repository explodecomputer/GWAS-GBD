library(dplyr)
library(data.table)

attention_pipeline_inputs_from_config <- function(cfg) {
  list(
    gwas_catalog = cfg_path(cfg_get(cfg, "inputs.gwas_catalog"), cfg),
    master_mapping = cfg_path(cfg_get(cfg, "canonical_mapping.master_mapping"), cfg),
    gbd_hierarchy = cfg_path(cfg_get(cfg, "inputs.gbd_hierarchy"), cfg),
    manual_zero_mapping = cfg_path(cfg_get(cfg, "inputs.manual_zero_mapping"), cfg)
  )
}

attention_pipeline_outputs_from_config <- function(cfg) {
  list(
    gbd_universe = cfg_path(cfg_get(cfg, "attention_scores.gbd_universe"), cfg),
    combined = cfg_path(cfg_get(cfg, "attention_scores.combined_output"), cfg),
    temporal = cfg_path(cfg_get(cfg, "attention_scores.temporal_output"), cfg)
  )
}

validate_attention_pipeline_inputs <- function(inputs) {
  required <- c("gwas_catalog", "master_mapping", "gbd_hierarchy")
  missing_paths <- required[!file.exists(unlist(inputs[required]))]
  if (length(missing_paths) > 0) {
    stop(
      "Required attention pipeline input(s) not found: ",
      paste(sprintf("%s=%s", missing_paths, unlist(inputs[missing_paths])), collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

load_manual_zero_mapping <- function(path) {
  if (is.null(path) || is.na(path) || !nzchar(path) || !file.exists(path)) {
    return(empty_gbd_efo_master_mapping())
  }

  manual_zero <- data.table::fread(path, na.strings = c("", "NA"), data.table = FALSE) %>%
    dplyr::filter(!is.na(mapped_trait_uri), nzchar(mapped_trait_uri)) %>%
    tidyr::separate_rows(mapped_trait_uri, sep = ",\\s*") %>%
    dplyr::mutate(mapped_trait_uri = gsub("_", ":", trimws(mapped_trait_uri))) %>%
    dplyr::filter(nzchar(mapped_trait_uri))

  if (nrow(manual_zero) == 0) {
    return(empty_gbd_efo_master_mapping())
  }

  manual_zero %>%
    dplyr::transmute(
      gbd_term = cause_name,
      mapped_trait_uri = mapped_trait_uri,
      mapping_strategy = "manual",
      mapping_source = "manual_zero_attention_mapping",
      include_in_pipeline = TRUE,
      hierarchy_distance = 0L,
      hierarchy_weight = 1,
      root_mapped_trait_uri = mapped_trait_uri,
      lookup_trait_uri = NA_character_,
      replacement_type = "original",
      source_file = basename(path),
      source_column = "mapped_trait_uri",
      source_value = mapped_trait_uri,
      search_term = cause_name,
      matched_gwas_trait_examples = NA_character_,
      n_matching_pubmeds = NA_integer_
    ) %>%
    .standardise_master_mapping()
}

append_manual_zero_mappings <- function(master_map, manual_zero_rows) {
  if (is.null(manual_zero_rows) || nrow(manual_zero_rows) == 0) {
    return(master_map)
  }
  dplyr::bind_rows(master_map, manual_zero_rows) %>%
    .standardise_master_mapping()
}

compute_attention_score_outputs <- function(attention,
                                            master_map,
                                            gbd_hierarchy_path,
                                            gbd_universe,
                                            rollup_fn = rollup_hierarchy,
                                            temporal_fn = build_temporal_scores_from_master) {
  leaf_scores <- map_attention_to_gbd_leaves_from_master(
    attention,
    master_map,
    EXCLUDE_CAUSES
  )

  all_scores_raw <- rollup_fn(leaf_scores, gbd_hierarchy_path)
  all_scores <- all_scores_raw %>%
    dplyr::filter(GBD.term %in% gbd_universe$cause_name)

  temporal_scores <- temporal_fn(
    attention,
    master_map,
    EXCLUDE_CAUSES,
    gbd_hierarchy_path
  )

  final <- assemble_output(all_scores, temporal_scores)

  list(
    universe = gbd_universe,
    combined = all_scores %>%
      dplyr::select(
        GBD.term,
        total_attention_score,
        weighted_nhits,
        weighted_attention_score_impact_factor,
        weighted_n,
        nhit
      ),
    temporal = final
  )
}

write_attention_score_outputs <- function(result, outputs) {
  for (path in unlist(outputs)) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  }
  data.table::fwrite(result$universe, outputs$gbd_universe)
  data.table::fwrite(result$combined, outputs$combined)
  data.table::fwrite(result$temporal, outputs$temporal)
  invisible(outputs)
}

run_attention_score_pipeline <- function(inputs,
                                         outputs,
                                         write_outputs = TRUE,
                                         derive_universe_fn = derive_gbd_universe,
                                         rollup_fn = rollup_hierarchy,
                                         temporal_fn = build_temporal_scores_from_master) {
  validate_attention_pipeline_inputs(inputs)

  message("Step 0: Deriving GBD universe (non-injury leaf terms at levels 3/4)...")
  gbd_universe <- derive_universe_fn(inputs$gbd_hierarchy)
  message("  GBD universe: ", nrow(gbd_universe), " terms")

  message("Step 1: Loading GWAS attention data...")
  attention <- load_gwas_attention(inputs$gwas_catalog)
  message("  ", nrow(attention), " EFO x publication rows loaded")

  message("Step 2: Loading GBD-EFO master mapping...")
  master_map <- load_gbd_efo_master_mapping(inputs$master_mapping, active_only = TRUE)
  message("  Master map: ", nrow(master_map), " active rows")
  message("  GBD terms: ", dplyr::n_distinct(master_map$gbd_term))

  manual_zero_rows <- load_manual_zero_mapping(inputs$manual_zero_mapping)
  if (nrow(manual_zero_rows) > 0) {
    master_map <- append_manual_zero_mappings(master_map, manual_zero_rows)
    message(
      "  Manual zero-attention mappings added: ",
      nrow(manual_zero_rows),
      " rows for ",
      dplyr::n_distinct(manual_zero_rows$gbd_term),
      " terms"
    )
  } else {
    message("  No filled manual zero-attention mappings found")
  }

  message("Step 3: Computing all-time and temporal attention outputs...")
  result <- compute_attention_score_outputs(
    attention = attention,
    master_map = master_map,
    gbd_hierarchy_path = inputs$gbd_hierarchy,
    gbd_universe = gbd_universe,
    rollup_fn = rollup_fn,
    temporal_fn = temporal_fn
  )

  if (write_outputs) {
    write_attention_score_outputs(result, outputs)
    message("  Written: ", outputs$gbd_universe)
    message("  Written: ", outputs$combined)
    message("  Written: ", outputs$temporal)
  }

  result
}
