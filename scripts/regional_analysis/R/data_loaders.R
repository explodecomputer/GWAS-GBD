source(here("scripts/attention_scores/config.R"))
regional_config <- load_project_config()

load_gwas_attention <- function(
    path = cfg_path(cfg_get(regional_config, "attention_scores.temporal_output"),
                    regional_config),
    analysis_type_value = "all") {
  fread(path) %>%
    filter(analysis_type == analysis_type_value) %>%
    select(cause_name, cause_id, total_attention_score)
}

load_gwas_attention_combined <- function(
    path = cfg_path(cfg_get(regional_config, "attention_scores.combined_output"),
                    regional_config)) {
  fread(path) %>%
    rename(cause_name = GBD.term) %>%
    select(cause_name, total_attention_score, weighted_nhits,
           weighted_attention_score_impact_factor, weighted_n)
}

load_gwas_attention_windows <- function(
    all_causes,
    path = cfg_path(cfg_get(regional_config, "attention_scores.temporal_output"),
                    regional_config)) {
  windows <- fread(path) %>%
    filter(analysis_type == "sliding_3yr") %>%
    select(cause_name, cause_id, total_attention_score, analysis_type, time_strata)

  windows %>%
    group_by(time_strata) %>%
    do({
      time_strata <- unique(.$time_strata)
      analysis_type <- unique(.$analysis_type)
      out <- left_join(all_causes, ., by = c("cause_name", "cause_id"))
      out$total_attention_score[is.na(out$total_attention_score)] <- 0
      out$time_strata <- time_strata
      out$analysis_type <- analysis_type
      out
    }) %>%
    ungroup()
}

load_gbd_sdi_sex <- function() {
  fread(cfg_path(cfg_get(regional_config, "regional_analysis.burden_files.sex"),
                 regional_config)) %>%
    rename(sex_name = sex, year = year_id)
}

load_gbd_sdi_year <- function() {
  fread(cfg_path(cfg_get(regional_config, "regional_analysis.burden_files.year"),
                 regional_config)) %>%
    rename(sex_name = sex, year = year_id)
}

load_gbd_country <- function() {
  fread(cfg_path(cfg_get(regional_config, "regional_analysis.burden_files.country"),
                 regional_config)) %>%
    rename(sex_name = sex, year = year_id)
}

load_gbd_age <- function() {
  fread(cfg_path(cfg_get(regional_config, "regional_analysis.burden_files.age"),
                 regional_config)) %>%
    rename(
      sex_name = sex,
      year = year_id,
      age_name = age_group_name,
      age_id = age_group_id
    )
}

join_burden_attention <- function(gbd, attention) {
  inner_join(gbd, attention, by = c("cause_name", "cause_id"))
}

add_global_sdi_burden <- function(data, group_vars) {
  global <- data %>%
    filter(location_name %in% sdi_locations) %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(
      nloc = n(),
      location_name = "Global",
      total_attention_score = first(total_attention_score),
      val = sum(val, na.rm = TRUE),
      .groups = "drop"
    )

  bind_rows(
    data %>% filter(location_name %in% sdi_locations),
    global
  ) %>%
    mutate(location_name = factor(location_name, levels = sdi_levels)) %>%
    ungroup()
}

format_age_group <- function(age_name) {
  age_name %>%
    gsub(" years", "", .) %>%
    gsub(" year", "", .) %>%
    gsub(" to ", "-", .) %>%
    factor(levels = c(
      "<1", "2-4", "5-9", "10-14", "15-19", "20-24", "25-29",
      "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
      "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-94"
    ))
}
