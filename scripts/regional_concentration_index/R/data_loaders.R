load_gwas_attention <- function(
    # path = here("Data/merged_dataset_exclude_Injuries_2023_updated_6.csv"),
    path = here("Data/merged_dataset_exclude_Injuries.csv"),
    analysis_type_value = "all") {
  fread(path) %>%
    filter(analysis_type == analysis_type_value) %>%
    select(cause_name, cause_id, total_attention_score)
}

load_gwas_attention_windows <- function(
    all_causes,
    # path = here("Data/merged_dataset_exclude_Injuries_2023_updated_6.csv")) {
    path = here("Data/merged_dataset_exclude_Injuries.csv")) {
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
  fread(here("Data/december2025/gbd_gwas_paper_data_2.csv")) %>%
    rename(sex_name = sex, year = year_id)
}

load_gbd_sdi_year <- function() {
  fread(here("Data/december2025/gbd_gwas_paper_data_3.csv")) %>%
    rename(sex_name = sex, year = year_id)
}

load_gbd_country <- function() {
  fread(here("Data/december2025/gbd_gwas_paper_data_4.csv")) %>%
    rename(sex_name = sex, year = year_id)
}

load_gbd_age <- function() {
  fread(here("Data/december2025/gbd_gwas_paper_data_1.csv")) %>%
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
