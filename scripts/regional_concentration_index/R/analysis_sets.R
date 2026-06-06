prepare_sex_alignment <- function(gwas_attention) {
  load_gbd_sdi_sex() %>%
    join_burden_attention(gwas_attention) %>%
    add_global_sdi_burden(c("cause_name", "cause_id", "year", "sex_name")) %>%
    compute_alignment_by_group(c("location_name", "sex_name", "year"))
}

prepare_year_alignment <- function(gwas_attention, exclude_covid = FALSE) {
  gbd <- load_gbd_sdi_year()

  if (exclude_covid) {
    gbd <- gbd %>%
      filter(!grepl("COVID", cause_name, ignore.case = TRUE))
  }

  gbd %>%
    join_burden_attention(gwas_attention) %>%
    add_global_sdi_burden(c("cause_name", "cause_id", "year", "sex_name")) %>%
    compute_alignment_by_group(c("location_name", "sex_name", "year"))
}

prepare_country_alignment <- function(gwas_attention) {
  load_gbd_country() %>%
    join_burden_attention(gwas_attention) %>%
    compute_alignment_by_group(c("location_name", "sex_name", "year"))
}

prepare_age_alignment <- function(gwas_attention) {
  age_data <- load_gbd_age() %>%
    join_burden_attention(gwas_attention) %>%
    mutate(age_group = format_age_group(age_name)) %>%
    filter(location_name %in% sdi_locations)

  age_data %>%
    add_global_sdi_burden(c(
      "cause_name",
      "cause_id",
      "sex_name",
      "age_name",
      "age_group",
      "age_id",
      "year"
    )) %>%
    compute_alignment_by_group(c(
      "location_name",
      "age_name",
      "age_group",
      "age_id",
      "year"
    )) %>%
    group_by(location_name, year) %>%
    mutate(daly_prop = daly_sum / sum(daly_sum, na.rm = TRUE)) %>%
    ungroup()
}

prepare_attention_gini_over_time <- function(gwas_attention_windows) {
  gwas_attention_windows %>%
    group_by(time_strata) %>%
    summarise(
      n_zero = sum(total_attention_score == 0),
      attention_gini = safe_ci(
        ineqvar = total_attention_score,
        outcome = total_attention_score
      )$concentration_index,
      .groups = "drop"
    )
}

prepare_year_alignment_with_yearly_attention <- function(gwas_attention_windows) {
  load_gbd_sdi_year() %>%
    inner_join(
      gwas_attention_windows,
      by = c("cause_name", "cause_id", "year" = "time_strata")
    ) %>%
    filter(location_name %in% sdi_locations) %>%
    add_global_sdi_burden(c("cause_name", "cause_id", "year", "sex_name")) %>%
    compute_alignment_by_group(c("location_name", "sex_name", "year"))
}

prepare_fixed_gbd_year_alignment <- function(gwas_attention_windows, years = c(1990, 2000, 2010, 2020)) {
  lapply(years, function(year_value) {
    load_gbd_sdi_year() %>%
      filter(year == year_value, !grepl("COVID", cause_name)) %>%
      inner_join(gwas_attention_windows, by = c("cause_name", "cause_id")) %>%
      filter(location_name %in% sdi_locations) %>%
      add_global_sdi_burden(c(
        "cause_name",
        "cause_id",
        "time_strata",
        "sex_name"
      )) %>%
      compute_alignment_by_group(c("location_name", "sex_name", "time_strata")) %>%
      mutate(gbd_year = year_value)
  }) %>%
    bind_rows()
}

prepare_lorenz_data <- function(gwas_attention) {
  year_data <- load_gbd_sdi_year() %>%
    join_burden_attention(gwas_attention) %>%
    add_global_sdi_burden(c("cause_name", "cause_id", "year", "sex_name"))

  bind_rows(
    get_attention_lorenz(year_data, "GWAS attention (Gini index)"),
    get_burden_ranked_attention_lorenz(
      year_data %>% filter(location_name == "Global", sex_name == "Both", year == 2023),
      "GWAS attention ranked by DALYs, Global"
    ),
    get_burden_ranked_attention_lorenz(
      year_data %>% filter(location_name == "Low SDI", sex_name == "Both", year == 2023),
      "GWAS attention ranked by DALYs, Low SDI"
    ),
    get_burden_ranked_attention_lorenz(
      year_data %>% filter(location_name == "High SDI", sex_name == "Both", year == 2023),
      "GWAS attention ranked by DALYs, High SDI"
    )
  )
}

prepare_top_disease_trends <- function(gwas_attention_windows, n_top = 30) {
  gbd_year <- load_gbd_sdi_year()

  top_high_sdi_2019 <- gbd_year %>%
    filter(
      location_name == "High SDI",
      sex_name == "Both",
      measure == "daly",
      age_group_name == "All Ages",
      metric_name == "Number",
      year == 2019
    ) %>%
    arrange(desc(val)) %>%
    slice_head(n = n_top) %>%
    select(cause_name, cause_id, daly_2019 = val)

  top_low_sdi_1990 <- gbd_year %>%
    filter(
      !cause_name %in% top_high_sdi_2019$cause_name,
      location_name == "Low SDI",
      sex_name == "Both",
      measure == "daly",
      age_group_name == "All Ages",
      metric_name == "Number",
      year == 1990
    ) %>%
    arrange(desc(val)) %>%
    slice_head(n = n_top) %>%
    select(cause_name, cause_id, daly_1990 = val)

  bind_rows(
    make_top_disease_trend(
      gbd_year,
      gwas_attention_windows,
      top_high_sdi_2019,
      burden_location = "Low SDI",
      label = "Top diseases in High SDI countries in 2019"
    ),
    make_top_disease_trend(
      gbd_year,
      gwas_attention_windows,
      top_low_sdi_1990,
      burden_location = "Low SDI",
      label = "Top diseases in Low SDI countries in 1990"
    )
  )
}

prepare_sdi_bias_group_trends <- function(gwas_attention_windows, year_value = 1990) {
  gbd_year <- load_gbd_sdi_year()
  sdi_bias <- get_high_low_sdi_bias(gbd_year, year_value = year_value)

  lapply(unique(sdi_bias$high_sdi_bias_group), function(group_value) {
    low_gbd <- gbd_year %>%
      filter(
        cause_name %in% sdi_bias$cause_name[sdi_bias$high_sdi_bias_group == group_value],
        location_name == "Low SDI",
        sex_name == "Both",
        measure == "daly",
        age_group_name == "All Ages",
        metric_name == "Number",
        year == year_value
      ) %>%
      arrange(desc(val)) %>%
      select(cause_name, cause_id, daly = val)

    daly_trend <- gbd_year %>%
      filter(
        location_name == "Low SDI",
        sex_name == "Both",
        measure == "daly",
        age_group_name == "All Ages",
        metric_name == "Number"
      ) %>%
      semi_join(low_gbd, by = c("cause_name", "cause_id")) %>%
      transmute(
        cause_name,
        year,
        value = val,
        metric = "DALYs"
      )

    attention_trend <- gwas_attention_windows %>%
      semi_join(low_gbd, by = c("cause_name", "cause_id")) %>%
      transmute(
        cause_name,
        year = time_strata,
        value = total_attention_score,
        metric = "GWAS attention"
      )

    bind_rows(daly_trend, attention_trend) %>%
      mutate(
        cause_name = factor(cause_name, levels = low_gbd$cause_name),
        what = group_value
      )
  }) %>%
    bind_rows() %>%
    mutate(what = factor(what))
}

prepare_sdi_bias_attention_rank <- function(gwas_attention, year_value = 1990, n_traits = 20) {
  sdi_bias <- get_high_low_sdi_bias(load_gbd_sdi_year(), year_value = year_value) %>%
    semi_join(gwas_attention, by = c("cause_name", "cause_id"))

  low_sdi_traits <- sdi_bias %>%
    arrange(high_sdi_bias) %>%
    slice_head(n = n_traits) %>%
    pull(cause_name)

  high_sdi_traits <- sdi_bias %>%
    arrange(desc(high_sdi_bias)) %>%
    slice_head(n = n_traits) %>%
    pull(cause_name)

  attention_rank <- gwas_attention %>%
    arrange(total_attention_score, cause_name) %>%
    mutate(
      attention_rank = row_number(),
      attention_rank_percentile = percent_rank(total_attention_score),
      attention_status = if_else(
        total_attention_score == 0,
        "Zero attention",
        "Non-zero attention"
      )
    ) %>%
    left_join(
      sdi_bias %>%
        select(cause_name, cause_id, high_sdi_bias, high_sdi_bias_group),
      by = c("cause_name", "cause_id")
    )

  bind_rows(
    attention_rank %>%
      mutate(is_highlighted = cause_name %in% low_sdi_traits) %>%
      mutate(comparison = "Most Low-SDI priority traits"),
    attention_rank %>%
      mutate(is_highlighted = cause_name %in% high_sdi_traits) %>%
      mutate(comparison = "Most High-SDI priority traits")
  )
}

make_top_disease_trend <- function(gbd_year, gwas_attention_windows, top_diseases, burden_location, label) {
  daly_trend <- gbd_year %>%
    filter(
      location_name == burden_location,
      sex_name == "Both",
      measure == "daly",
      age_group_name == "All Ages",
      metric_name == "Number"
    ) %>%
    group_by(cause_name) %>%
    mutate(value = scale(val)[, 1]) %>%
    semi_join(top_diseases, by = c("cause_name", "cause_id")) %>%
    transmute(
      cause_name,
      year,
      value,
      metric = "Standardised DALY"
    )

  attention_trend <- gwas_attention_windows %>%
    group_by(cause_name) %>%
    mutate(value = scale(total_attention_score)[, 1]) %>%
    semi_join(top_diseases, by = c("cause_name", "cause_id")) %>%
    transmute(
      cause_name,
      year = time_strata,
      value,
      metric = "Standardised GWAS attention"
    ) %>%
    ungroup()

  bind_rows(daly_trend, attention_trend) %>%
    mutate(
      cause_name = factor(cause_name, levels = top_diseases$cause_name),
      what = label
    )
}

prepare_zero_attention_dalys <- function(gwas_attention) {
  zero_attention_traits <- gwas_attention %>%
    filter(total_attention_score == 0) %>%
    pull(cause_name) %>%
    unique()

  load_gbd_sdi_year() %>%
    mutate(
      zero_attention = if_else(
        cause_name %in% zero_attention_traits,
        "Zero attention traits",
        "Other traits"
      )
    ) %>%
    filter(
      age_group_name == "All Ages",
      sex_name == "Both",
      location_name %in% sdi_locations
    ) %>%
    mutate(location_name = factor(location_name, levels = sdi_locations)) %>%
    group_by(zero_attention, location_name, year) %>%
    summarise(
      median_daly = median(val, na.rm = TRUE),
      mean_daly = mean(val, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    )
}
