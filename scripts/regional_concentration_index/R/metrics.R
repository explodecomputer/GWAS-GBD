safe_ci <- function(ineqvar, outcome) {
  valid <- !is.na(ineqvar) & !is.na(outcome)
  ineqvar <- ineqvar[valid]
  outcome <- outcome[valid]

  if (length(ineqvar) < 2 || sum(outcome, na.rm = TRUE) == 0) {
    return(list(
      concentration_index = NA_real_,
      variance = NA_real_,
      fractional_rank = numeric(),
      outcome = numeric()
    ))
  }

  ci(
    ineqvar = ineqvar,
    outcome = outcome,
    method = "direct"
  )
}

get_alignment_metrics <- function(data) {
  burden_ci <- safe_ci(
    ineqvar = data$val,
    outcome = data$total_attention_score
  )

  attention_gini <- safe_ci(
    ineqvar = data$total_attention_score,
    outcome = data$total_attention_score
  )

  ci_value <- burden_ci$concentration_index
  ci_se <- sqrt(burden_ci$variance)
  gini_value <- attention_gini$concentration_index
  share_metrics <- get_share_alignment(data)

  tibble(
    concentration_index = ci_value,
    concentration_index_se = ci_se,
    concentration_index_lci = ci_value - 1.96 * ci_se,
    concentration_index_uci = ci_value + 1.96 * ci_se,
    attention_gini = gini_value,
    alignment_ratio = ci_value / gini_value,
    share_distance = share_metrics$share_distance,
    share_alignment = share_metrics$share_alignment,
    daly_sum = sum(data$val, na.rm = TRUE),
    n_causes = n_distinct(data$cause_id)
  )
}

get_share_alignment <- function(data) {
  data <- data %>%
    filter(!is.na(val), !is.na(total_attention_score))

  daly_total <- sum(data$val, na.rm = TRUE)
  attention_total <- sum(data$total_attention_score, na.rm = TRUE)

  if (daly_total <= 0 || attention_total <= 0 || nrow(data) == 0) {
    return(tibble(
      share_distance = NA_real_,
      share_alignment = NA_real_
    ))
  }

  daly_share <- data$val / daly_total
  attention_share <- data$total_attention_score / attention_total
  share_distance <- 0.5 * sum(abs(attention_share - daly_share), na.rm = TRUE)

  tibble(
    share_distance = share_distance,
    share_alignment = 1 - share_distance
  )
}

compute_alignment_by_group <- function(data, group_vars) {
  data %>%
    group_by(across(all_of(group_vars))) %>%
    do(get_alignment_metrics(.)) %>%
    ungroup()
}

make_lorenz_data <- function(ci_object, group_label) {
  if (length(ci_object$fractional_rank) == 0) {
    return(tibble(x_coord = numeric(), cumdist = numeric(), group = character()))
  }

  my_order <- order(ci_object$fractional_rank)
  y <- ci_object$outcome[my_order]

  tibble(
    x_coord = ci_object$fractional_rank[my_order],
    cumdist = cumsum(y) / sum(y),
    group = group_label
  )
}

get_attention_lorenz <- function(data, label = "GWAS attention (Gini index)") {
  data_single <- data %>%
    filter(!duplicated(cause_id))

  safe_ci(
    ineqvar = data_single$total_attention_score,
    outcome = data_single$total_attention_score
  ) %>%
    make_lorenz_data(label)
}

get_burden_ranked_attention_lorenz <- function(data, label) {
  safe_ci(
    ineqvar = data$val,
    outcome = data$total_attention_score
  ) %>%
    make_lorenz_data(label)
}

get_high_low_sdi_bias <- function(gbd_year_data, year_value = 1990) {
  gbd_year_data %>%
    filter(
      age_group_name == "All Ages",
      sex_name == "Both",
      year == year_value,
      location_name %in% c("High SDI", "Low SDI")
    ) %>%
    select(cause_name, cause_id, location_name, daly = val) %>%
    filter(!is.na(daly)) %>%
    group_by(location_name) %>%
    mutate(daly_rank = rank(-daly, ties.method = "average")) %>%
    ungroup() %>%
    select(cause_name, cause_id, location_name, daly_rank) %>%
    pivot_wider(names_from = location_name, values_from = daly_rank) %>%
    filter(!is.na(`High SDI`), !is.na(`Low SDI`)) %>%
    mutate(
      high_sdi_bias = `Low SDI` - `High SDI`,
      high_sdi_bias_group = case_when(
        `High SDI` <= 150 & `Low SDI` > 150 ~ "High SDI priority",
        `Low SDI` <= 150 & `High SDI` > 150 ~ "Low SDI priority",
        TRUE ~ "No strong priority"
      ),
      high_sdi_bias_quantile = ntile(high_sdi_bias, 4)
    )
}
