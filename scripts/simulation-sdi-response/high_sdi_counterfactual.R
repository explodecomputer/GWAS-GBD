is_covid_condition <- function(cause_name) {
  grepl("COVID", cause_name, ignore.case = TRUE)
}

ci_from_fractional_rank <- function(outcome, fractional_rank) {
  valid <- !is.na(outcome) & !is.na(fractional_rank)
  outcome <- outcome[valid]
  fractional_rank <- fractional_rank[valid]

  if (length(outcome) < 2 || sum(outcome, na.rm = TRUE) <= 0) {
    return(NA_real_)
  }

  2 * sum(outcome * fractional_rank, na.rm = TRUE) / sum(outcome, na.rm = TRUE) - 1
}

fractional_rank <- function(value) {
  n <- length(value)
  (rank(value, ties.method = "average", na.last = "keep") - 0.5) / n
}

burden_fractional_rank <- function(burden) {
  fractional_rank(burden)
}

reversed_ci_for_simulations <- function(scores, burden) {
  if (is.null(dim(scores))) {
    scores <- matrix(scores, nrow = 1)
  }

  reference_scores <- scores[1, ]
  rank_lookup <- tibble(
    score = reference_scores,
    score_rank = fractional_rank(reference_scores)
  ) %>%
    group_by(score) %>%
    summarise(score_rank = first(score_rank), .groups = "drop")

  score_ranks <- matrix(
    rank_lookup$score_rank[match(as.vector(scores), rank_lookup$score)],
    nrow = nrow(scores),
    ncol = ncol(scores)
  )

  as.numeric(2 * (score_ranks %*% burden) / sum(burden, na.rm = TRUE) - 1)
}

default_counterfactual_proposals <- function() {
  tibble(
    proposal = c(
      "exchangeable_random",
      "exchangeable_rank_jitter_0.05",
      "exchangeable_rank_jitter_0.10",
      "exchangeable_rank_jitter_0.20",
      "exchangeable_rank_jitter_0.50",
      "exchangeable_rank_jitter_1.00",
      "weighted_alpha_1_rank_jitter_0.20",
      "weighted_alpha_1_rank_jitter_0.50",
      "weighted_alpha_2_rank_jitter_0.20",
      "weighted_alpha_2_rank_jitter_0.50"
    ),
    selection_alpha = c(0, 0, 0, 0, 0, 0, 1, 1, 2, 2),
    score_assignment = c(
      "random",
      "rank_aligned",
      "rank_aligned",
      "rank_aligned",
      "rank_aligned",
      "rank_aligned",
      "rank_aligned",
      "rank_aligned",
      "rank_aligned",
      "rank_aligned"
    ),
    rank_jitter_sd = c(NA_real_, 0.05, 0.10, 0.20, 0.50, 1.00, 0.20, 0.50, 0.20, 0.50)
  )
}

generate_candidate_attention <- function(
    n_conditions,
    nonzero_scores,
    high_sdi_rank,
    proposal_spec) {
  n_nonzero <- length(nonzero_scores)
  attention <- numeric(n_conditions)

  if (proposal_spec$selection_alpha > 0) {
    weights <- (high_sdi_rank + 1e-6)^proposal_spec$selection_alpha
    selected_conditions <- sample.int(n_conditions, n_nonzero, prob = weights)
  } else {
    selected_conditions <- sample.int(n_conditions, n_nonzero)
  }

  if (proposal_spec$score_assignment == "rank_aligned") {
    noisy_rank <- high_sdi_rank[selected_conditions] +
      rnorm(n_nonzero, mean = 0, sd = proposal_spec$rank_jitter_sd)

    attention[selected_conditions[order(noisy_rank)]] <- sort(nonzero_scores)
  } else {
    attention[selected_conditions] <- sample(nonzero_scores, n_nonzero)
  }

  attention
}

prepare_counterfactual_windows <- function(exclude_covid = TRUE) {
  gwas_attention <- load_gwas_attention()

  if (exclude_covid) {
    gwas_attention <- gwas_attention %>%
      filter(!is_covid_condition(cause_name))
  }

  all_causes <- gwas_attention %>%
    select(cause_name, cause_id)

  gwas_attention_windows <- load_gwas_attention_windows(all_causes)

  gbd <- load_gbd_sdi_year() %>%
    filter(
      location_name %in% sdi_locations,
      sex_name == "Both"
    )

  if (exclude_covid) {
    gbd <- gbd %>%
      filter(!is_covid_condition(cause_name))
  }

  gbd %>%
    inner_join(
      gwas_attention_windows,
      by = c("cause_name", "cause_id", "year" = "time_strata")
    ) %>%
    add_global_sdi_burden(c(
      "cause_name",
      "cause_id",
      "year",
      "sex_name",
      "analysis_type"
    ))
}

prepare_counterfactual_all_time <- function(
    burden_years = 2023,
    exclude_covid = TRUE) {
  gwas_attention <- load_gwas_attention()

  if (exclude_covid) {
    gwas_attention <- gwas_attention %>%
      filter(!is_covid_condition(cause_name))
  }

  gbd <- load_gbd_sdi_year() %>%
    filter(
      location_name %in% sdi_locations,
      sex_name == "Both",
      year %in% burden_years
    )

  if (exclude_covid) {
    gbd <- gbd %>%
      filter(!is_covid_condition(cause_name))
  }

  gbd %>%
    inner_join(gwas_attention, by = c("cause_name", "cause_id")) %>%
    add_global_sdi_burden(c(
      "cause_name",
      "cause_id",
      "year",
      "sex_name"
    ))
}

compute_observed_counterfactual_alignment <- function(window_data) {
  compute_alignment_by_group(
    window_data,
    c("location_name", "sex_name", "year")
  )
}

simulate_counterfactual_year <- function(
    year_data,
    year_value,
    n_candidates = 50000,
    n_keep = 1000,
    calibration_location = "High SDI",
    calibration_metric = c("active", "reversed"),
    proposal_specs = default_counterfactual_proposals(),
    median_abs_error_warn = 0.02,
    max_abs_error_warn = 0.05) {
  calibration_metric <- match.arg(calibration_metric)
  year_data <- year_data %>%
    filter(year == year_value)

  universe <- year_data %>%
    filter(location_name == calibration_location) %>%
    arrange(cause_id, cause_name) %>%
    select(cause_id, cause_name, total_attention_score, val)

  if (nrow(universe) == 0) {
    stop("No calibration data for year ", year_value, " and location ", calibration_location)
  }

  if (any(is.na(universe$val))) {
    stop("Missing calibration burden values for year ", year_value)
  }

  observed_scores <- universe$total_attention_score
  nonzero_scores <- observed_scores[observed_scores > 0]
  n_conditions <- length(observed_scores)
  n_nonzero <- length(nonzero_scores)
  score_sum <- sum(nonzero_scores)

  if (n_nonzero == 0 || score_sum <= 0) {
    return(list(
      simulations = tibble(),
      calibration = tibble(
        year = year_value,
        calibration_metric = calibration_metric,
        n_candidates = n_candidates,
        n_keep_requested = n_keep,
        n_kept = 0L,
        n_conditions = n_conditions,
        n_nonzero_attention = n_nonzero,
        n_zero_attention = n_conditions - n_nonzero,
        n_unique_nonzero_condition_sets = 0L,
        observed_high_sdi_ci = NA_real_,
        observed_high_sdi_reversed_ci = NA_real_,
        mean_kept_high_sdi_ci = NA_real_,
        mean_kept_high_sdi_reversed_ci = NA_real_,
        observed_calibration_ci = NA_real_,
        mean_kept_calibration_ci = NA_real_,
        median_abs_error = NA_real_,
        max_abs_error = NA_real_,
        calibration_quality = NA_character_
      )
    ))
  }

  n_keep <- min(n_keep, n_candidates)
  high_rank <- burden_fractional_rank(universe$val)
  observed_high_sdi_ci <- ci_from_fractional_rank(observed_scores, high_rank)
  observed_high_sdi_reversed_ci <- ci_from_fractional_rank(
    outcome = universe$val,
    fractional_rank = fractional_rank(observed_scores)
  )

  candidate_scores <- matrix(0, nrow = n_candidates, ncol = n_conditions)
  for (candidate_index in seq_len(n_candidates)) {
    proposal_spec <- proposal_specs[((candidate_index - 1) %% nrow(proposal_specs)) + 1, ]
    candidate_scores[candidate_index, ] <- generate_candidate_attention(
      n_conditions = n_conditions,
      nonzero_scores = nonzero_scores,
      high_sdi_rank = high_rank,
      proposal_spec = proposal_spec
    )
  }

  candidate_high_ci <- as.numeric(2 * (candidate_scores %*% high_rank) / score_sum - 1)
  candidate_high_reversed_ci <- reversed_ci_for_simulations(
    scores = candidate_scores,
    burden = universe$val
  )

  if (calibration_metric == "active") {
    observed_calibration_ci <- observed_high_sdi_ci
    candidate_calibration_ci <- candidate_high_ci
  } else {
    observed_calibration_ci <- observed_high_sdi_reversed_ci
    candidate_calibration_ci <- candidate_high_reversed_ci
  }

  keep_index <- order(abs(candidate_calibration_ci - observed_calibration_ci))[seq_len(n_keep)]
  kept_scores <- candidate_scores[keep_index, , drop = FALSE]
  kept_high_ci <- candidate_high_ci[keep_index]
  kept_high_reversed_ci <- candidate_high_reversed_ci[keep_index]
  kept_calibration_ci <- candidate_calibration_ci[keep_index]

  location_ranks <- year_data %>%
    semi_join(universe, by = c("cause_id", "cause_name")) %>%
    arrange(location_name, cause_id, cause_name) %>%
    group_by(location_name) %>%
    summarise(
      burden_rank = list(burden_fractional_rank(val)),
      burden = list(val),
      .groups = "drop"
    )

  simulations <- lapply(seq_len(nrow(location_ranks)), function(location_index) {
    ranks <- location_ranks$burden_rank[[location_index]]
    burden <- location_ranks$burden[[location_index]]
    sim_ci <- as.numeric(2 * (kept_scores %*% ranks) / score_sum - 1)
    sim_reversed_ci <- reversed_ci_for_simulations(
      scores = kept_scores,
      burden = burden
    )

    tibble(
      year = year_value,
      sim_id = seq_along(sim_ci),
      location_name = location_ranks$location_name[[location_index]],
      concentration_index = sim_ci,
      concentration_index_rev = sim_reversed_ci,
      high_sdi_calibration_ci = kept_high_ci,
      high_sdi_calibration_reversed_ci = kept_high_reversed_ci,
      selected_calibration_ci = kept_calibration_ci,
      selected_calibration_abs_error = abs(kept_calibration_ci - observed_calibration_ci)
    )
  }) %>%
    bind_rows()

  nonzero_sets <- apply(kept_scores > 0, 1, function(x) {
    paste(which(x), collapse = ",")
  })

  median_abs_error <- median(abs(kept_calibration_ci - observed_calibration_ci), na.rm = TRUE)
  max_abs_error <- max(abs(kept_calibration_ci - observed_calibration_ci), na.rm = TRUE)

  calibration <- tibble(
    year = year_value,
    calibration_metric = calibration_metric,
    n_candidates = n_candidates,
    n_keep_requested = n_keep,
    n_kept = nrow(kept_scores),
    n_conditions = n_conditions,
    n_nonzero_attention = n_nonzero,
    n_zero_attention = n_conditions - n_nonzero,
    n_unique_nonzero_condition_sets = n_distinct(nonzero_sets),
    observed_high_sdi_ci = observed_high_sdi_ci,
    observed_high_sdi_reversed_ci = observed_high_sdi_reversed_ci,
    mean_kept_high_sdi_ci = mean(kept_high_ci, na.rm = TRUE),
    mean_kept_high_sdi_reversed_ci = mean(kept_high_reversed_ci, na.rm = TRUE),
    observed_calibration_ci = observed_calibration_ci,
    mean_kept_calibration_ci = mean(kept_calibration_ci, na.rm = TRUE),
    median_abs_error = median_abs_error,
    max_abs_error = max_abs_error,
    calibration_quality = case_when(
      median_abs_error > median_abs_error_warn ||
        max_abs_error > max_abs_error_warn ~ "Warning",
      TRUE ~ "Well calibrated"
    )
  )

  list(
    simulations = simulations,
    calibration = calibration
  )
}

run_high_sdi_counterfactual <- function(
    window_data,
    years = NULL,
    n_candidates = 50000,
    n_keep = 1000,
    seed = 20260615,
    calibration_location = "High SDI",
    calibration_metric = c("active", "reversed"),
    proposal_specs = default_counterfactual_proposals(),
    median_abs_error_warn = 0.02,
    max_abs_error_warn = 0.05) {
  calibration_metric <- match.arg(calibration_metric)
  if (is.null(years)) {
    years <- sort(unique(window_data$year))
  }

  set.seed(seed)

  results <- lapply(years, function(year_value) {
    simulate_counterfactual_year(
      year_data = window_data,
      year_value = year_value,
      n_candidates = n_candidates,
      n_keep = n_keep,
      calibration_location = calibration_location,
      calibration_metric = calibration_metric,
      proposal_specs = proposal_specs,
      median_abs_error_warn = median_abs_error_warn,
      max_abs_error_warn = max_abs_error_warn
    )
  })

  list(
    simulations = bind_rows(lapply(results, `[[`, "simulations")),
    calibration = bind_rows(lapply(results, `[[`, "calibration")) %>%
      mutate(seed = seed)
  )
}

summarise_counterfactual <- function(
    simulations,
    observed_alignment,
    calibration = NULL) {
  null_summary <- simulations %>%
    group_by(location_name, year) %>%
    summarise(
      sim_lci = quantile(concentration_index, 0.025, na.rm = TRUE),
      sim_median = median(concentration_index, na.rm = TRUE),
      sim_uci = quantile(concentration_index, 0.975, na.rm = TRUE),
      sim_reversed_lci = quantile(concentration_index_rev, 0.025, na.rm = TRUE),
      sim_reversed_median = median(concentration_index_rev, na.rm = TRUE),
      sim_reversed_uci = quantile(concentration_index_rev, 0.975, na.rm = TRUE),
      n_sim = n(),
      .groups = "drop"
    )

  observed <- observed_alignment %>%
    filter(sex_name == "Both") %>%
    select(
      location_name,
      year,
      observed_ci = concentration_index,
      observed_reversed_ci = concentration_index_rev,
      observed_attention_gini = attention_gini,
      observed_alignment_ratio = alignment_ratio,
      observed_share_alignment = share_alignment,
      observed_n_causes = n_causes
    )

  summary <- null_summary %>%
    left_join(observed, by = c("location_name", "year")) %>%
    left_join(
      simulations %>%
        left_join(
          observed %>%
            select(location_name, year, observed_ci, observed_reversed_ci),
          by = c("location_name", "year")
        ) %>%
        group_by(location_name, year) %>%
        summarise(
          observed_percentile_rank = mean(concentration_index <= observed_ci, na.rm = TRUE),
          observed_reversed_percentile_rank = mean(
            concentration_index_rev <= observed_reversed_ci,
            na.rm = TRUE
          ),
          .groups = "drop"
        ),
      by = c("location_name", "year")
    ) %>%
    mutate(
      observed_minus_sim_median = observed_ci - sim_median,
      observed_reversed_minus_sim_median = observed_reversed_ci - sim_reversed_median,
      null_compatibility = case_when(
        is.na(observed_ci) ~ NA_character_,
        observed_ci < sim_lci ~ "Below null ribbon",
        observed_ci > sim_uci ~ "Above null ribbon",
        TRUE ~ "Compatible with null ribbon"
      ),
      reversed_null_compatibility = case_when(
        is.na(observed_reversed_ci) ~ NA_character_,
        observed_reversed_ci < sim_reversed_lci ~ "Below null ribbon",
        observed_reversed_ci > sim_reversed_uci ~ "Above null ribbon",
        TRUE ~ "Compatible with null ribbon"
      )
    )

  if (!is.null(calibration)) {
    summary <- summary %>%
      left_join(
        calibration %>%
          select(
            year,
            calibration_quality,
            median_abs_error,
            max_abs_error
          ),
        by = "year"
      )
  }

  summary
}

summarise_null_compatibility <- function(
    counterfactual_summary,
    metric = c("active", "reversed"),
    include_global = FALSE) {
  metric <- match.arg(metric)
  summary_data <- counterfactual_summary

  if (!include_global) {
    summary_data <- summary_data %>%
      filter(location_name != "Global")
  }

  if (metric == "active") {
    summary_data <- summary_data %>%
      mutate(
        compatibility = null_compatibility,
        percentile_rank = observed_percentile_rank,
        observed_minus_sim_median_value = observed_minus_sim_median
      )
  } else {
    summary_data <- summary_data %>%
      mutate(
        compatibility = reversed_null_compatibility,
        percentile_rank = observed_reversed_percentile_rank,
        observed_minus_sim_median_value = observed_reversed_minus_sim_median
      )
  }

  summary_data %>%
    group_by(location_name) %>%
    summarise(
      n_years = n(),
      n_below = sum(compatibility == "Below null ribbon", na.rm = TRUE),
      n_compatible = sum(compatibility == "Compatible with null ribbon", na.rm = TRUE),
      n_above = sum(compatibility == "Above null ribbon", na.rm = TRUE),
      share_below = n_below / n_years,
      median_percentile_rank = median(percentile_rank, na.rm = TRUE),
      min_percentile_rank = min(percentile_rank, na.rm = TRUE),
      max_percentile_rank = max(percentile_rank, na.rm = TRUE),
      median_observed_minus_sim = median(observed_minus_sim_median_value, na.rm = TRUE),
      n_calibration_warnings = sum(calibration_quality == "Warning", na.rm = TRUE),
      .groups = "drop"
    )
}

plot_counterfactual_ribbon <- function(
    counterfactual_summary,
    include_global = FALSE,
    metric = c("active", "reversed"),
    x_label = "GWAS attention window",
    title = NULL) {
  metric <- match.arg(metric)
  plot_data <- counterfactual_summary

  if (!include_global) {
    plot_data <- plot_data %>%
      filter(location_name != "Global")
  }

  if (metric == "active") {
    plot_data <- plot_data %>%
      mutate(
        observed_value = observed_ci,
        sim_lci_value = sim_lci,
        sim_median_value = sim_median,
        sim_uci_value = sim_uci
      )
    y_label <- "Attention-burden concentration index"
  } else {
    plot_data <- plot_data %>%
      mutate(
        observed_value = observed_reversed_ci,
        sim_lci_value = sim_reversed_lci,
        sim_median_value = sim_reversed_median,
        sim_uci_value = sim_reversed_uci
      )
    y_label <- "Reversed concentration index"
  }

  plot_data %>%
    ggplot(aes(x = year)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey55") +
    geom_ribbon(
      aes(ymin = sim_lci_value, ymax = sim_uci_value),
      fill = "#9ecae1",
      alpha = 0.45
    ) +
    geom_line(
      aes(y = sim_median_value, colour = "Null median"),
      linewidth = 0.5
    ) +
    geom_line(
      aes(y = observed_value, colour = "Observed"),
      linewidth = 0.6
    ) +
    geom_point(
      aes(y = observed_value, shape = calibration_quality, colour = "Observed"),
      size = 1.6
    ) +
    scale_colour_manual(
      values = c("Observed" = "#de2d26", "Null median" = "#3182bd"),
      breaks = c("Observed", "Null median")
    ) +
    scale_shape_manual(
      values = c("Well calibrated" = 16, "Warning" = 17),
      na.translate = FALSE
    ) +
    facet_grid(. ~ location_name) +
    theme_report() +
    labs(
      x = x_label,
      y = y_label,
      title = title,
      colour = NULL,
      shape = "Calibration"
    )
}
