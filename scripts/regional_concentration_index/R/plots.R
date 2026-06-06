plot_alignment_by_sex <- function(alignment) {
  alignment %>%
    filter(year == 2023, !is.na(location_name)) %>%
    ggplot(aes(x = concentration_index, y = sex_name)) +
    geom_point(
      aes(colour = sex_name),
      position = ggstance::position_dodge2v(height = 0.3)
    ) +
    geom_errorbarh(
      aes(
        xmin = concentration_index_lci,
        xmax = concentration_index_uci,
        colour = sex_name
      ),
      height = 0,
      position = ggstance::position_dodge2v(height = 0.3)
    ) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    facet_grid(location_name ~ .) +
    theme_report() +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
    labs(x = "Concentration index", y = NULL, colour = NULL)
}

plot_alignment_over_time <- function(alignment, y_var = "concentration_index") {
  alignment %>%
    ggplot(aes(y = .data[[y_var]], x = year)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_point() +
    geom_errorbar(
      aes(
        ymin = concentration_index_lci,
        ymax = concentration_index_uci
      ),
      colour = "grey",
      width = 0
    ) +
    geom_smooth(se = FALSE) +
    facet_grid(. ~ location_name) +
    theme_report() +
    labs(x = "Year", y = nice_metric_label(y_var))
}

plot_alignment_sensitivity_over_time <- function(alignment, y_var = "concentration_index") {
  alignment %>%
    ggplot(aes(y = .data[[y_var]], x = year, colour = sensitivity)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey45") +
    geom_point() +
    geom_line() +
    facet_grid(. ~ location_name) +
    theme_report() +
    labs(
      x = "Year",
      y = nice_metric_label(y_var),
      colour = "Analysis"
    )
}

plot_covid_sensitivity_delta <- function(alignment) {
  alignment %>%
    filter(year >= 2018, sex_name == "Both", location_name != "Global") %>%
    select(location_name, year, sensitivity, concentration_index) %>%
    pivot_wider(names_from = sensitivity, values_from = concentration_index) %>%
    mutate(delta = `Excluding COVID` - `Original`) %>%
    ggplot(aes(x = year, y = delta, colour = location_name)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_point() +
    geom_line() +
    theme_report() +
    labs(
      x = "Year",
      y = "Difference in concentration index",
      colour = "Location"
    )
}

plot_sdi_alignment_metric_comparison <- function(alignment, year_value = 2023) {
  alignment %>%
    filter(year == year_value, sex_name == "Both", location_name != "Global") %>%
    select(location_name, alignment_ratio, share_alignment) %>%
    pivot_longer(
      cols = c(alignment_ratio, share_alignment),
      names_to = "metric",
      values_to = "value"
    ) %>%
    mutate(
      metric = factor(
        metric,
        levels = c("alignment_ratio", "share_alignment"),
        labels = c(
          "Rank-aligned inequality (CI / Gini)",
          "Proportional share alignment"
        )
      ),
      location_name = factor(location_name, levels = sdi_locations)
    ) %>%
    ggplot(aes(x = location_name, y = value, colour = metric, group = metric)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_line() +
    geom_point(size = 2) +
    theme_report() +
    theme(axis.text.x = element_text(angle = 35, hjust = 1)) +
    labs(
      x = NULL,
      y = "Alignment score",
      colour = "Metric"
    )
}

plot_sdi_alignment_metric_trends <- function(alignment) {
  alignment %>%
    filter(sex_name == "Both", location_name != "Global") %>%
    select(location_name, year, alignment_ratio, share_alignment) %>%
    pivot_longer(
      cols = c(alignment_ratio, share_alignment),
      names_to = "metric",
      values_to = "value"
    ) %>%
    mutate(
      metric = factor(
        metric,
        levels = c("alignment_ratio", "share_alignment"),
        labels = c(
          "Rank-aligned inequality (CI / Gini)",
          "Proportional share alignment"
        )
      )
    ) %>%
    ggplot(aes(x = year, y = value, colour = metric)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_point() +
    geom_smooth(se = FALSE) +
    facet_grid(. ~ location_name) +
    theme_report() +
    labs(
      x = "Year",
      y = "Alignment score",
      colour = "Metric"
    )
}

plot_country_ranked_alignment <- function(alignment, year_value = 2023) {
  alignment %>%
    filter(year == year_value, sex_name == "Both") %>%
    arrange(desc(concentration_index)) %>%
    mutate(country_rank = row_number()) %>%
    ggplot(aes(x = country_rank, y = concentration_index)) +
    geom_point() +
    geom_errorbar(
      aes(
        ymin = concentration_index_lci,
        ymax = concentration_index_uci
      ),
      width = 0
    ) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme_report() +
    labs(x = "Rank of country", y = "Concentration index")
}

plot_lorenz_curves <- function(lorenz_data) {
  ggplot(lorenz_data, aes(x = x_coord, y = cumdist, group = group)) +
    geom_line(aes(colour = group)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    theme_report() +
    theme(legend.position = c(0.22, 0.78)) +
    labs(
      x = "Fractional rank",
      y = "Cumulative proportion of outcome",
      colour = "Outcome"
    ) +
    scale_colour_manual(values = c(
      "#a6cee3",
      "#1f78b4",
      "#b2df8a",
      "#33a02c"
    ))
}

plot_age_alignment <- function(alignment) {
  alignment %>%
    filter(year == 2023) %>%
    ggplot(aes(y = concentration_index, x = age_group)) +
    geom_errorbar(
      aes(
        ymin = concentration_index_lci,
        ymax = concentration_index_uci
      ),
      colour = "grey",
      width = 0
    ) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_point(aes(size = daly_prop)) +
    facet_grid(. ~ location_name) +
    scale_x_discrete(labels = c(
      "<1", "", "5-9", "", "15-19", "", "25-29", "", "35-39", "",
      "45-49", "", "55-59", "", "65-69", "", "75-79", "", "85-89", ""
    )) +
    theme_report() +
    theme(legend.position = c(0.08, 0.2)) +
    labs(
      x = "Age group",
      y = "Concentration index",
      size = "DALY proportion"
    ) +
    ylim(-0.7, 0.7)
}

plot_gini_over_time <- function(gini_over_time, y_var = "attention_gini") {
  gini_over_time %>%
    ggplot(aes(x = time_strata, y = .data[[y_var]])) +
    geom_point() +
    geom_smooth(se = TRUE, method = "lm") +
    theme_report() +
    labs(x = "Time strata", y = nice_metric_label(y_var))
}

plot_attention_gini_fullscale <- function(gini_over_time) {
  gini_over_time %>%
    ggplot(aes(x = time_strata, y = attention_gini)) +
    geom_point() +
    geom_smooth(se = TRUE, method = "lm") +
    theme_report() +
    labs(x = "Time strata", y = "Gini index of GWAS attention score") +
    ylim(c(0, 1))
}

plot_fixed_gbd_year_alignment <- function(alignment) {
  alignment %>%
    filter(time_strata != 2024) %>%
    ggplot(aes(y = concentration_index, x = time_strata)) +
    geom_errorbar(
      colour = "grey",
      aes(ymin = concentration_index_lci, ymax = concentration_index_uci),
      width = 0
    ) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    facet_grid(paste0("GBD year: ", gbd_year) ~ location_name) +
    geom_smooth(se = FALSE) +
    geom_point() +
    theme_report() +
    labs(
      x = "GWAS attention year",
      y = "Concentration index"
    )
}

plot_top_disease_trends <- function(plot_data) {
  plot_data %>%
    ggplot(aes(x = year, y = value)) +
    geom_line(alpha = 0.3, aes(group = cause_name)) +
    facet_grid(metric ~ what, scales = "free_y") +
    geom_smooth(method = "loess", se = TRUE, linewidth = 1.2) +
    theme_report() +
    theme(legend.position = "none") +
    labs(x = "Year", y = NULL, colour = "Disease")
}

plot_sdi_bias_group_trends <- function(plot_data) {
  plot_data %>%
    filter(year < 2024) %>%
    ggplot(aes(x = year, y = value)) +
    facet_grid(metric ~ ., scales = "free_y") +
    geom_smooth(
      method = "loess",
      se = TRUE,
      linetype = "solid",
      linewidth = 1.2,
      aes(colour = what)
    ) +
    theme_report() +
    theme(legend.position = c(0.22, 0.2)) +
    labs(
      x = "Year",
      y = NULL,
      colour = "Trait breakdown",
      title = "DALY and GWAS attention trends in Low SDI"
    ) +
    scale_colour_brewer(type = "qual")
}

plot_sdi_bias_attention_rank <- function(plot_data, comparison_value) {
  plot_data %>%
    filter(comparison == comparison_value) %>%
    ggplot(aes(x = attention_rank, y = total_attention_score)) +
    geom_point(aes(colour = is_highlighted)) +
    scale_y_log10() +
    geom_label_repel(
      data = plot_data %>%
        filter(comparison == comparison_value, is_highlighted),
      aes(label = cause_name),
      size = 3,
      max.overlaps = 30
    ) +
    theme_report() +
    labs(
      x = "Rank of GWAS attention score",
      y = "GWAS attention score",
      colour = comparison_value
    )
}

plot_sdi_attention_rank_lollipop <- function(plot_data, comparison_value) {
  title_text <- if_else(
    comparison_value == "Most Low-SDI priority traits",
    "Low-SDI priority traits in GWAS attention rank",
    "High-SDI priority traits in GWAS attention rank"
  )

  selected <- plot_data %>%
    filter(comparison == comparison_value, is_highlighted) %>%
    arrange(attention_rank_percentile, total_attention_score, cause_name) %>%
    mutate(
      cause_label = cause_name,
      attention_label = if_else(
        total_attention_score == 0,
        "0",
        format(total_attention_score, big.mark = ",", trim = TRUE)
      )
    )

  background <- plot_data %>%
    filter(comparison == comparison_value) %>%
    mutate(cause_label = "All mapped traits")

  y_limits <- c("All mapped traits", selected$cause_name)

  ggplot() +
    geom_point(
      data = background,
      aes(x = attention_rank_percentile, y = cause_label),
      colour = "grey70",
      alpha = 0.45,
      size = 1.4,
      position = position_jitter(height = 0.08, width = 0)
    ) +
    geom_segment(
      data = selected,
      aes(
        x = 0,
        xend = attention_rank_percentile,
        y = cause_label,
        yend = cause_label
      ),
      colour = "grey80"
    ) +
    geom_point(
      data = selected,
      aes(
        x = attention_rank_percentile,
        y = cause_label,
        colour = attention_status
      ),
      size = 2.4
    ) +
    geom_text(
      data = selected %>% filter(total_attention_score > 0),
      aes(
        x = attention_rank_percentile,
        y = cause_label,
        label = attention_label
      ),
      hjust = -0.2,
      size = 2.8,
      colour = "grey30"
    ) +
    scale_x_continuous(
      labels = scales::percent_format(accuracy = 1),
      limits = c(0, 1.10),
      expand = expansion(mult = c(0.01, 0.01))
    ) +
    scale_colour_manual(values = c(
      "Zero attention" = "#b2182b",
      "Non-zero attention" = "#2166ac"
    )) +
    scale_y_discrete(limits = y_limits) +
    theme_report() +
    theme(
      axis.text.x = element_text(angle = 0),
      panel.grid.major.y = element_blank()
    ) +
    labs(
      x = "GWAS attention rank percentile",
      y = NULL,
      colour = NULL,
      title = title_text
    )
}

plot_low_sdi_attention_rank_lollipop <- function(plot_data) {
  plot_sdi_attention_rank_lollipop(
    plot_data,
    comparison_value = "Most Low-SDI priority traits"
  )
}

plot_high_sdi_attention_rank_lollipop <- function(plot_data) {
  plot_sdi_attention_rank_lollipop(
    plot_data,
    comparison_value = "Most High-SDI priority traits"
  )
}

save_sdi_priority_four_panel <- function(
    rank_plot,
    trend_plot,
    low_attention_plot,
    high_attention_plot,
    path = here("figures/sdi_priority_four_panel.pdf")) {
  combined <- cowplot::plot_grid(
    rank_plot,
    trend_plot,
    low_attention_plot,
    high_attention_plot,
    labels = c("A", "B", "C", "D"),
    ncol = 2,
    align = "hv"
  )

  ggsave(path, combined, width = 16, height = 12)
  combined
}

plot_zero_attention_dalys <- function(plot_data) {
  plot_data %>%
    ggplot(aes(x = year, y = mean_daly)) +
    geom_point(aes(colour = year)) +
    geom_line() +
    facet_grid(zero_attention ~ location_name, scale = "free_y") +
    theme_report() +
    theme(legend.position = "none") +
    labs(x = "Year", y = "Mean DALYs")
}

nice_metric_label <- function(metric) {
  labels <- c(
    concentration_index = "Concentration index",
    alignment_ratio = "Alignment ratio",
    share_alignment = "Proportional share alignment",
    share_distance = "Absolute share distance",
    attention_gini = "Gini index of GWAS attention score",
    n_zero = "Number of zero attention scores"
  )

  if (metric %in% names(labels)) {
    labels[[metric]]
  } else {
    metric
  }
}
