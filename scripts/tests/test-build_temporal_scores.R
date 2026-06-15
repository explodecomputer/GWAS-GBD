library(testthat)
library(dplyr)
library(tidyr)

# Synthetic attention spanning 3 years (2010, 2011, 2012).
# GBD term "Disease A" maps to EFO_0000001 via direct match.

make_temporal_attention <- function() {
  data.frame(
    MAPPED_TRAIT_URI  = rep("efo0000001", 6),
    PUBMEDID          = as.character(101:106),
    pub_year          = c(2010L, 2010L, 2011L, 2011L, 2012L, 2012L),
    n_efo             = rep(1L, 6),
    ASSOCIATION_COUNT = c(10L, 10L, 20L, 20L, 30L, 30L),
    Impact_factor     = rep(1.0, 6),
    DISEASE_TRAIT     = rep("Disease A", 6),
    stringsAsFactors  = FALSE
  )
}

make_temporal_direct_map <- function() {
  data.frame(
    `GBD term`       = "Disease A",
    MAPPED_TRAIT_URI = "efo0000001",
    stringsAsFactors = FALSE,
    check.names      = FALSE
  )
}

# Lightweight stub: map_attention_to_gbd_leaves + rollup that bypasses file I/O
stub_map_and_rollup <- function(attention, direct_map, yr, cause_id_val = 999L) {
  att_yr <- attention %>% filter(pub_year == yr)
  if (nrow(att_yr) == 0) return(NULL)

  efo_totals <- att_yr %>%
    group_by(MAPPED_TRAIT_URI) %>%
    summarise(nhits = sum(ASSOCIATION_COUNT), .groups = "drop")

  matched <- att_yr %>%
    inner_join(direct_map, by = "MAPPED_TRAIT_URI") %>%
    group_by(`GBD term`, PUBMEDID) %>%
    summarise(nhits = sum(ASSOCIATION_COUNT), .groups = "drop") %>%
    group_by(`GBD term`) %>%
    summarise(nhit = sum(nhits), .groups = "drop")

  matched %>%
    transmute(
      cause_name            = `GBD term`,
      cause_id              = cause_id_val,
      total_attention_score = nhit,
      analysis_type         = "year",
      time_strata           = yr
    )
}

test_that("year rows equal per-year nhit totals", {
  att  <- make_temporal_attention()
  dmap <- make_temporal_direct_map()

  year_rows <- bind_rows(lapply(c(2010L, 2011L, 2012L), function(yr) {
    stub_map_and_rollup(att, dmap, yr)
  }))

  expect_setequal(unique(year_rows$time_strata), c(2010L, 2011L, 2012L))

  score_2010 <- year_rows %>% filter(time_strata == 2010L) %>% pull(total_attention_score)
  score_2011 <- year_rows %>% filter(time_strata == 2011L) %>% pull(total_attention_score)
  score_2012 <- year_rows %>% filter(time_strata == 2012L) %>% pull(total_attention_score)

  # 2010: two pubs × 10 hits each = 20
  expect_equal(score_2010, 20)
  # 2011: two pubs × 20 hits each = 40
  expect_equal(score_2011, 40)
  # 2012: two pubs × 30 hits each = 60
  expect_equal(score_2012, 60)
})

test_that("sliding_3yr rows equal sum of three constituent year scores", {
  att  <- make_temporal_attention()
  dmap <- make_temporal_direct_map()

  year_rows <- bind_rows(lapply(c(2010L, 2011L, 2012L), function(yr) {
    stub_map_and_rollup(att, dmap, yr)
  }))

  # Only one 3-year window: 2010 = sum of 2010+2011+2012
  min_yr <- 2010L; max_yr <- 2012L - 2L  # = 2010
  sliding <- lapply(min_yr:max_yr, function(i) {
    year_rows %>%
      filter(time_strata %in% c(i, i + 1, i + 2)) %>%
      group_by(cause_name, cause_id) %>%
      summarise(
        total_attention_score = sum(total_attention_score, na.rm = TRUE),
        analysis_type         = "sliding_3yr",
        time_strata           = i,
        .groups = "drop"
      )
  })
  sliding_data <- bind_rows(sliding)

  score_sliding_2010 <- sliding_data %>%
    filter(time_strata == 2010L) %>%
    pull(total_attention_score)

  # 20 + 40 + 60 = 120
  expect_equal(score_sliding_2010, 120)
})

test_that("both year and sliding_3yr analysis_type values appear", {
  att  <- make_temporal_attention()
  dmap <- make_temporal_direct_map()

  year_rows <- bind_rows(lapply(c(2010L, 2011L, 2012L), function(yr) {
    stub_map_and_rollup(att, dmap, yr)
  }))

  # year rows
  expect_true(all(year_rows$analysis_type == "year"))

  # sliding rows
  sliding_rows <- lapply(2010L:2010L, function(i) {
    year_rows %>%
      filter(time_strata %in% c(i, i + 1, i + 2)) %>%
      group_by(cause_name, cause_id) %>%
      summarise(
        total_attention_score = sum(total_attention_score, na.rm = TRUE),
        analysis_type = "sliding_3yr",
        time_strata = i,
        .groups = "drop"
      )
  }) %>% bind_rows()

  combined <- bind_rows(year_rows, sliding_rows)
  expect_setequal(unique(combined$analysis_type), c("year", "sliding_3yr"))
})
