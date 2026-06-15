library(testthat)
library(dplyr)

test_that("all-time output uses total_attention_score, not nhit", {
  all_scores <- tibble(
    GBD.term = "Condition A",
    cause_id = 101L,
    total_attention_score = 7,
    nhit = 0
  )
  temporal_scores <- tibble(
    cause_name = character(),
    cause_id = integer(),
    total_attention_score = numeric(),
    analysis_type = character(),
    time_strata = integer()
  )

  result <- assemble_output(all_scores, temporal_scores)

  all_row <- result %>% filter(analysis_type == "all")
  expect_equal(all_row$total_attention_score, 7)
})
