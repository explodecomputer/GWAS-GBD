library(testthat)
library(dplyr)

# Synthetic 3-level hierarchy: 1 grandparent → 2 parents → 4 leaves
# Grandparent is level 2 (excluded); parents at level 3; leaves at level 4.
# rollup_hierarchy should produce one row per level-3/4 term in the CMNN/NCD set.

make_hierarchy_df <- function() {
  data.frame(
    `Cause ID`    = c(1L,  2L,  3L,  10L, 11L, 20L, 21L),
    `Cause Name`  = c("Grand", "Parent A", "Parent B",
                       "Leaf A1", "Leaf A2", "Leaf B1", "Leaf B2"),
    `Parent ID`   = c(0L,  1L,  1L,  2L,  2L,  3L,  3L),
    `Parent Name` = c("Root", "Grand", "Grand",
                       "Parent A", "Parent A", "Parent B", "Parent B"),
    `Level`       = c(2L, 3L, 3L, 4L, 4L, 4L, 4L),
    `Cause Outline` = c("B", "B.1", "B.2", "B.1.1", "B.1.2", "B.2.1", "B.2.2"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

make_leaf_scores <- function() {
  data.frame(
    `GBD term`                             = c("Parent A", "Parent B",
                                                "Leaf A1", "Leaf A2",
                                                "Leaf B1", "Leaf B2"),
    total_attention_score                  = c(0, 0, 10, 20, 30, 40),
    nhits                                  = c(0, 0, 10, 20, 30, 40),
    weighted_n                             = c(0, 0,  1,  2,  3,  4),
    weighted_nhits                         = c(0, 0, 10, 20, 30, 40),
    weighted_attention_score_impact_factor = c(0, 0,  1,  2,  3,  4),
    match_type                             = c("none", "none",
                                               "direct", "direct",
                                               "direct", "direct"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

# Stub rollup_hierarchy to use in-memory data instead of reading a file
rollup_hierarchy_stub <- function(leaf_scores, hier_df) {
  h <- hier_df
  names(h) <- trimws(names(h))
  h <- h %>%
    rename(
      cause_id    = `Cause ID`,
      Cause_Name  = `Cause Name`,
      Parent_Name = `Parent Name`,
      Level       = Level,
      Outline     = `Cause Outline`
    )

  h1 <- h %>% filter(grepl("^[AB]", Outline), Level %in% c(3, 4))
  h3 <- h1 %>% filter(!Cause_Name %in% Parent_Name)

  cause_id_lookup <- h1 %>% select(Cause_Name, cause_id) %>% distinct()
  combined <- leaf_scores %>% rename(GBD.term = `GBD term`)

  matched_h3 <- inner_join(
    h3 %>% select(Cause_Name, cause_id),
    combined %>% select(GBD.term, total_attention_score, nhits,
                        weighted_n, weighted_nhits,
                        weighted_attention_score_impact_factor),
    by = c("Cause_Name" = "GBD.term"),
    multiple = "first"
  ) %>%
    mutate(across(c(total_attention_score, nhits, weighted_n,
                    weighted_nhits, weighted_attention_score_impact_factor),
                  ~ coalesce(., 0)))

  parent_terms <- setdiff(combined$GBD.term, h3$Cause_Name)
  parents <- combined %>% filter(GBD.term %in% parent_terms)

  children_map <- h1 %>%
    filter(Parent_Name %in% parents$GBD.term) %>%
    select(Cause_Name, Parent_Name)

  children_scores <- inner_join(
    children_map,
    combined %>% select(GBD.term, total_attention_score, nhits,
                        weighted_n, weighted_nhits,
                        weighted_attention_score_impact_factor),
    by = c("Cause_Name" = "GBD.term")
  ) %>%
    group_by(Parent_Name) %>%
    summarise(
      child_nhits = sum(nhits, na.rm = TRUE),
      child_total = sum(total_attention_score, na.rm = TRUE),
      .groups = "drop"
    )

  parents_rolled <- parents %>%
    left_join(children_scores, by = c("GBD.term" = "Parent_Name")) %>%
    left_join(cause_id_lookup, by = c("GBD.term" = "Cause_Name")) %>%
    mutate(
      nhits                 = coalesce(nhits, 0) + coalesce(child_nhits, 0),
      total_attention_score = coalesce(total_attention_score, 0) + coalesce(child_total, 0)
    ) %>%
    select(GBD.term, cause_id, total_attention_score, nhits,
           weighted_n, weighted_nhits, weighted_attention_score_impact_factor)

  bind_rows(
    matched_h3 %>% rename(GBD.term = Cause_Name, nhit = nhits),
    parents_rolled %>% rename(nhit = nhits)
  ) %>%
    distinct(GBD.term, .keep_all = TRUE)
}

test_that("each parent score equals sum of its children", {
  hier <- make_hierarchy_df()
  ls   <- make_leaf_scores()
  res  <- rollup_hierarchy_stub(ls, hier)

  pa <- res %>% filter(GBD.term == "Parent A") %>% pull(nhit)
  pb <- res %>% filter(GBD.term == "Parent B") %>% pull(nhit)

  # Parent A children: Leaf A1 (10) + Leaf A2 (20) = 30
  expect_equal(pa, 30)
  # Parent B children: Leaf B1 (30) + Leaf B2 (40) = 70
  expect_equal(pb, 70)
})

test_that("leaf node scores are unchanged by rollup", {
  hier <- make_hierarchy_df()
  ls   <- make_leaf_scores()
  res  <- rollup_hierarchy_stub(ls, hier)

  expect_equal(res %>% filter(GBD.term == "Leaf A1") %>% pull(nhit), 10)
  expect_equal(res %>% filter(GBD.term == "Leaf B2") %>% pull(nhit), 40)
})

test_that("cause_id is attached to each output row", {
  hier <- make_hierarchy_df()
  ls   <- make_leaf_scores()
  res  <- rollup_hierarchy_stub(ls, hier)
  expect_true(all(!is.na(res$cause_id)))
})
