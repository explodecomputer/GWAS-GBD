library(testthat)
library(dplyr)
library(writexl)

# ── Fixture ────────────────────────────────────────────────────────────────

make_hierarchy_fixture <- function() {
  data.frame(
    CauseId    = c("1", "2", "3", "4", "5"),
    CauseName  = c("Parent A", "Child A1", "Child A2", "Other A NEC", "Child B1"),
    ParentName = c("All causes", "Parent A", "Parent A", "Parent A", "Parent B"),
    Level      = c("2", "3", "3", "3", "3"),
    CauseOutline = c("B1", "B1.1", "B1.2", "B1.3", "B2.1"),
    stringsAsFactors = FALSE
  )
}

write_hierarchy_xlsx <- function(df = make_hierarchy_fixture()) {
  p <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(list(`Cause Hierarchy` = df), p)
  p
}

# ── Tests ──────────────────────────────────────────────────────────────────

test_that("one row per included condition", {
  path <- write_hierarchy_xlsx()
  ctx <- build_gbd_condition_context(path, include_levels = c(2L, 3L))
  expect_equal(nrow(ctx), n_distinct(ctx$condition_name))
})

test_that("parent_name is correct for child conditions", {
  path <- write_hierarchy_xlsx()
  ctx <- build_gbd_condition_context(path, include_levels = c(2L, 3L))
  child_a1 <- ctx %>% filter(condition_name == "Child A1")
  expect_equal(child_a1$parent_name, "Parent A")
})

test_that("siblings lists sibling conditions excluding self", {
  path <- write_hierarchy_xlsx()
  ctx <- build_gbd_condition_context(path, include_levels = c(2L, 3L))
  a1 <- ctx %>% filter(condition_name == "Child A1")
  sibs <- unlist(strsplit(a1$siblings, " | ", fixed = TRUE))
  expect_true("Child A2" %in% sibs)
  expect_true("Other A NEC" %in% sibs)
  expect_false("Child A1" %in% sibs)
})

test_that("residual conditions flagged by 'Other' prefix", {
  path <- write_hierarchy_xlsx()
  ctx <- build_gbd_condition_context(path, include_levels = c(2L, 3L))
  residuals <- ctx %>% filter(is_residual) %>% pull(condition_name)
  expect_true("Other A NEC" %in% residuals)
  expect_false("Child A1" %in% residuals)
})

test_that("alias, excluded_alias, scope_note columns are present and NA by default", {
  path <- write_hierarchy_xlsx()
  ctx <- build_gbd_condition_context(path, include_levels = c(2L, 3L))
  expect_true(all(is.na(ctx$alias)))
  expect_true(all(is.na(ctx$excluded_alias)))
  expect_true(all(is.na(ctx$scope_note)))
})

test_that("exclude_causes removes specified conditions", {
  path <- write_hierarchy_xlsx()
  ctx <- build_gbd_condition_context(
    path,
    exclude_causes = "Child A1",
    include_levels = c(2L, 3L)
  )
  expect_false("Child A1" %in% ctx$condition_name)
})

test_that("n_siblings is correct", {
  path <- write_hierarchy_xlsx()
  ctx <- build_gbd_condition_context(path, include_levels = c(2L, 3L))
  a1 <- ctx %>% filter(condition_name == "Child A1")
  # Siblings: Child A2, Other A NEC → n_siblings = 2
  expect_equal(a1$n_siblings, 2L)
})
