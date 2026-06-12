library(testthat)
library(dplyr)

make_universe <- function(ids, release = "test") {
  list(
    terms = data.frame(
      catalog_release   = release,
      ontology_id       = ids,
      stringsAsFactors  = FALSE
    ),
    metadata = list(catalog_release = release)
  )
}

test_that("retained, new, and dropped terms are correctly classified", {
  old <- make_universe(c("EFO:0000001", "EFO:0000002", "EFO:0000003"), "2023")
  new <- make_universe(c("EFO:0000001", "EFO:0000004"), "2024")  # 0002+0003 dropped, 0004 new

  report <- generate_drift_report(old, new)

  retained <- report$term_drift %>% filter(term_status == "retained") %>% pull(ontology_id)
  new_terms <- report$term_drift %>% filter(term_status == "new")     %>% pull(ontology_id)
  dropped   <- report$term_drift %>% filter(term_status %in% c("dropped", "replaced",
                                                                "obsolete_no_replacement"))
  expect_true("EFO:0000001" %in% retained)
  expect_true("EFO:0000004" %in% new_terms)
  expect_true(all(c("EFO:0000002", "EFO:0000003") %in% dropped$ontology_id))
})

test_that("carry_forward status is correct for retained vs dropped canonical mappings", {
  old <- make_universe(c("EFO:0000001", "EFO:0000002"), "2023")
  new <- make_universe(c("EFO:0000001"), "2024")  # EFO:0000002 dropped

  old_canonical <- data.frame(
    gbd_condition = c("Condition A", "Condition B"),
    ontology_id   = c("EFO:0000001", "EFO:0000002"),
    stringsAsFactors = FALSE
  )

  report <- generate_drift_report(old, new, old_canonical)

  carry <- report$mapping_drift %>%
    filter(gbd_condition == "Condition A") %>%
    pull(carry_forward_status)
  expect_equal(carry, "carry_forward")

  needs_review <- report$mapping_drift %>%
    filter(gbd_condition == "Condition B") %>%
    pull(carry_forward_status)
  expect_true(needs_review %in% c("needs_review", "review_replacement"))
})

test_that("report is a non-empty string", {
  old <- make_universe(c("EFO:0000001"), "2023")
  new <- make_universe(c("EFO:0000001", "EFO:0000002"), "2024")
  report <- generate_drift_report(old, new)
  expect_true(nchar(report$report) > 0)
})

test_that("drift report with no old canonical still returns term drift", {
  old <- make_universe(c("EFO:0000001"), "2023")
  new <- make_universe(c("EFO:0000001", "EFO:0000002"), "2024")
  report <- generate_drift_report(old, new, old_canonical = NULL)
  expect_null(report$mapping_drift)
  expect_true(nrow(report$term_drift) > 0)
})
