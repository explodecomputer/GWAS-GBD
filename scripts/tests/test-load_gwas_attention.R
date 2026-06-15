library(testthat)
library(dplyr)
library(tidyr)

# Synthetic fixture: 5-row GWAS-catalog-like data frame
make_fixture <- function() {
  data.frame(
    PUBMEDID            = c(111, 111, 222, 333, 333),
    DATE                = c("2010-01-01", "2010-01-01", "2015-06-15",
                             "2020-03-01", "2020-03-01"),
    `MAPPED_TRAIT_URI`  = c("EFO_0000001", "EFO_0000002", "EFO_0000001",
                              "EFO_0000003", "EFO_0000001"),
    `ASSOCIATION COUNT` = c(10L, 20L, 5L, 30L, 15L),
    `INITIAL SAMPLE SIZE` = c(
      "1,000 European ancestry cases, 2,000 European ancestry controls",
      "500 cases, 1,000 controls",
      "300 individuals",
      NA,
      "100 cases"
    ),
    `REPLICATION SAMPLE SIZE` = c(
      "200 cases, 400 controls",
      NA,
      "50 individuals",
      NA,
      "25 cases"
    ),
    `Impact factor`     = c("3.5", "3.5", "1·2", NA, "5.0"),
    `DISEASE/TRAIT`     = c("Type 1 diabetes", "Obesity", "Type 1 diabetes",
                             "Asthma", "Type 1 diabetes"),
    stringsAsFactors    = FALSE,
    check.names         = FALSE
  )
}

# Internal helper that replicates load_gwas_attention without file I/O
simulate_load <- function(df) {
  df$NCASE <- .parse_sample_cases(df$`INITIAL SAMPLE SIZE`) +
    .parse_sample_cases(df$`REPLICATION SAMPLE SIZE`)

  df <- df %>%
    tidyr::separate_rows(MAPPED_TRAIT_URI, sep = ", ")

  pubmed_count <- df %>%
    group_by(PUBMEDID) %>%
    summarise(n_efo = length(unique(MAPPED_TRAIT_URI)), .groups = "drop")
  df <- left_join(df, pubmed_count, by = "PUBMEDID")

  df$`Impact factor` <- trimws(df$`Impact factor`)
  df$`Impact factor` <- gsub("·", ".", df$`Impact factor`)
  df$`Impact factor` <- as.numeric(df$`Impact factor`)
  df$`Impact factor`[is.na(df$`Impact factor`)] <- 0

  df$pub_year <- as.integer(format(as.Date(df$DATE), "%Y"))

  df %>%
    rename(
      ASSOCIATION_COUNT = `ASSOCIATION COUNT`,
      Impact_factor     = `Impact factor`,
      DISEASE_TRAIT     = `DISEASE/TRAIT`
    ) %>%
    select(MAPPED_TRAIT_URI, PUBMEDID, pub_year, n_efo,
           ASSOCIATION_COUNT, Impact_factor, DISEASE_TRAIT, NCASE) %>%
    distinct()
}

test_that("EFO terms are separated and one row per EFO×PUBMEDID", {
  df <- make_fixture()
  # Introduce a comma-separated multi-EFO entry
  df$MAPPED_TRAIT_URI[1] <- "EFO_0000001, EFO_0000099"
  result <- simulate_load(df)
  expect_true("EFO_0000099" %in% result$MAPPED_TRAIT_URI)
  expect_equal(nrow(result), nrow(result %>% distinct(MAPPED_TRAIT_URI, PUBMEDID)))
})

test_that("n_efo counts unique EFOs per PUBMEDID", {
  df <- make_fixture()
  result <- simulate_load(df)
  # PUBMEDID 111 has 2 unique EFO terms (EFO_0000001 and EFO_0000002)
  n_efo_111 <- result %>% filter(PUBMEDID == 111) %>% pull(n_efo) %>% unique()
  expect_equal(n_efo_111, 2L)
  # PUBMEDID 222 has only 1 unique EFO term
  n_efo_222 <- result %>% filter(PUBMEDID == 222) %>% pull(n_efo) %>% unique()
  expect_equal(n_efo_222, 1L)
})

test_that("middle-dot in Impact factor is coerced to numeric", {
  df <- make_fixture()
  result <- simulate_load(df)
  efo2_222 <- result %>% filter(PUBMEDID == 222, MAPPED_TRAIT_URI == "EFO_0000001")
  expect_true(is.numeric(result$Impact_factor))
  expect_equal(efo2_222$Impact_factor, 1.2)
})

test_that("NA Impact factor becomes 0", {
  df <- make_fixture()
  result <- simulate_load(df)
  efo3_333 <- result %>% filter(PUBMEDID == 333, MAPPED_TRAIT_URI == "EFO_0000003")
  expect_equal(efo3_333$Impact_factor, 0)
})

test_that("pub_year is extracted from DATE column", {
  df <- make_fixture()
  result <- simulate_load(df)
  expect_equal(result %>% filter(PUBMEDID == 111) %>% pull(pub_year) %>% unique(), 2010L)
  expect_equal(result %>% filter(PUBMEDID == 333) %>% pull(pub_year) %>% unique(), 2020L)
})

test_that("sample case counts are parsed from initial and replication samples", {
  result <- load_gwas_attention({
    df <- make_fixture()
    tsv_path <- tempfile(fileext = ".tsv")
    data.table::fwrite(df, tsv_path, sep = "\t")
    tsv_path
  })

  efo1_111 <- result %>% filter(PUBMEDID == 111, MAPPED_TRAIT_URI == "EFO_0000001")
  efo1_222 <- result %>% filter(PUBMEDID == 222, MAPPED_TRAIT_URI == "EFO_0000001")
  efo3_333 <- result %>% filter(PUBMEDID == 333, MAPPED_TRAIT_URI == "EFO_0000003")

  expect_equal(efo1_111$NCASE, 1200)
  expect_equal(efo1_222$NCASE, 350)
  expect_equal(efo3_333$NCASE, 0)
})

test_that("TSV GWAS catalog without Impact factor loads with zero impact factor", {
  df <- make_fixture()
  df$`Impact factor` <- NULL
  tsv_path <- tempfile(fileext = ".tsv")
  data.table::fwrite(df, tsv_path, sep = "\t")

  result <- load_gwas_attention(tsv_path)

  expect_true(is.numeric(result$Impact_factor))
  expect_true(all(result$Impact_factor == 0))
  expect_equal(result %>% filter(PUBMEDID == 111) %>% pull(pub_year) %>% unique(), 2010L)
})
