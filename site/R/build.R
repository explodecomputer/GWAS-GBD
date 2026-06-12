#!/usr/bin/env Rscript
# build.R — generate static JSON artifacts for the country explorer
# Usage: Rscript site/R/build.R
# Writes artifacts to site/public/data/

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(jsonlite)
  library(here)
})

source(here("site/R/metrics.R"))
source(here("site/R/validate.R"))

OUT_DIR       <- here("site/public/data")
COUNTRY_DIR   <- file.path(OUT_DIR, "country")
GBD_PATH      <- here("Data/december2025/gbd_gwas_paper_data_4.csv")
ATTENTION_SOURCE <- "merged_dataset_exclude_Injuries_2023_updated_6.csv"
ATTENTION_PATH <- here("Data", ATTENTION_SOURCE)
ELIGIBILITY_THRESHOLD <- 0.01

dir.create(COUNTRY_DIR, recursive = TRUE, showWarnings = FALSE)

message("Loading GBD country data...")
gbd <- fread(GBD_PATH) %>%
  filter(
    location_type == "admin0",
    age_group_name == "All Ages",
    sex == "Both",
    year_id %in% c(1990L, 2023L)
  ) %>%
  select(
    location_id, location_name,
    cause_id, cause_name = cause_name,
    year = year_id,
    dalys = val
  )

message("Loading GWAS attention data...")
attention <- fread(ATTENTION_PATH) %>%
  filter(analysis_type == "all") %>%
  select(cause_id, attention_score = total_attention_score) %>%
  distinct(cause_id, .keep_all = TRUE)

message("Joining and computing metrics...")
joined <- inner_join(gbd, attention, by = "cause_id") %>%
  as.data.frame()

validate_joined(joined)

# Compute shares and derived metrics per country + year
joined <- joined %>%
  group_by(location_id, location_name, year) %>%
  mutate(
    burden_share   = to_share(dalys),
    attention_total = sum(attention_score, na.rm = TRUE),
    attention_share = if (sum(attention_score, na.rm = TRUE) > 0)
      attention_score / sum(attention_score, na.rm = TRUE)
    else
      0,
    mismatch_share = mismatch_share(burden_share, attention_share),
    zero_attention = attention_score == 0,
    eligible       = is_eligible(burden_share, attention_share, ELIGIBILITY_THRESHOLD)
  ) %>%
  ungroup()

# ── countries.json ────────────────────────────────────────────────────────────
message("Writing countries.json...")
countries <- joined %>%
  distinct(location_id, location_name) %>%
  arrange(location_name)

write_json(countries, file.path(OUT_DIR, "countries.json"), auto_unbox = TRUE)

# ── conditions.json ───────────────────────────────────────────────────────────
message("Writing conditions.json...")
conditions <- joined %>%
  distinct(cause_id, cause_name) %>%
  left_join(attention, by = "cause_id") %>%
  mutate(zero_attention = attention_score == 0) %>%
  arrange(cause_name)

write_json(conditions, file.path(OUT_DIR, "conditions.json"), auto_unbox = TRUE)

# ── opportunities.json ────────────────────────────────────────────────────────
message("Writing opportunities.json...")
opps <- joined %>%
  filter(eligible) %>%
  select(
    location_id, location_name, cause_id, cause_name, year,
    dalys, burden_share, attention_share, mismatch_share, zero_attention
  ) %>%
  arrange(year, desc(mismatch_share))

validate_opportunities(opps)
write_json(opps, file.path(OUT_DIR, "opportunities.json"), auto_unbox = TRUE)

# ── country_summaries.json ────────────────────────────────────────────────────
message("Writing country_summaries.json...")
summaries <- joined %>%
  group_by(location_id, location_name, year) %>%
  group_modify(~ {
    s <- country_summary(.x)
    as.data.frame(lapply(s, function(v) if (is.null(v)) NA else v))
  }) %>%
  ungroup() %>%
  arrange(location_name, year)

write_json(summaries, file.path(OUT_DIR, "country_summaries.json"), auto_unbox = TRUE)

# ── per-country files ─────────────────────────────────────────────────────────
message("Writing per-country files...")
country_ids <- unique(joined$location_id)

for (lid in country_ids) {
  cdata <- joined %>%
    filter(location_id == lid) %>%
    select(
      location_name,
      cause_id, cause_name, year, dalys,
      burden_share, attention_score, attention_share,
      mismatch_share, zero_attention, eligible
    )

  loc_name <- cdata$location_name[1]

  # Split by year into nested structure
  years_list <- lapply(
    split(cdata %>% select(-year, -location_name), cdata$year),
    function(df) {
      df[order(-df$burden_share), ]
    }
  )

  out <- c(
    list(location_id = lid, location_name = loc_name),
    setNames(years_list, paste0("y", names(years_list)))
  )

  write_json(
    out,
    file.path(COUNTRY_DIR, paste0(lid, ".json")),
    auto_unbox = TRUE
  )
}

# ── metadata.json ─────────────────────────────────────────────────────────────
message("Writing metadata.json...")
meta <- list(
  build_time            = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  burden_source         = "IHME GBD 2023 (gbd_gwas_paper_data_4.csv)",
  attention_source      = sprintf(
    "GWAS Catalog, curated GBD-GWAS alignment (%s)",
    ATTENTION_SOURCE
  ),
  burden_definition     = "DALY count, all ages, both sexes",
  attention_definition  = "All-time mapped attention score from curated GBD-GWAS alignment",
  eligibility_threshold = ELIGIBILITY_THRESHOLD,
  years                 = sort(unique(joined$year)),
  n_countries           = nrow(countries),
  n_conditions          = nrow(conditions),
  n_eligible_opps       = nrow(opps)
)

write_json(meta, file.path(OUT_DIR, "metadata.json"), auto_unbox = TRUE, pretty = TRUE)

message(sprintf(
  "Done. %d countries, %d conditions, %d eligible opportunities.",
  nrow(countries), nrow(conditions), nrow(opps)
))
