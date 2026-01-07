####################
###Harmonise data###
####################

# Load necessary libraries

library(dplyr)
library(data.table)
library(tidyr)
library(stringr)
library(DescTools)
library(ggplot2)
library(stats)
library(readr)
library(ontologyIndex)
library(readxl)
library(openxlsx)

# Read in relevant datasets 

## Create a new column for disease names to facilitate partial matching in attention
# attention is created in generate_attention_scores.R
attention <- attention %>%
  mutate(MAPPED_TRAIT_NAME = tolower(trimws(gsub("[^a-zA-Z0-9 ]", "", DISEASE_TRAIT))))

## Create a new column for disease names to facilitate partial matching in combined_unmatched_terms
combined_unmatched_terms <- combined_unmatched_terms%>%
  mutate(GBD_TERM_NAME = tolower(trimws(gsub("[^a-zA-Z0-9 ]", "", combined_unmatched_terms$`GBD term`))))


## Perform partial matching between MAPPED_TRAIT_NAME in attention and GBD_TERM_NAME in combined_unmatched_terms with the rest of the information

partial_matches <- combined_unmatched_terms %>%
  cross_join(attention) %>%
  filter(str_detect(MAPPED_TRAIT_NAME, GBD_TERM_NAME)) %>%
  select(
    GBD_TERM_NAME,
    MAPPED_TRAIT_NAME,
    GBD_TRAIT_URI = MAPPED_TRAIT_URI.x,
    MAPPED_TRAIT_URI = MAPPED_TRAIT_URI.y,
    n,
    weighted_n,
    nhits,
    weighted_nhits,
    weighted_attention_score_impact_factor,
    PUBMEDID,
    `GBD term`
  ) %>%
  distinct()



## Group by GBD term and PUBMEDID and summarize within these groups
 partial_matches_filtered <- partial_matches %>%
    group_by(`GBD term`, PUBMEDID) %>%
    summarise(
      total_attention_score_per_pubmed = first(n, na.rm = TRUE),
      weighted_n = first(weighted_n),
      nhits = first(nhits),
      weighted_nhits = first(weighted_nhits),
      weighted_attention_score_impact_factor = first(weighted_attention_score_impact_factor),
      unique_pubmed_ids = n_distinct(PUBMEDID),
      MAPPED_TRAIT_URI = first(MAPPED_TRAIT_URI),
      GBD_TRAIT_URI = first(GBD_TRAIT_URI),
      MAPPED_TRAIT_NAME = first(MAPPED_TRAIT_NAME),
      .groups = "drop"
    )

## Now summarize these attention scores across the GBD term and sum the total_attention_score_per_pubmed
  partial_matches_unique <- partial_matches_filtered %>%
    group_by(`GBD term`) %>%
    summarise(
      total_attention_score = sum(total_attention_score_per_pubmed, na.rm = TRUE),
      weighted_n = first(weighted_n),
      nhits = first(nhits),
      weighted_nhits = first(weighted_nhits),
      weighted_attention_score_impact_factor = first(weighted_attention_score_impact_factor),
      unique_pubmed_ids = n_distinct(PUBMEDID),
      MAPPED_TRAIT_URI = first(MAPPED_TRAIT_URI),
      GBD_TRAIT_URI = first(GBD_TRAIT_URI),
      MAPPED_TRAIT_NAME = first(MAPPED_TRAIT_NAME),
      .groups = "drop"
    )


## Identify the GBD terms that are not mapped via identifiers or trait with GWAS to combine them
##in one data set. Now we will have final_matched_gbd and final_unmatched_gbd.


## Identify the rows that were matched partially
matched_partial_gbd <- partial_matches_unique %>%
  select(GBD_TRAIT_URI, `GBD term`) %>%
  distinct()

## Get final unmatched rows
final_unmatched_gbd <- anti_join(combined_unmatched_terms, matched_partial_gbd, by = "GBD term")


## Check for duplicates in final_unmatched_gbd
duplicates_final_unmatched_gbd <- final_unmatched_gbd[duplicated(final_unmatched_gbd$`GBD term`), ]
if (nrow(duplicates_final_unmatched_gbd) > 0) {
  print("Duplicates found in final_unmatched_gbd:")
  print(duplicates_final_unmatched_gbd)
} else {
  print("No duplicates found in final_unmatched_gbd.")
}


## final matched_gbd (combine those identified via Identifier and Partially)
final_matched_gbd <- bind_rows(
  combined_matched_terms %>% mutate(Match_Type = "Identifier"),
  partial_matches_unique %>% mutate(Match_Type = "Partial")
)

## Check for duplicates in final matched_gbd
duplicates_final_matched_gbd <- final_matched_gbd[duplicated(final_matched_gbd$`GBD term`), ]
if (nrow(duplicates_final_matched_gbd) > 0) {
  print("Duplicates found in final_matched_gbd:")
  print(duplicates_final_matched_gbd)
} else {
  print("No duplicates found in final_matched_gbd")
}


## Save the results to a CSV file
#write.csv(final_unmatched_gbd, "final_unmatched_gbd.csv", row.names = FALSE)
#write.csv(final_matched_gbd, "final_matched_gbd.csv", row.names = FALSE)

#If there are no matches for a GBD trait then the attention score = 0. also, we ill combine matced and unmatched GBD terms in one data set (combined dataset). It has 377 GBD terms, as we started.

##  Add 'total_attention_score' column with value 0 to final_unmatched_gbd
final_unmatched_gbd <- final_unmatched_gbd %>%
  mutate(total_attention_score = 0)

##  Ensure both datasets have the same columns
## Here, we ensure that final_unmatched_gbd has all necessary columns
required_columns <- names(final_matched_gbd)

## Add any missing columns to final_unmatched_gbd and set their values to NA
for (col in required_columns) {
  if (!(col %in% names(final_unmatched_gbd))) {
    final_unmatched_gbd[[col]] <- NA
  }
}

## Combine the datasets
combined_dataset <- bind_rows(final_matched_gbd, final_unmatched_gbd)

combined_dataset <- combined_dataset %>%
  distinct(`GBD term`, .keep_all = TRUE)


## Save the combined dataset to a CSV file
write.csv(combined_dataset, "combined_dataset.csv", row.names = FALSE)




