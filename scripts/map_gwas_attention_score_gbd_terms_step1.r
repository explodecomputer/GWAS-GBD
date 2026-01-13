##########################################################
###Map GWAS attention scores to GBD disease burden data###
##########################################################

# This mapping is performed in two parts: 
# 1. Map EFO terms in GWAS Catalog to GBD terms 
# 2. Map EFO terms to GBD terms using descendants from the tree via ontologyIndex library

# Read in relevant packages 
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
library(boot)
library(here)


# Part 1: Map EFO terms in GWAS Catalog to GBD terms

First_part_GBD<- read_xlsx(here("data", "First_part_GBD.xlsx"))

str(First_part_GBD)

## Unique matched rows  by GBD term
First_part_GBD<- First_part_GBD%>%
  distinct(`GBD term`, .keep_all = TRUE)

## Specify columns that contain EFO terms 
efo_columns <- c("EFO 1", "EFO 2", "EFO 3", "EFO 4", "EFO 5", "EFO 6",
                 "EFO 7", "EFO 8", "EFO 9", "EFO 10", "EFO 11", "EFO 12",
                 "EFO 13", "EFO 14", "EFO 15", "EFO 16", "EFO 17", "EFO 18",
                 "EFO 19", "EFO 20", "EFO 21", "EFO 22", "EFO 23", "EFO 24",
                 "EFO 25", "EFO 26", "EFO 27", "EFO 28", "EFO 29", "EFO 30")

## Reshape the GBD data to gather all relevant columns into key-value pairs and drop rows with NA EFO terms
gbd_long <- First_part_GBD%>%
pivot_longer(cols = all_of(efo_columns), names_to = "EFO_number", values_to =  "MAPPED_TRAIT_URI") %>% drop_na(MAPPED_TRAIT_URI)

gbd_long

## Separate rows based on a separator (assuming comma-separated EFO terms) and remove whitespace
gbd_long <- gbd_long %>%
  separate_rows(MAPPED_TRAIT_URI, sep = ",") %>%
  mutate(MAPPED_TRAIT_URI = trimws(MAPPED_TRAIT_URI))

gbd_long

## Format, clean and standardise MAPPED_TRAIT_URI column in gbd_long file 
gbd_long$MAPPED_TRAIT_URI <- tolower(trimws(gbd_long$MAPPED_TRAIT_URI))
gbd_long$MAPPED_TRAIT_URI <- tolower(gbd_long$MAPPED_TRAIT_URI)
gbd_long$MAPPED_TRAIT_URI <- trimws(gbd_long$MAPPED_TRAIT_URI)
gbd_long$MAPPED_TRAIT_URI <- gsub("\\s+", "", gbd_long$MAPPED_TRAIT_URI)
gbd_long$MAPPED_TRAIT_URI <- gsub(" ", "", gbd_long$MAPPED_TRAIT_URI)
gbd_long$MAPPED_TRAIT_URI <- tolower(gsub("\\s+", "", gbd_long$MAPPED_TRAIT_URI))
gbd_long$MAPPED_TRAIT_URI <- gsub("[:\\s]", "", gbd_long$MAPPED_TRAIT_URI)
gbd_long$MAPPED_TRAIT_URI <- gsub("\"", "", gbd_long$MAPPED_TRAIT_URI)
gbd_long$MAPPED_TRAIT_URI <- gsub("_", ":", gbd_long$MAPPED_TRAIT_URI)
gbd_long$MAPPED_TRAIT_URI <- str_squish(gbd_long$MAPPED_TRAIT_URI)

(head(gbd_long$MAPPED_TRAIT_URI, 20))

## check which ones are in gbd_long
gbd_terms <- unique(gbd_long$`GBD term`) #224
# new_causes[!new_causes %in% gbd_terms]
# gbd_terms[!gbd_terms %in% new_causes]

## Function to clean and standardize MAPPED_TRAIT_URI column in GWAS attention score file 
clean_mapped_trait_uri <- function(uri) {
  # Convert to lowercase
  uri <- tolower(uri)
  # Trim leading/trailing whitespace
  uri <- trimws(uri)
  # Remove all internal spaces explicitly
  uri <- gsub("\\s+", "", uri)
  # Remove quotation marks
  uri <- gsub("\"", "", uri)
  # Replace _ with :
  uri <- gsub("_", ":", uri)
  # Remove colons and spaces
  uri <- gsub("[:\\s]", "", uri)
  # Remove prefixes and ensure consistency
  uri <- sub(".*[:/]", "", uri)
  return(uri)
}

## Apply the function to MAPPED_TRAIT_URI column
attention$MAPPED_TRAIT_URI <- clean_mapped_trait_uri(attention$MAPPED_TRAIT_URI)


## Convert "na" strings to actual NA values
attention$MAPPED_TRAIT_URI[attention$MAPPED_TRAIT_URI == "na"] <- NA

## Remove rows with NA in MAPPED_TRAIT_URI
attention <- attention %>%
  filter(!is.na(MAPPED_TRAIT_URI))

(head(attention$MAPPED_TRAIT_URI, 20))

# Map GWAS data and GBD (First_part_GBD) via identifiers (EFO terms)- for those with no descendants. Then grouping those terms by GBD term and PUBMEDID (as we need to ensure the studies are independent when we sum the n ).

## Merge the data frames
merged_data <- merge(attention, gbd_long, by = "MAPPED_TRAIT_URI", all.x = TRUE)

## Identify matched rows
matched_rows <- inner_join(attention, gbd_long, by = "MAPPED_TRAIT_URI")


## Group by GBD term and PUBMEDID and summarize within these groups
 matched_rows_unique <- matched_rows %>%
    group_by(`GBD term`, PUBMEDID) %>%
    summarise(
      total_attention_score_per_pubmed = first(n, na.rm = TRUE),
      weighted_n = first(weighted_n),
      nhits = first(nhits),
      weighted_nhits = first(weighted_nhits),
      weighted_attention_score_impact_factor = first(weighted_attention_score_impact_factor),
      unique_pubmed_ids = n_distinct(PUBMEDID),
      MAPPED_TRAIT_URI = first(MAPPED_TRAIT_URI),
      DISEASE_TRAIT = first(DISEASE_TRAIT),
      .groups = "drop"
    )

## Now summarize these attention scores across the GBD term and sum the total_attention_score_per_pubmed
  unique_matched_gbd_terms_First_part_GBD <- matched_rows_unique %>%
    group_by(`GBD term`) %>%
    summarise(
      total_attention_score = sum(total_attention_score_per_pubmed, na.rm = TRUE),
      weighted_n = first(weighted_n),
      nhits = first(nhits),
      weighted_nhits = first(weighted_nhits),
      weighted_attention_score_impact_factor = first(weighted_attention_score_impact_factor),
      unique_pubmed_ids = n_distinct(PUBMEDID),
      MAPPED_TRAIT_URI = first(MAPPED_TRAIT_URI),
      DISEASE_TRAIT = first(DISEASE_TRAIT),
      .groups = "drop"
    )


## Identify unmatched rows from attention
unmatched_attention <- anti_join(attention, gbd_long, by = "MAPPED_TRAIT_URI")

## Identify unmatched rows from gbd_long
unmatched_gbd <- anti_join(gbd_long, attention, by = "MAPPED_TRAIT_URI")

## Ensure unique unmatched_gbd rows by GBD term
unmatched_gbd_unique <- unmatched_gbd %>%
  distinct(`GBD term`, .keep_all = TRUE)

## Extract unique GBD terms for matched and unmatched entries
matched_gbd_terms <- matched_rows_unique$`GBD term` %>% unique()
unmatched_gbd_terms <- unmatched_gbd_unique$`GBD term` %>% unique()

## Remove any GBD terms from unmatched_gbd that are also in matched_rows
unique_unmatched_gbd_terms <- setdiff(unmatched_gbd_terms, matched_gbd_terms)

## Ensure unique unmatched_gbd rows by GBD term
unique_unmatched_gbd_terms_First_part_GBD <- unmatched_gbd_unique %>%
  filter(`GBD term` %in% unique_unmatched_gbd_terms)

## Save the data frames to Excel files
file_path <- here("data", "unique_matched_gbd_terms_First_part_GBD.xlsx")
write.xlsx(unique_matched_gbd_terms_First_part_GBD, file = file_path)

file_path <- here("data", "unique_unmatched_gbd_terms_First_part_GBD.xlsx")
write.xlsx(unique_unmatched_gbd_terms_First_part_GBD, file = file_path)

## Verification of the summing up of n in total_attention_score_per_pubmed "as long as they are independent publications" (all PUBMEDID are unique)

## Select a specific GBD term for manual verification
selected_gbd_term <- "Acne vulgaris"  # Example term

## Filter the intermediate summarization for the selected GBD term
filtered_intermediate <- matched_rows_unique %>%
  filter(`GBD term` == selected_gbd_term)

## Print the filtered data for manual verification
print(filtered_intermediate)

## Sum the `n` values manually for verification
manual_sum_n <- sum(filtered_intermediate$total_attention_score_per_pubmed)
print(paste("Manual sum of n for", selected_gbd_term, ":", manual_sum_n))

