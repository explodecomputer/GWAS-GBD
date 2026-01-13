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


# Part 2: Map EFO terms to GBD terms using descendants from the tree via ontologyIndex library

## Finding a match between GWAS and GBD (Second_part_GBD) via identifiers- for those with  descendants. We need to change the format of the identifiers again to get the descendants using the ontology from the tree then reverse it again to match it with GWAS (to ensure the consistency in the matching process). Also, as we did not part one, we will group them by GBD term and PUBMEDID.

## Define utility functions
format_identifier <- function(identifier) {
  formatted <- gsub(" ", "", identifier) # Remove any spaces
  formatted <- toupper(formatted) # Convert to uppercase
  formatted <- gsub("([A-Z]+)([0-9]+)", "\\1:\\2", formatted) # Add colon
  return(formatted)
}

revert_identifier <- function(identifier) {
  reverted <- gsub(":", "", identifier) # Remove colons
  return(tolower(reverted)) # Convert to lowercase
}

clean_mapped_trait_uri <- function(uri) {
  uri <- tolower(uri) # Convert to lowercase
  uri <- trimws(uri) # Trim leading/trailing whitespace
  uri <- gsub("\\s+", "", uri) # Remove all internal spaces explicitly
  uri <- gsub("\"", "", uri) # Remove quotation marks
  uri <- gsub("_", ":", uri) # Replace _ with :
  uri <- gsub("[:\\s]", "", uri) # Remove colons and spaces
  uri <- sub(".*[:/]", "", uri) # Remove prefixes and ensure consistency
  return(uri)
}

## Read data files
# ontology <- get_OBO("/Users/rayanaloliet/Desktop/GWAS Catalog/efo-obo.txt")
ontology <- get_OBO(here("data/efo-obo.txt"))
file_path <- here("data", "Second_part_GBD.xlsx") 
Second_part_GBD <- read_excel(file_path)


## Extract and combine identifiers from Second_part_GBD
identifiers_list <- list(
  Second_part_GBD$`MAPPED_TRAIT_URI...2`,
  Second_part_GBD$`MAPPED_TRAIT_URI...3`,
  Second_part_GBD$`MAPPED_TRAIT_URI...4`,
  Second_part_GBD$`MAPPED_TRAIT_URI...5`,
  Second_part_GBD$`MAPPED_TRAIT_URI...6`,
  Second_part_GBD$`MAPPED_TRAIT_URI...7`,
  Second_part_GBD$`MAPPED_TRAIT_URI...8`,
  Second_part_GBD$`MAPPED_TRAIT_URI...9`,
  Second_part_GBD$`MAPPED_TRAIT_URI...10`,
  Second_part_GBD$`MAPPED_TRAIT_URI...11`,
  Second_part_GBD$`MAPPED_TRAIT_URI...12`,
  Second_part_GBD$`MAPPED_TRAIT_URI...13`
)
identifiers <- do.call(c, lapply(identifiers_list, na.omit))

## Format the identifiers
formatted_identifiers <- sapply(identifiers, format_identifier)

## Extract descendants for each identifier
descendants_list <- list()
for (i in 1:length(formatted_identifiers)) {
  id <- formatted_identifiers[i]
  if (id %in% ontology$id) {
    descendants <- get_descendants(ontology, id)
    descendants_list[[id]] <- descendants
  } else {
    descendants_list[[id]] <- NA
  }
}

## Determine the maximum number of descendants
max_descendants <- max(sapply(descendants_list, function(x) if (is.null(x) || all(is.na(x))) 0 else length(x)), na.rm = TRUE)

## Create a data frame for descendants
descendants_df <- data.frame(MAPPED_TRAIT_URI = character(), stringsAsFactors = FALSE)
for (i in 1:max_descendants) {
  descendants_df[paste0("Descendant_", i)] <- character()
}

## Populate the data frame with identifiers and their descendants
for (i in seq_along(formatted_identifiers)) {
  id <- formatted_identifiers[i]
  descendants <- descendants_list[[id]]
  if (is.null(descendants) || all(is.na(descendants))) {
    row <- c(MAPPED_TRAIT_URI = id, rep("", max_descendants))
  } else {
    row <- c(MAPPED_TRAIT_URI = id, descendants, rep("", max_descendants - length(descendants)))
  }

  row_df <- as.data.frame(t(row), stringsAsFactors = FALSE)
  colnames(row_df) <- colnames(descendants_df)
  descendants_df <- rbind(descendants_df, row_df)
}

## Convert columns to character type
descendants_df[] <- lapply(descendants_df, as.character)

## Add the GBD_TERM column
gbd_terms <- Second_part_GBD$`GBD term`
descendants_df$GBD_TERM <- gbd_terms[match(descendants_df$MAPPED_TRAIT_URI, formatted_identifiers)]

## Reorder columns to place GBD_TERM after MAPPED_TRAIT_URI
if ("GBD_TERM" %in% colnames(descendants_df)) {
  descendants_df <- descendants_df %>% select(MAPPED_TRAIT_URI, GBD_TERM, everything())
}

## Revert the identifiers to their original format
descendants_df <- descendants_df %>% mutate(across(c(MAPPED_TRAIT_URI, starts_with("Descendant_")), ~sapply(., revert_identifier)))

## Ensure identifiers are formatted correctly in attention
attention <- attention %>% mutate(MAPPED_TRAIT_URI = format_identifier(MAPPED_TRAIT_URI))

## Remove duplicates in both data frames
attention <- attention %>% distinct(MAPPED_TRAIT_URI, .keep_all = TRUE)
descendants_df <- descendants_df %>% distinct(MAPPED_TRAIT_URI, .keep_all = TRUE)

## Reshape descendants_df to long format
descendants_long <- descendants_df %>%
  pivot_longer(cols = starts_with("Descendant_"), names_to = "Descendant", values_to = "Descendant_URI") %>%
  filter(Descendant_URI != "") %>%
  select(MAPPED_TRAIT_URI, GBD_TERM, Descendant_URI)

## Clean and standardize Descendant_URI
descendants_long$Descendant_URI <- tolower(trimws(descendants_long$Descendant_URI))
descendants_long$Descendant_URI <- gsub("\\s+", "", descendants_long$Descendant_URI)
descendants_long$Descendant_URI <- gsub(" ", "", descendants_long$Descendant_URI)
descendants_long$Descendant_URI <- gsub("[:\\s]", "", descendants_long$Descendant_URI)
descendants_long$Descendant_URI <- gsub("\"", "", descendants_long$Descendant_URI)
descendants_long$Descendant_URI <- gsub("_", ":", descendants_long$Descendant_URI)
descendants_long$Descendant_URI <- str_squish(descendants_long$Descendant_URI)
descendants_long <- descendants_long %>% distinct(Descendant_URI, .keep_all = TRUE)

## Clean and standardize MAPPED_TRAIT_URI in attention
attention$MAPPED_TRAIT_URI <- clean_mapped_trait_uri(attention$MAPPED_TRAIT_URI)

## Identify matched entries
matched_entries <- attention %>%
  filter(MAPPED_TRAIT_URI %in% descendants_long$Descendant_URI) %>%
  left_join(descendants_long %>% select(Descendant_URI, GBD_TERM), by = c("MAPPED_TRAIT_URI" = "Descendant_URI"))


## Group by GBD term and PUBMEDID and summarize within these groups and drop na
 matched_entries_unique <- matched_entries %>%
    drop_na() %>%
    group_by(GBD_TERM, PUBMEDID) %>%
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
  unique_matched_gbd_terms_Second_part_GBD <- matched_entries_unique %>%
    group_by(GBD_TERM) %>%
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

## Identify unmatched GBD entries based on GBD_TERM in both datasets
## First, extract unique GBD terms from both datasets
matched_gbd_terms <- matched_entries$GBD_TERM %>% unique()
unmatched_gbd_terms <- Second_part_GBD$`GBD term` %>% unique()

## Identify unmatched GBD terms
unique_unmatched_gbd_terms <- setdiff(unmatched_gbd_terms, matched_gbd_terms)

## Filter the Second_part_GBD dataframe to get rows with these unique unmatched GBD terms
unique_unmatched_gbd_terms_Second_part_GBD <- Second_part_GBD %>%
  filter(`GBD term` %in% unique_unmatched_gbd_terms)

## Save the data frames to Excel files
file_path <- here("data", "unique_unmatched_gbd_terms_Second_part_GBD.xlsx")
write.xlsx(unique_unmatched_gbd_terms_Second_part_GBD, file = file_path)

file_path <- here("data", "unique_matched_gbd_terms_Second_part_GBD.xlsx")
write.xlsx(unique_matched_gbd_terms_Second_part_GBD, file = file_path)

