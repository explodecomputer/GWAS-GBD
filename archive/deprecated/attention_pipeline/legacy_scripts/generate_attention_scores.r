#######################################################
###Generate GWAS attention scores using GWAS Catalog###
#######################################################

# Load relevant packages 
library(dplyr)
library(data.table)
library(tidyr)
library(stringr)
library(here)
library(readxl)

# Create EFO count

## Read in GWAS catalog data
a <- read_excel(here("data/gwas_catalog_v1.0.2.1-studies_r2024-06-07.xlsx"))

## Visualise association count
hist(a$`ASSOCIATION COUNT`, breaks=100)

## Separate EFO terms
a <- a %>%
    tidyr::separate_rows(MAPPED_TRAIT_URI, sep = ", ")

print(a, 10)

## Identify number of EFO terms per publication
pubmed_count <- a %>% group_by(PUBMEDID) %>%
    summarise(
        n_efo = length(unique(MAPPED_TRAIT_URI))
    ) %>% arrange(desc(n_efo))

pubmed_count

## Add EFO count per PUBMED ID to the data frame
a <- left_join(a, pubmed_count, by="PUBMEDID")
str(a)

# Generate attention scores 

attention <- a %>% group_by(
    MAPPED_TRAIT_URI
) %>%
    summarise(
        n = n(),
        weighted_n = sum(1/n_efo),
        nhits = sum(`ASSOCIATION COUNT`),
        weighted_nhits = sum(`ASSOCIATION COUNT` / n_efo)
    )

str(attention)

## Format 'Impact factor' column 
a$`Impact factor` <- trimws(a$`Impact factor`)
a$`Impact factor` <- gsub("<C2><B7>", ".", a$`Impact factor`)
a$`Impact factor` <- as.numeric(a$`Impact factor`)
a$`Impact factor`[is.na(a$`Impact factor`)] <- 0

str(a)
unique(a$`Impact factor`)
summary(a$`Impact factor`)

## Prepare output for mapping to GBD disease burden data 
attention <- a %>%
  group_by(MAPPED_TRAIT_URI) %>%
  summarise(
    n = n(),
    weighted_n = sum(1 / n_efo),
    nhits = sum(`ASSOCIATION COUNT`),
    weighted_nhits = sum(`ASSOCIATION COUNT` / n_efo),
    weighted_attention_score_impact_factor = sum((1 / n_efo) * `Impact factor`),
     DISEASE_TRAIT = first(`DISEASE/TRAIT`),
    PUBMEDID = paste(unique(PUBMEDID), collapse = ", ")
  )

# Expand PUBMEDID into separate rows
attention <- attention %>%
  separate_rows(PUBMEDID, sep = ", ")


str(attention)


