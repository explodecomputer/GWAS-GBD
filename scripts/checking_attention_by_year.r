library(here)
library(data.table)
library(dplyr)
library(ggplot2)

gwas_attention <- fread(here("Data/merged_dataset_exclude_Injuries_2023_updated.csv")) %>%
  select(cause_name = `Cause Name`, cause_id, total_attention_score) %>%
  filter(!duplicated(cause_id))

gwas_attention2 <- fread(here("Data/merged_dataset_exclude_Injuries_2023_updated_5.csv")) %>%
  select(cause_name, cause_id, total_attention_score, analysis_type, time_strata) %>%
  filter(!duplicated(paste(cause_id, analysis_type, time_strata)))

str(gwas_attention)
str(gwas_attention2)

# Merge the two datasets based on cause_id
merged_attention <- merge(gwas_attention, gwas_attention2, by = "cause_id", all.x = TRUE)
str(merged_attention)

merged_attention$what <- paste(merged_attention$analysis_type, merged_attention$time_strata)

group_by(merged_attention, what) %>%
  summarise(n = sum(!is.na(total_attention_score.x) & !is.na(total_attention_score.y)), 
            cor = cor(total_attention_score.x, total_attention_score.y, method = "pearson", use = "complete.obs")) %>%
  as.data.frame()


temp <- gwas_attention2 %>%
    filter(analysis_type == "sliding_3yr") 

alltraits <- tibble(cause_id=unique(gwas_attention2$cause_id))

l <- list()
k <- 1
for(i in unique(temp$time_strata)) {
  for(j in unique(temp$time_strata)) {
    print(paste(i, j))
    t1 <- temp %>% filter(time_strata == i) %>% left_join(alltraits, ., by = "cause_id") %>% mutate(total_attention_score = ifelse(is.na(total_attention_score), 0, total_attention_score))
    t2 <- temp %>% filter(time_strata == j) %>% left_join(alltraits, ., by = "cause_id") %>% mutate(total_attention_score = ifelse(is.na(total_attention_score), 0, total_attention_score))
    merged_temp <- merge(t1, t2, by = "cause_id", suffixes = c(".x", ".y"))
    l[[k]] <- tryCatch(
        {
        tibble(
            yr1 = i,
            yr2 = j,
            n = sum(!is.na(merged_temp$total_attention_score.x) & !is.na(merged_temp$total_attention_score.y)), 
            cor = cor(rank(merged_temp$total_attention_score.x), rank(merged_temp$total_attention_score.y), method = "pearson", use = "complete.obs")
        )
        }, error = function(e) {
            tibble(
                yr1 = i,
                yr2 = j,
                n = NA, 
                cor = NA
            )
        }
    )
    k <- k + 1
  }
}
l <- bind_rows(l)

l %>% ggplot(aes(x = yr1, y = yr2, fill = cor)) +
    geom_tile() +
    geom_text(aes(label = round(cor, 2) * 100), size = 2.5) +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, limits = c(-1, 1)) +
    theme_minimal() +
    labs(x = "Year (3yr sliding window)",
         y = "Year (3yr sliding window)",
         fill = "Correlation between attention scores") +
    theme(legend.position="none")

ggsave(here("figures/correlation_attention_by_year.pdf"), width = 8, height = 9)

# by year

hist(temp$total_attention_score, breaks=100)



temp <- gwas_attention2 %>%
    filter(analysis_type == "year") 

l <- list()
k <- 1
for(i in unique(temp$time_strata)) {
  for(j in unique(temp$time_strata)) {
    print(paste(i, j))
    t1 <- temp %>% filter(time_strata == i)
    t2 <- temp %>% filter(time_strata == j)
    merged_temp <- merge(t1, t2, by = "cause_id", suffixes = c(".x", ".y"))
    l[[k]] <- tryCatch(
        {
        tibble(
            yr1 = i,
            yr2 = j,
            n = sum(!is.na(merged_temp$total_attention_score.x) & !is.na(merged_temp$total_attention_score.y)), 
            cor = cor(merged_temp$total_attention_score.x, merged_temp$total_attention_score.y, method = "pearson", use = "complete.obs")
        )
        }, error = function(e) {
            tibble(
                yr1 = i,
                yr2 = j,
                n = NA, 
                cor = NA
            )
        }
    )
    k <- k + 1
  }
}
l <- bind_rows(l)    

l %>% ggplot(aes(x = yr1, y = yr2, fill = cor)) +
    geom_tile() +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, limits = c(-1, 1)) +
    theme_minimal() +
    theme(legend.position="bottom") +
    labs(title = "Correlation of Total Attention Scores between Time Strata",
         x = "Year (3yr sliding window)",
         y = "Year (3yr sliding window)",
         fill = "Correlation between attention scores")



## Issues
# year 2024 and sliding_3yr 2024 are the same - there is no year 2025 or year 2026. Correct to stop at sliding_3yr 2024 but need to include years 24-26, and also have year 2025 and year 2026
# There are still 69 terms with zero attention

# Read in the new dataset
gwas_attention2 <- fread(here("Data/merged_dataset_exclude_Injuries_2023_updated_2.csv")) %>%
  select(cause_name = `Cause Name`, cause_id, total_attention_score, analysis_type, time_strata) %>%
  filter(!duplicated(paste(cause_id, analysis_type, time_strata)))
 
# Only keep the 'year' scores
gwas_attention_yr <- subset(gwas_attention2, analysis_type == "year")
 
# Get the first year, and the last year (subtracting 2 to account for the sliding 3-year window)
min_year <- min(gwas_attention_yr$time_strata)
max_year <- max(gwas_attention_yr$time_strata) - 2
 
# Create the sliding 3-year window dataset
gwas_attention_sliding <- lapply(min_year:max_year, function(i) {
        gwas_attention_yr %>%
            filter(time_strata %in% c(i, i + 1, i + 2)) %>%
            group_by(cause_name, cause_id) %>%
            summarise(
                analysis_type = "sliding_3yr",
                time_strata = i,
                total_attention_score = sum(total_attention_score, na.rm = TRUE)
            )
}) %>% bind_rows()
 
# Check how many non-zero attention scores there are per sliding_window_year
gwas_attention_sliding %>%
    group_by(time_strata) %>%
    summarise(n = sum(!is.na(total_attention_score))) %>%
    as.data.frame()


# How does attention change over time

allyears <- tibble(time_strata = unique(gwas_attention2$time_strata, na.rm=TRUE))
temp <- subset(gwas_attention2, analysis_type == "year")

group_by(temp, cause_name) %>%
    do({
        x <- left_join(allyears, ., by = "time_strata") %>%
            mutate(total_attention_score = ifelse(is.na(total_attention_score), 0, total_attention_score)) %>%
            select(time_strata, total_attention_score) %>%
            filter(!is.na(time_strata)) %>%
            mutate(total_attention_score_std = total_attention_score / max(total_attention_score, na.rm = TRUE))
    }) %>% 
        ggplot(aes(x = time_strata, y = total_attention_score_std, group = cause_name)) +
        geom_smooth(se=FALSE, method="lm")


gbd <- fread(here("Data/merged_dataset_exclude_Injuries_2023_updated.csv")) %>%
    filter(age_name == "All ages", sex_name == "Both", year == 2023, location_name %in% c("High SDI", "Low SDI")) %>%
    select(cause_name = `Cause Name`, cause_id, DALY, location_name) %>% 
    group_by(location_name) %>%
    mutate(importance = rank(DALY)) %>%
    ungroup() %>%
    group_by(cause_name) %>%
    mutate(high_sdi_bias = importance[1] - importance[2])
gbd

library(tidyr)
gbd %>% select(cause_name, location_name, importance) %>%
    spread(key = location_name, value = importance) %>%
    ggplot(aes(x = `High SDI`, y = `Low SDI`)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    theme_minimal()

hist(gbd$high_sdi_bias)
gbd %>% arrange(high_sdi_bias)
gbd %>% arrange(desc(high_sdi_bias))

gbd_temp <- gbd %>% 
    filter(!duplicated(cause_name)) %>% 
    select(cause_name, high_sdi_bias) %>% 
    ungroup() %>%
    mutate(gbd_cat = cut(high_sdi_bias, quantile(high_sdi_bias, probs = c(0, 0.33, 0.66, 1)), include.lowest = TRUE))

temp <- subset(gwas_attention2, analysis_type == "sliding_3yr")
group_by(temp, cause_name) %>%
    do({
        x <- left_join(allyears, ., by = "time_strata") %>%
            mutate(total_attention_score = ifelse(is.na(total_attention_score), 0, total_attention_score)) %>%
            select(time_strata, total_attention_score) %>%
            filter(!is.na(time_strata)) %>%
            mutate(total_attention_score_std = total_attention_score / max(total_attention_score, na.rm = TRUE))
    }) %>% 
    inner_join(gbd_temp, by = "cause_name") %>%
        ggplot(aes(x = time_strata, y = total_attention_score_std, group = cause_name)) +
        geom_smooth(se=FALSE) +
        facet_grid(. ~ high_sdi_bias > 0)

group_by(temp, cause_name) %>%
    do({
        x <- left_join(allyears, ., by = "time_strata") %>%
            mutate(total_attention_score = ifelse(is.na(total_attention_score), 0, total_attention_score)) %>%
            select(time_strata, total_attention_score) %>%
            filter(!is.na(time_strata)) %>%
            mutate(total_attention_score_std = total_attention_score / max(total_attention_score, na.rm = TRUE))
    }) %>% 
    inner_join(gbd_temp, by = "cause_name") %>%
        ggplot(aes(x = time_strata, y = total_attention_score_std, colour = high_sdi_bias > 0)) +
        geom_point() +
        geom_smooth(se=FALSE)


gbd_gwas_merged <- group_by(temp, cause_name) %>%
    do({
        x <- left_join(allyears, ., by = "time_strata") %>%
            mutate(total_attention_score = ifelse(is.na(total_attention_score), 0, total_attention_score)) %>%
            select(time_strata, total_attention_score) %>%
            filter(!is.na(time_strata)) %>%
            mutate(total_attention_score_std = total_attention_score / max(total_attention_score, na.rm = TRUE))
    }) %>% 
    inner_join(gbd_temp, by = "cause_name")



gbd_gwas_merged %>% ggplot(aes(x = time_strata, y = total_attention_score_std, group = cause_name)) +
    geom_smooth(se=FALSE, method="lm") +
    facet_grid(. ~ gbd_cat)

gbd_gwas_merged %>% ggplot(aes(x = time_strata, y = total_attention_score_std)) +
    geom_point() +
    geom_smooth(se=TRUE, method="lm") +
    facet_grid(. ~ gbd_cat)




gbd_gwas_merged <- group_by(temp, cause_name) %>%
    do({
        x <- left_join(allyears, ., by = "time_strata") %>%
            mutate(total_attention_score = ifelse(is.na(total_attention_score), 0, total_attention_score)) %>%
            select(time_strata, total_attention_score) %>%
            filter(!is.na(time_strata)) %>%
            mutate(total_attention_score_std = rank(total_attention_score))
    }) %>% 
    group_by(time_strata) %>%
    mutate(total_attention_score_std2 = rank(total_attention_score)) %>%
    inner_join(gbd_temp, by = "cause_name")

gbd_gwas_merged %>% ggplot(aes(x = time_strata, y = total_attention_score_std2)) +
    geom_point() +
    geom_smooth(se=TRUE, method="lm") +
    facet_grid(. ~ gbd_cat)

gbd_gwas_merged %>% ggplot(aes(x = time_strata, y = total_attention_score_std2, group=cause_name)) +
    geom_smooth(se=FALSE) +
    facet_grid(. ~ gbd_cat)

gbd_gwas_merged %>% ggplot(aes(x = time_strata, y = total_attention_score_std2, group=cause_name)) +
    geom_smooth(se=FALSE, method="lm") +
    facet_grid(. ~ gbd_cat)
