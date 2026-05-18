library(dplyr)
library(data.table)
library(here)
library(ggplot2)

april1 <- fread(here("Data/april2025/by_sdi_and_age/IHME-GBD_2021_DATA-2c936676-1.csv"))
december1 <- fread(here("Data/december2025/gbd_gwas_paper_data_1.csv"))

dim(april1)
dim(december1)

head(april1)
head(december1)
table(april1$sex_id)
table(december1$sex)

table(april1$location_name)
table(december1$location_name)

table(april1$year)
table(december1$year)

table(april1$cause_name)
table(december1$cause_name)

old_causes <- unique(april1$cause_name)
new_causes <- unique(december1$cause_name)
old_causes[!old_causes %in% new_causes]
new_causes[!new_causes %in% old_causes]

# which EFO mapped terms are missing?
# a <- Second_part_GBD$`GBD term`[!Second_part_GBD$`GBD term` %in% new_causes]
# b <- First_part_GBD$`GBD term`[!First_part_GBD$`GBD term` %in% new_causes]
# any(b %in% a)
# c(a,b)
all_in <- c(Second_part_GBD$`GBD term`, First_part_GBD$`GBD term`)
new_causes[!new_causes %in% all_in] # these are all injuries or the level 1 totals or the ones I just added

table(april1$age_name)
table(december1$age_group_name)

april1$age_name <- gsub("-", " to ", april1$age_name)
april1$age_name <- gsub(" years", "", april1$age_name)

table(unique(april1$age_name) %in% unique(december1$age_group_name))

april1$year[april1$year == 2021] <- 2023

dat1 <- inner_join(
    april1, december1,
    by = c("location_name" = "location_name", "year" = "year_id", "age_name" = "age_group_name", "cause_name" = "cause_name"
    )
)

dat1 %>%
    ggplot(aes(x=val.x, y=val.y)) +
        geom_point() +
        geom_abline(slope=1, intercept=0, color='red') +
        facet_grid(location_name ~ year)

###

april2 <- fread(here("Data/april2025/by_sdi_and_sex_3years/IHME-GBD_2021_DATA-f187b5da-1.csv"))
december2 <- fread(here("Data/december2025/gbd_gwas_paper_data_2.csv"))

dim(april2)
dim(december2)

head(april2)
head(december2)

table(april2$sex_name)
table(december2$sex)

table(april2$location_name)
table(december2$location_name)

table(april2$year)
table(december2$year)

table(april2$cause_name)
table(december2$cause_name)

table(april2$age_name)
table(december2$age_group_name)

april2$year[april2$year == 2021] <- 2023

dat2 <- inner_join(
    april2, december2,
    by = c("location_name" = "location_name", "year" = "year_id", "sex_name" = "sex", "cause_name" = "cause_name"
    )
)

dim(dat2)
cor(dat2$val.x, dat2$val.y)

dat2 %>%
    ggplot(aes(x=val.x, y=val.y)) +
        geom_point() +
        geom_abline(slope=1, intercept=0, color='red') +
        facet_grid(location_name ~ year)

###

april3 <- fread(here("Data/april2025/by_sdi_and_year/IHME-GBD_2021_DATA-45f67a86-1.csv"))
december3 <- fread(here("Data/december2025/gbd_gwas_paper_data_3.csv"))

dim(april3)
dim(december3)

head(april3)
head(december3)

table(april3$sex_name)
table(december3$sex)

table(april3$location_name)
table(december3$location_name)

table(april3$year)
table(december3$year)

table(april3$cause_name)
table(december3$cause_name)

table(april3$age_name)
table(december3$age_group_name)

dat3 <- inner_join(
    april3, december3,
    by = c("location_name" = "location_name", "year" = "year_id", "cause_name" = "cause_name"
    )
)

dim(dat3)
cor(dat3$val.x, dat3$val.y)

dat3 %>%
    ggplot(aes(x=val.x, y=val.y)) +
        geom_point() +
        geom_abline(slope=1, intercept=0, color='red') +
        facet_grid(location_name ~ .)

###

april4 <- fread(here("Data/april2025/by_country/IHME-GBD_2021_DATA-84f55027-1.csv"))
december4 <- fread(here("Data/december2025/gbd_gwas_paper_data_4.csv"))

dim(april4)
dim(december4)

head(april4)
head(december4)

table(april4$sex_name)
table(december4$sex)

table(april4$location_name)
table(december4$location_name)

table(april4$year)
table(december4$year)

table(april4$cause_name)
table(december4$cause_name)

table(april4$age_name)
table(december4$age_group_name)

dat4 <- inner_join(
    april4, december4,
    by = c("location_name" = "location_name", "year" = "year_id", "cause_name" = "cause_name"
    )
)

dim(dat4)
cor(dat4$val.x, dat4$val.y)

dat4 %>%
    ggplot(aes(x=val.x, y=val.y)) +
        geom_point() +
        geom_abline(slope=1, intercept=0, color='red')


### checking numbers

gbd_output <- fread(here("data/december2025/gbd_gwas_paper_data_2.csv"))
rayan_latest <- fread(here("data/merged_dataset_exclude_Injuries_2023_updated_5.csv"))

unique_gbd <- unique(gbd_output$cause_name)
unique_rayan <- unique(rayan_latest$cause_name)

unique_rayan[!unique_rayan %in% unique_gbd]
unique_gbd[!unique_gbd %in% unique_rayan]
