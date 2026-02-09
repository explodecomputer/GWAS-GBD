library(here)
library(dplyr)
library(data.table)
library(rineq)
library(ggplot2)
library(ggstance)
library(rworldmap)
library(RColorBrewer)

get_ci <- function(x) {
  out <- ci(
    ineqvar = x$total_attention_score,
    outcome = x$val, method = "direct"
  )
  l <- tibble(
    ci = out$concentration_index,
    ci_se = sqrt(out$variance),
    ci_lci = ci - 1.96 * ci_se,
    ci_uci = ci + 1.96 * ci_se,
    daly_sum = sum(x$val, na.rm=TRUE)
  )
  l
}

gwas_attention <- fread(here("Data/merged_dataset_exclude_Injuries_2023_updated.csv")) %>%
  select(cause_name = `Cause Name`, cause_id, total_attention_score) %>%
  filter(!duplicated(cause_id))

gbd1 <- fread(here("Data/december2025/gbd_gwas_paper_data_2.csv")) %>%
  rename(sex_name = sex, year = year_id)

temp1 <- inner_join(gbd1, gwas_attention, by="cause_name") %>% filter(grepl("SDI", location_name ))

length(unique(temp1$cause_name))
tempglobal <-   group_by(temp1, cause_name, year, sex_name) %>%
    summarise(nloc= n(), location_name = "Global",
              total_attention_score = first(total_attention_score),
              val = sum(val, na.rm=TRUE)) %>% ungroup
tempglobal

temp1 <- bind_rows(
  temp1 %>% filter(grepl("SDI", location_name)), 
  tempglobal
) %>% ungroup()


o1 <- group_by(temp1, location_name, sex_name, year) %>%
  do(get_ci(.))
o1$location_name <- factor(o1$location_name, levels = c(
  "High SDI", "High-middle SDI", "Middle SDI",
  "Low-middle SDI", "Low SDI", "Global"
))

o1 %>%
  dplyr::filter(year == 2023, !is.na(location_name)) %>%
ggplot(., aes(x = ci, y = sex_name)) +
  geom_point(
    aes(colour = sex_name),
    position = ggstance::position_dodge2v(height = 0.3)
  ) +
  geom_errorbarh(aes(
    xmin = ci_lci,
    xmax = ci_uci,
    colour = sex_name),
  height = 0,
  position = ggstance::position_dodge2v(height = 0.3)
  ) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_grid(location_name ~ .) +
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(),legend.position="bottom") +
  labs(x="Concentration index", y="", colour="")


##

gbd2 <- fread(here("Data/december2025/gbd_gwas_paper_data_3.csv")) %>%
  rename(sex_name = sex, year = year_id)
str(gbd2)

temp2 <- inner_join(gbd2, gwas_attention, by="cause_name") %>% filter(grepl("SDI", location_name ))

tempglobal <-   group_by(temp2, cause_name, year, sex_name) %>%
    summarise(nloc= n(), location_name = "Global",
              total_attention_score = first(total_attention_score),
              val = sum(val, na.rm=TRUE)) %>% ungroup
tempglobal

temp2 <- bind_rows(
  temp2 %>% filter(grepl("SDI", location_name)), 
  tempglobal
) %>% ungroup()
table(temp2$nloc)
table(temp2$location_name)
temp2
o2 <- group_by(temp2, location_name, sex_name, year) %>%
  do(get_ci(.))
o2$location_name <- factor(o2$location_name, levels = c(
  "High SDI", "High-middle SDI", "Middle SDI",
  "Low-middle SDI", "Low SDI", "Global"
))
o2 <- subset(o2, !is.na(o2$location_name))


o2 %>%
ggplot(., aes(y = ci, x = year)) +
  geom_errorbar(colour="grey", aes(
    ymin = ci_lci,
    ymax = ci_uci),
  width = 0) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  facet_grid(. ~ location_name) +
  geom_smooth(se=FALSE) +
  geom_point() +
  theme_bw() +
  theme(axis.text.x=element_text(angle=90, vjust=0.5)) +
  labs(x="Year", y="Concentration index")
ggsave(here("figures/ci_by_year2.pdf"), width = 10, height = 4)

o1 %>%
  dplyr::filter(year == 2023, !is.na(location_name)) %>%
ggplot(., aes(y = ci, x = sex_name)) +
  geom_errorbar(colour="grey", aes(
    ymin = ci_lci,
    ymax = ci_uci),
  width = 0) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  facet_grid(. ~ location_name) +
  geom_smooth(se=FALSE) +
  theme_bw() +
  labs(x="Sex", y="Concentration index")
ggsave(here("figures/ci_by_sex.pdf"), width = 10, height = 4)


gbd3 <- fread(here("Data/december2025/gbd_gwas_paper_data_4.csv")) %>%
  rename(sex_name = sex, year = year_id)
temp3 <- inner_join(gbd3, gwas_attention, by="cause_name")
o3 <- group_by(temp3, location_name, sex_name, year) %>%
  do(get_ci(.))

# Get the highest and lowest countries CI
o3 %>% ungroup() %>% filter(year == 2023, sex_name == "Both") %>% arrange(desc(ci)) %>% select(location_name, ci, ci_se, daly_sum) %>% as.data.frame
o3 %>% filter(year == 2023, sex_name == "Both") %>% arrange(ci)

ggplot(o3, aes(x=daly_sum, y=ci)) +
geom_point() +
scale_x_log10()



o3 %>% ungroup() %>% filter(year == 2023, sex_name == "Both") %>% arrange(desc(ci)) %>% mutate(rank=1:n()) %>%
ggplot(., aes(x=rank, y=ci)) +
  geom_point() +
  geom_errorbar(aes(ymin=ci_lci, ymax=ci_uci), width=0) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw() +
  labs(x="Rank of country", y="Concentration index") +
  scale_x_continuous(breaks = seq(0, 200, by = 20)) +
  theme(axis.text.x=element_text(angle=90, vjust=0.5))

o3 %>% filter(year == 2021, sex_name == "Both") %>% arrange(ci)

o3 %>% ungroup() %>% filter(year == 2021, sex_name == "Both") %>% filter(ci_lci > 0)


colourPalette <- brewer.pal(5,'RdPu')

spdf <- joinCountryData2Map(o3 %>% filter(year == 1990), joinCode="NAME", nameJoinColumn="location_name")
mapDevice('pdf', file="figures/map1990.pdf")
mapParams <- mapCountryData(spdf, nameColumnToPlot="ci", colourPalette = colourPalette, mapTitle="", addLegend=FALSE, catMethod=seq(min(o3$ci), max(o3$ci), length=10))

do.call(addMapLegend, c(mapParams, legendLabels="all", legendWidth=0.5, legendIntervals="data", legendMar = 2))
dev.off()


spdf <- joinCountryData2Map(o3 %>% filter(year == 2023), joinCode="NAME", nameJoinColumn="location_name")
mapDevice('pdf', file="figures/map2023.pdf")
mapParams <- mapCountryData(spdf, nameColumnToPlot="ci", colourPalette = colourPalette, mapTitle="", addLegend=FALSE, catMethod=seq(min(o3$ci), max(o3$ci), length=10))

do.call(addMapLegend, c(mapParams, legendLabels="all", legendWidth=0.5, legendIntervals="data", legendMar = 2))
dev.off()


## Lorenz curves

temp2 <- bind_rows(
  temp2, 
  group_by(temp2, sex_name, year, cause_name) %>%
  summarise(location_name = "Global",
            val = sum(val, na.rm=TRUE),
            total_attention_score = first(total_attention_score))
)



# Gini index
gini <- ci(
  ineqvar = subset(temp2, location_name == "Global")$total_attention_score,
  outcome = subset(temp2, location_name == "Global")$total_attention_score, method = "direct"
)
gini
sdi_low <- subset(temp2, location_name == "Low SDI" & sex_name=="Both" & year==2023)
ci_low <- ci(
  ineqvar = sdi_low$total_attention_score,
  outcome = sdi_low$val, method = "direct"
)

sdi_high <- subset(temp2, location_name == "High SDI" & sex_name=="Both" & year==2023)
ci_high <- ci(
  ineqvar = sdi_high$total_attention_score,
  outcome = sdi_high$val, method = "direct"
)

global <- subset(temp2, location_name == "Global" & sex_name=="Both" & year==2023)
ci_global <- ci(
  ineqvar = global$total_attention_score,
  outcome = global$val, method = "direct"
)

make_plot_dat <- function(x) {
  myOrder <- order(x$fractional_rank)
  xCoord <- x$fractional_rank[myOrder]
  y <- x$outcome[myOrder]
  cumdist <- cumsum(y) / sum(y)
  tibble(xCoord, cumdist)
}

dat <- bind_rows(
  make_plot_dat(gini) %>% mutate(group = "GWAS attention (Gini index)"),
  make_plot_dat(ci_global) %>% mutate(group = "DALY burden, Global"),
  make_plot_dat(ci_low) %>% mutate(group = "DALY burden, Low SDI"),
  make_plot_dat(ci_high) %>% mutate(group = "DALY burden, High SDI")
)

ggplot(aes(x = xCoord, y = cumdist, group = group), data = dat) +
  geom_line(aes(colour = group)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  theme_bw() +
  theme(legend.position = "inside", legend.position.inside=c(0.2,0.8)) +
  labs(
    x = "Fractional rank of GWAS attention score",
    y = "Cumulative proportion of outcome",
    colour = "Outcome"
  ) +
  scale_colour_manual(values = c(
    "#a6cee3",
    "#1f78b4",
    "#b2df8a",
    "#33a02c"
  ))
ggsave(here("figures/lorenz_curve.pdf"), width = 6, height = 6)


# gbd4 <- fread(here("Data/april2025/by_sdi_and_age/IHME-GBD_2021_DATA-2c936676-1.csv"))

gbd4 <- fread(here("Data/december2025/gbd_gwas_paper_data_1.csv")) %>%
  rename(sex_name = sex, year = year_id, age_name = age_group_name, age_id = age_group_id)
table(gbd4$age_name) %>% as.data.frame
table(gbd4$age_id) %>% as.data.frame
temp4 <- inner_join(gbd4, gwas_attention)
temp4$age_group <- gsub(" years", "", temp4$age_name)
temp4$age_group <- gsub(" year", "", temp4$age_group)
temp4$age_group <- gsub(" to ", "-", temp4$age_group)
temp4$age_group <- factor(temp4$age_group, levels = c("<1", "2-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-94"))

table(gbd4$age_id, gbd4$location_id, gbd4$year)

temp4 <- filter(temp4, grepl("SDI", location_name))
temp4 <- bind_rows(
  temp4,
  group_by(temp4, cause_name, sex_name, age_name, age_group, age_id, year) %>%
    summarise(location_name = "Global", nloc = n(),
              val = sum(val, na.rm=TRUE),
              total_attention_score = first(total_attention_score))
)
table(temp4$nloc)
o4 <- temp4 %>%
  # filter(age_group != "<1") %>%
  group_by(location_name, age_name, age_group, age_id, year) %>%
  do(get_ci(.)) %>%
  ungroup() %>%
  group_by(location_name, year) %>%
  mutate(daly_prop = daly_sum / sum(daly_sum, na.rm=TRUE))
o4$location_name <- factor(o4$location_name, levels = c(
  "High SDI", "High-middle SDI", "Middle SDI",
  "Low-middle SDI", "Low SDI", "Global"
))

o4 %>%
  dplyr::filter(year == 2023) %>%
  ggplot(., aes(y = ci, x = age_group)) +
    geom_errorbar(colour="grey", aes(
      ymin = ci_lci,
      ymax = ci_uci),
    width = 0) +
    geom_hline(yintercept = 0, linetype = "dashed") + 
    facet_grid(. ~ location_name) +
    geom_point(aes(size=daly_prop)) +
    theme_bw() +
    theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=1, size=6), legend.position = "inside", legend.position.inside=c(0.08,0.2)) +
    labs(x="Age group", y="Concentration index", size="DALY proportion") +
    ylim(-0.7, 0.7)
ggsave(here("figures/ci_by_age.pdf"), width = 10, height = 4)


# Which traits have the biggest change in DALY by age group?
reg <- group_by(temp4, cause_name, year, location_name) %>%
  do({
    tryCatch({
      .$val <- scale(.$val)[, 1]
      a <- summary(lm(val ~ as.numeric(age_group), data = .))
      b <- a$coefficients[2, 1]
      c <- a$coefficients[2, 4]
      d <- a$coefficients[2, 2]
      e <- a$coefficients[2, 3]
      tibble(
        slope = b,
        pval = c,
        se = d,
        tval = e
      )
    },
    error = function(e) {
      message("Error in regression for cause: ", unique(.$cause_name))
      tibble(
        slope = NA,
        pval = NA,
        se = NA,
        tval = NA
      )
    })
})

reg$pval_adj <- p.adjust(reg$pval, method = "bonferroni")
table(reg$pval_adj < 0.05)

reg %>% arrange(pval_adj)


reg %>% arrange(slope) %>% filter(year == 2021, pval_adj < 0.05) %>% select(cause_name, slope, location_name) %>% as.data.frame


gwas_attention2 <- subset(gwas_attention, cause_name %in% temp1$cause_name)
table(gwas_attention2$total_attention_score > 0)
