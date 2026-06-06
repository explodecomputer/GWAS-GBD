library(here)
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggstance)
library(ggrepel)
library(rineq)
library(RColorBrewer)
library(cowplot)
library(rworldmap)

sdi_levels <- c(
  "High SDI",
  "High-middle SDI",
  "Middle SDI",
  "Low-middle SDI",
  "Low SDI",
  "Global"
)

sdi_locations <- setdiff(sdi_levels, "Global")

theme_report <- function() {
  theme_bw() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
    )
}
