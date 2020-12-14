# Load packages.

library(readr)
library(janitor)
library(shiny)
library(tidyverse)
library(maptools)
library(rgdal)

# Read in datasets.

climaterisk <- read_xls("raw_data/vulnerability.xls", skip = 2) %>%
    clean_names() %>%
    rename(vulnerable = climate_vulnerability_cv_cdi_adj_for_income_regulation) %>%
    select(country, vulnerable, world_sub_region)

countries <- read_xls("raw_data/countries_files/icowcol.xls") %>%
    select(Name, Indep) %>%
    mutate(twenty = ifelse(Indep >= 190000, "Colonized", "Independent"))

# Join data to "fit_mod." For each country I have whether they were colonized in the 20th
# century, date of their independence, world region, climate risk, and average
# risk grouped by colonized vs. independent.

fit_mod <- inner_join(countries, climaterisk, by = c("Name" = "country")) %>%
    group_by(twenty) %>%
    mutate(avg_risk = mean(vulnerable)) 

# Modifying Polygons: world countries from the maptools package.
# More info: https://rdrr.io/cran/maptools/man/wrld_simpl.html
# Merging with my data on climate risk.

data(wrld_simpl)

wrld_simpl@data <-
    inner_join(wrld_simpl@data, fit_mod, by = c("NAME" = "Name")) 

saveRDS(wrld_simpl, "joined.rds")



