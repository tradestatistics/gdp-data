# Open inflation-data.Rproj before running this script.

# This program creates and updates the discount_rates table with the latest
# available innformation.
# Source information:
# https://data.worldbank.org/indicator/FP.CPI.TOTL.ZG
# https://data.worldbank.org/indicator/NY.GDP.MKTP.KD

# Package dependencies:
library(wbstats)
library(dplyr)
library(tidyr)
library(matrixStats)
library(readr)
library(jsonlite)

inflation <- wbstats::wb_data(indicator = "FP.CPI.TOTL.ZG")

gdp_constant_2010_usd <- wbstats::wb_data(indicator = "NY.GDP.MKTP.KD")

d <- gdp_constant_2010_usd %>%
  select(iso3c, date, gdp = NY.GDP.MKTP.KD) %>%
  inner_join(
    inflation %>%
      select(iso3c, date, inflation = FP.CPI.TOTL.ZG) %>%
      mutate(inflation = inflation / 100)
  ) %>%
  rename(
    country_iso = iso3c,
    to = date
  ) %>%
  mutate(
    country_iso = tolower(country_iso),
    country_iso = ifelse(country_iso == "rou", "rom", country_iso)
  ) %>%
  drop_na() %>% 
  filter(to >= 1962) %>%
  group_by(to) %>%
  summarise(
    conversion_factor = weightedMedian(inflation, gdp) + 1
  ) %>% 
  mutate(
    to = as.integer(to),
    from = to - 1
  ) %>% 
  select(from, to, conversion_factor)

readr::write_csv(d, "inflation-data.csv")
jsonlite::write_json(d, "inflation-data.json")
