# Open inflation-data.Rproj before running this script.

# This program creates and updates the discount_rates table with the latest
# available innformation.
# Source information:
# https://data.worldbank.org/indicator/FP.CPI.TOTL.ZG
# https://data.worldbank.org/indicator/NY.GDP.MKTP.KD

# Package dependencies:
library(wbstats)
library(dplyr)
library(readr)
library(jsonlite)

inflation <- wbstats::wb(indicator = "FP.CPI.TOTL.ZG")

gdp_constant_2010_usd <- wbstats::wb(indicator = "NY.GDP.MKTP.KD")

d <- gdp_constant_2010_usd %>%
  select(iso3c, date, value) %>%
  rename(gdp = value) %>%
  inner_join(
    inflation %>%
      select(iso3c, date, value) %>%
      rename(inflation = value) %>%
      mutate(inflation = inflation / 100)
  ) %>%
  rename(
    country_iso = iso3c,
    year = date
  ) %>%
  mutate(
    country_iso = tolower(country_iso),
    country_iso = ifelse(country_iso == "rou", "rom", country_iso)
  ) %>%
  filter(year >= 1962) %>%
  group_by(year) %>%
  summarise(
    conversion_factor = weighted.mean(inflation, gdp) + 1
  )

d2 <- d %>%
  select(year) %>%
  rename(from = year) %>%
  mutate(to = from) %>%
  expand.grid(stringsAsFactors = FALSE) %>%
  filter(from < to) %>%
  arrange(from) %>%
  left_join(
    d %>%
      select(year, conversion_factor),
    by = c("to" = "year")
  )

readr::write_csv(d2, "inflation-data.csv")
jsonlite::write_json(d2, "inflation-data.json")
