# Open discount-rate.Rproj before running this script.

# This program creates and updates the discount_rates table with the latest
# available innformation.
# Source information:
# https://data.worldbank.org/indicator/FP.CPI.TOTL.ZG
# https://data.worldbank.org/indicator/NY.GDP.MKTP.KD

# Package dependencies:
library(wbstats)
library(dplyr)
library(readr)

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
  ) %>%
  tidyr::spread(to, conversion_factor)

d2 <- d2 %>%
  select(-from) %>%
  as.matrix()

rownames(d2) <- 1962:2017

for (i in 1:nrow(d2)) {
  for (j in 1:ncol(d2)) {
    if (i <= j) {
      break()
    } else {
      if (i == j + 1) {
        d2[i,j] <- 1
      } else {
        d2[i,j] <- d2[j,i-1]
      }
    }
  }
}

d3 <- d2 %>%
  as.data.frame() %>%
  tibble::rownames_to_column() %>%
  dplyr::rename(from = rowname) %>%
  tidyr::gather(to, conversion_factor, -from)

readr::write_csv(d3, "discount-rates.csv")
