# Open discount-rate.Rproj before running this script.

# This program creates and updates the discount_rates table with the latest
# available innformation.
# Source information: https://data.worldbank.org/indicator/FP.CPI.TOTL.ZG

# Package dependencies:
library(wbstats)
library(dplyr)

# Create a function to download the raw data and to generate the discount rate 
# table.
generate_discount_rates <- function(save = TRUE) {
  df <- wbstats::wb(indicator = "FP.CPI.TOTL.ZG",
                    country = "US")
  discount_rates <- df %>% 
              dplyr::arrange(date) %>% 
              dplyr::mutate(from = date,
                            to = as.integer(date) + 1,
                            rate = round(cumprod(1 + value / 100),  2)) %>% 
              dplyr::select(from, to, rate) 
  
  if (save == TRUE) {
    write.csv(discount_rates, "./output/discount_rates.csv", row.names = FALSE)
  } else {
    discount_rates
  }
}


# Query the last year of the discount_rates table.
last_date <- function() {
  df <- read.csv("./output/discount_rates.csv")
  max(df$to)
}


# Verify the current state of discount_rates table.
FILE_DEST <- "./output/discount_rates.csv"

if (!file.exists(FILE_DEST)) {
  
  message("Downloading and creating discount_rates table from World Bank repository.
          \nCheck source website for more information about the raw data: 
          https://data.worldbank.org/indicator/FP.CPI.TOTL.ZG")
  
  generate_discount_rates()
  
} else {
  
  message("Updating discount_rates table with the latest information available...")
  LAST_YEAR <- last_date()
  df <- generate_discount_rates(save = FALSE)

  if (any(df$to > LAST_YEAR)) {
    
    # append latest information to the discount_rates table.
    df %>% 
      dplyr::filter(to > LAST_YEAR) %>% 
      write.table("./output/discount_rates.csv", sep = ",", 
                 col.names = !file.exists("./output/discount_rates.csv"), 
                  append = TRUE, row.names = FALSE)
    
    # report number of new observations added into discount_rates table.
    NUM_OBS_ADDED <- nrow(dplyr::filter(df, to > LAST_YEAR))
    message(NUM_OBS_ADDED, " new ", 
            if (NUM_OBS_ADDED == 1) "observation was " else "observations were ",
            "added to discount_rate table.")
    
    
  } else {
    message("No new information available to update discount_rates table.")
  }
}