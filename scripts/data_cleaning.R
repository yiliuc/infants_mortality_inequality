# Import the raw data of income
income16 <- read.csv("inputs/data/ACS_DP03_income_county_2016.csv")

# Clean the income data (From ACS DP05)
library(dplyr)
library(stringr)
library(tidyverse)
library(janitor)

# Convert full state name to its abbreviation.
get_state_abbreviation <- function(state_name) {
  state_data <- state.abb[match(state_name, state.name)]
  if (!is.na(state_data)) {
    return(state_data)
  } else {
    return(NULL)
  }
}

# Extract the county name from a 'County, State' formatted string.
extract_county <- function(name) {
  if (!is.na(name) && str_detect(name, ",")) {
    return(str_replace(str_split(name, ",")[[1]][1], " County", ""))
  } else {
    return(NULL)
  }
}

# Extract the state abbreviation from a 'County, State' formatted string.
extract_state <- function(name) {
  if (!is.na(name) && str_detect(name, ",")) {
    return(get_state_abbreviation(str_trim(str_split(name, ",")[[1]][2])))
  } else {
    return(NULL)
  }
}

# Clean and rename columns in a DataFrame.
data_cleaning <- function(df, rename_dict) {
  df <- df[-1, ]  # Remove the first row
  df$county <- sapply(df$NAME, extract_county)
  df$state <- sapply(df$NAME, extract_state)
  columns_to_select <- c("county", "state", names(rename_dict))
  selected_df <- df[, columns_to_select, drop = FALSE]
  colnames(selected_df) <- c("county", "state", rename_dict)
  return(selected_df %>% arrange(state, county))
}

in16dict <- c("DP03_0063E" = "mean_household_income")

income16_clean <- data_cleaning(income16, in16dict)
null_indices <- which(sapply(income16_clean$state, is.null))
income16_clean <- income16_clean[-null_indices, ]
income16_clean$state <- unlist(income16_clean$state)
income <- income16_clean # income is now the cleaned data set


# Import the raw data of infants mortality
infants <- read.csv("inputs/data/CDC_infants_mortality_1999_to_2016.csv")

# Clean the infants mortality data
# Re-formatting the "county" and "state" in the infants data
split <- strsplit(infants$County, ",")
county_county <- sapply(split, `[`, 1)
state <- sapply(split, `[`, 2)
split2 <- strsplit(county_county, " ")
county <- sapply(split2, `[`, 1)
infants$county <- county
infants$state <- trimws(state)
# Select only the White and Black races and sellect the certain columns
infants <- infants %>% 
  filter(Race == "Black or African American" | Race == "White") %>% 
  select(county, state, Race, Deaths, Population)
# Recalculate the crude rate
infants$crude_rate <- (infants$Deaths/infants$Population)*100000
# Find the percentile of household income of each county
income <- income %>%
  arrange(mean_household_income) %>%
  mutate(pctile = ntile(mean_household_income, 100))


# Merge the two data sets
# This is the data for the this paper
data <- merge(income, infants, by = c("county", "state")) %>% 
  clean_names()

# You can also export the merged data
write.csv(data, "outputs/data/data.csv", row.names = FALSE)