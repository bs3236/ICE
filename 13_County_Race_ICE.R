# Description -------------------------------------------------------------

## ZCTA Level Index of Concentrations at the Extremes (ICE):
#
# 2007-11 US Census Data
#
## Calculate ICE using County Specific Income Thresholds
#   County, Black Households Only and White Households Only

# Load Packages ----------------------------------------------------------

library(tidyverse)
library(dplyr)
library(here)

# Load Data ---------------------------------------------------------------
#Cleaned county HH income data with race 2007-11
load(here(
  "data",
  "processed",
  "county_state_race_static_ice_07_11.rdata"
))


# Pivot Black County HH Income with Race Dataset to Sum Across Rows -------

county_b_inc_07_11 <- county_state_race_07_11 %>%
  select(GISJOIN_county,
         tot_hh,
         threshold_bin_p20_state,
         b_inc_less_10k:b_inc_200k_more) %>%
  pivot_longer(
    cols = starts_with("b_inc"),
    names_to = "b_income",
    names_prefix = "inc",
    values_to = "count",
    values_drop_na = TRUE
  )


# Code Black Household Income Buckets in Ascending Order ------------------

county_b_inc_07_11 <- county_b_inc_07_11 %>%
  mutate(
    b_income_bin = case_when(
      b_income == "b_inc_less_10k" ~ 1,
      b_income == "b_inc_10k_15k" ~ 2,
      b_income == "b_inc_15k_20k" ~ 3,
      b_income == "b_inc_20k_25k" ~ 4,
      b_income == "b_inc_25k_30k" ~ 5,
      b_income == "b_inc_30k_35k" ~ 6,
      b_income == "b_inc_35k_40k" ~ 7,
      b_income == "b_inc_40k_45k" ~ 8,
      b_income == "b_inc_45k_50k" ~ 9,
      b_income == "b_inc_50k_60k" ~ 10,
      b_income == "b_inc_60k_75k" ~ 11,
      b_income == "b_inc_75k_100k" ~ 12,
      b_income == "b_inc_100k_125k" ~ 13,
      b_income == "b_inc_125k_150k" ~ 14,
      b_income == "b_inc_150k_200k" ~ 15,
      b_income == "b_inc_200k_more" ~ 16,
      TRUE ~ NA_real_
    )
  )


# Create poor_black Variable Using case_when ------------------------------

#all Black HH below the Overall County HH 20th percentile for Income
#and 50% of HH Where Income = 20th percentile

county_poor_black_07_11 <- county_b_inc_07_11 %>%
  mutate(count = as.double(as.character(count))) %>%
  mutate(
    sum_temp = case_when(
      b_income_bin < threshold_bin_p20_state ~ count,
      b_income_bin == threshold_bin_p20_state ~ count *
        0.5,
      TRUE ~ NA_real_
    )
  )
county_poor_black_07_11 <- county_poor_black_07_11 %>%
  group_by(GISJOIN_county) %>%
  summarise(poor_black = sum(sum_temp, na.rm = TRUE))

#join poor_black variable to race dataset
county_state_race_07_11 <- county_poor_black_07_11 %>%
  left_join(county_state_race_07_11,
            county_poor_black_07_11,
            by = c("GISJOIN_county"))

# Pivot White County HH Income with Race Dataset to Sum Across Rows -------

county_w_inc_07_11 <- county_state_race_07_11 %>%
  select(GISJOIN_county,
         tot_hh,
         threshold_bin_p80_state ,
         w_inc_less_10k:w_inc_200k_more) %>%
  pivot_longer(
    cols = starts_with("w_inc"),
    names_to = "w_income",
    names_prefix = "inc",
    values_to = "count",
    values_drop_na = TRUE
  )

# Code White Household Income Buckets in Ascending Order ------------------

county_w_inc_07_11 <- county_w_inc_07_11 %>%
  mutate(
    w_income_bin = case_when(
      w_income == "w_inc_less_10k" ~ 1,
      w_income == "w_inc_10k_15k" ~ 2,
      w_income == "w_inc_15k_20k" ~ 3,
      w_income == "w_inc_20k_25k" ~ 4,
      w_income == "w_inc_25k_30k" ~ 5,
      w_income == "w_inc_30k_35k" ~ 6,
      w_income == "w_inc_35k_40k" ~ 7,
      w_income == "w_inc_40k_45k" ~ 8,
      w_income == "w_inc_45k_50k" ~ 9,
      w_income == "w_inc_50k_60k" ~ 10,
      w_income == "w_inc_60k_75k" ~ 11,
      w_income == "w_inc_75k_100k" ~ 12,
      w_income == "w_inc_100k_125k" ~ 13,
      w_income == "w_inc_125k_150k" ~ 14,
      w_income == "w_inc_150k_200k" ~ 15,
      w_income == "w_inc_200k_more" ~ 16,
      TRUE ~ NA_real_
    )
  )


# Create rich_white variable using case_when ------------------------------

#all White HH Above the Overall County/CBSA HH 80th percentile for Income
#and 50% of HH Where Income = 80th percentile

county_rich_white_07_11 <- county_w_inc_07_11 %>%
  mutate(count = as.double(as.character(count))) %>%
  mutate(
    sum_temp = case_when(
      w_income_bin > threshold_bin_p80_state ~ count,
      w_income_bin == threshold_bin_p80_state ~ count *
        0.5,
      TRUE ~ NA_real_
    )
  )
county_rich_white_07_11 <- county_rich_white_07_11 %>%
  group_by(GISJOIN_county) %>%
  summarise(rich_white = sum(sum_temp, na.rm = TRUE))

#join rich_white variable to race dataset
county_state_race_07_11 <- county_rich_white_07_11 %>%
  left_join(county_state_race_07_11,
            county_rich_white_07_11,
            by = c("GISJOIN_county"))

# Calculate ICE with Race using County-Specific Thresholds ----------------

county_state_race_07_11 <- county_state_race_07_11 %>%
  group_by(GISJOIN_county) %>%
  mutate(ice_race = (rich_white - poor_black) / tot_hh) %>%
  ungroup()
summary(county_state_race_07_11$ice_race)

# Save Data ---------------------------------------------------------------

save(county_state_race_07_11, file = "data/processed/county_state_race_ice_07_11.rdata")
