# Description -------------------------------------------------------------

## ZCTA Level Index of Concentrations at the Extremes (ICE):
#
# 2007-11 US Census Data
#
## Calculate ICE using County Specific Income Thresholds
#   CBSA, race independent

# Load Packages ----------------------------------------------------------

library(tidyverse)
library(dplyr)
library(here)

# Load Data ---------------------------------------------------------------
#Cleaned CBSA HH income data race independent 2007-11
load(here("data",
          "processed",
          "cbsa_static_ice_07_11.rdata"))


# Pivot CBSA HH Income Dataset to Sum Across Rows -----------------------

#select variables
#pivot income data
cbsa_pivot_07_11 <- cbsa_07_11 %>%
  select(
    GISJOIN_CBSA,
    tot_hh,
    threshold_bin_p20_cbsa,
    threshold_bin_p80_cbsa,
    inc_less_10k:inc_200k_more
  ) %>%
  pivot_longer(
    cols = starts_with("inc_"),
    names_to = "income",
    names_prefix = "",
    values_to = "count",
    values_drop_na = TRUE
  )

# Create Pivoted HH Income Bins -------------------------------------------

cbsa_pivot_07_11 <- cbsa_pivot_07_11 %>%
  mutate(
    income_bin = case_when(
      income == "inc_less_10k" ~ 1,
      income == "inc_10k_15k" ~ 2,
      income == "inc_15k_20k" ~ 3,
      income == "inc_20k_25k" ~ 4,
      income == "inc_25k_30k" ~ 5,
      income == "inc_30k_35k" ~ 6,
      income == "inc_35k_40k" ~ 7,
      income == "inc_40k_45k" ~ 8,
      income == "inc_45k_50k" ~ 9,
      income == "inc_50k_60k" ~ 10,
      income == "inc_60k_75k" ~ 11,
      income == "inc_75k_100k" ~ 12,
      income == "inc_100k_125k" ~ 13,
      income == "inc_125k_150k" ~ 14,
      income == "inc_150k_200k" ~ 15,
      income == "inc_200k_more" ~ 16,
      TRUE ~ NA_real_
    )
  )


# Create Rich Variable ----------------------------------------------------

#all HH Above the Overall CBSA HH 80th percentile for Income
#and 50% of HH Where Income = 80th percentile

cbsa_pivot_rich_07_11 <- cbsa_pivot_07_11 %>%
  mutate(count = as.double(as.character(count))) %>%
  mutate(
    sum_temp = case_when(
      income_bin > threshold_bin_p80_cbsa ~ count,
      income_bin == threshold_bin_p80_cbsa ~ count *
        0.5,
      TRUE ~ NA_real_
    )
  )
cbsa_pivot_rich_07_11 <- cbsa_pivot_rich_07_11 %>%
  group_by(GISJOIN_CBSA) %>%
  summarise(rich = sum(sum_temp, na.rm = TRUE)) %>%
  group_by()

#join rich variable to cbsa dataset
cbsa_07_11 <-
  left_join(cbsa_07_11, cbsa_pivot_rich_07_11, by = c("GISJOIN_CBSA"))


# Create Poor Variable ----------------------------------------------------

#all HH below the Overall CBSA HH 20th percentile for Income
#and 50% of HH Where Income = 20th percentile

cbsa_pivot_poor_07_11 <- cbsa_pivot_07_11 %>%
  mutate(count = as.double(as.character(count))) %>%
  mutate(
    sum_temp = case_when(
      income_bin < threshold_bin_p20_cbsa ~ count,
      income_bin == threshold_bin_p20_cbsa ~ count *
        0.5,
      TRUE ~ NA_real_
    )
  )

cbsa_pivot_poor_07_11 <- cbsa_pivot_poor_07_11 %>%
  group_by(GISJOIN_CBSA) %>%
  summarise(poor = sum(sum_temp, na.rm = TRUE)) %>%
  group_by()
#join poor variable to cbsa dataset
cbsa_07_11 <-
  left_join(cbsa_07_11, cbsa_pivot_poor_07_11, by = c("GISJOIN_CBSA"))


# Calculate ICE using CBSA-Specific Thresholds, Race Independent -------

cbsa_07_11 <- cbsa_07_11 %>%
  mutate(ice_income = (rich - poor) / tot_hh)



# Save Data ---------------------------------------------------------------

save(cbsa_07_11, file = "data/processed/cbsa_ice_income_07_11.rdata")
