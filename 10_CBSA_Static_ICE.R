# Description -------------------------------------------------------------

## ZCTA Level Index of Concentrations at the Extremes (ICE):
#
# 2007-11 US Census Data
#
## Calculate ICE using static nationwide HH income cutoffs
#   CBSA, race independent
#   CBSA by race

# Load Packages ----------------------------------------------------------

library(tidyverse)
library(dplyr)
library(here)


# Load Data ---------------------------------------------------------------

#Cleaned CBSA HH income data by race 2007-11
load(here("data",
          "processed",
          "cbsa_race_hh_income_07_11.rdata"))

#Cleaned CBSA HH income data by race 2007-11
load(here("data",
          "processed",
          "cbsa_hh_income_07_11.rdata"))


# Add 2007-11 State Income Cutoffs from 2009 Data -------------------------

#Use https://www2.census.gov/library/publications/2010/demo/p60-238/p60-238.pdf

#CBSA HH income race independent
cbsa_07_11 <- cbsa_07_11 %>%
  mutate(p20_static = 20453, p80_static = 100000)

#CBSA HH income by race
cbsa_race_07_11 <- cbsa_race_07_11 %>%
  mutate(p20_static = 20453, p80_static = 100000)



# Calculate Static ICE,  Race Independent ---------------------------------

cbsa_07_11 <- cbsa_07_11 %>%
  mutate(
    poor_income_static = (inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k *
                            (0.5)),
    #poor_income_static = All households <20th percentile of $20453
    rich_income_static = (
      inc_100k_125k * (0.5) + inc_125k_150k + inc_150k_200k + inc_200k_more
    )
  ) %>%
  #rich_income_static = All households >80th percentile of $100000
  mutate(static_ice_income = (rich_income_static - poor_income_static) /
           tot_hh)
summary(cbsa_07_11$static_ice_income)



# Calculate Static ICE by Race --------------------------------------------

#Create poor_black_static variable (all Black households <20th percentile income)
#Create rich_white_static variable (all NH-White households >80th percentile income)
#Create static_ice_race (all NH-White households >80th percentile income - all Black households <20th percentile income)/Total Households)

cbsa_race_07_11 <- cbsa_race_07_11 %>%
  mutate(
    poor_black_static = (
      b_inc_less_10k + b_inc_10k_15k + b_inc_15k_20k + b_inc_20k_25k * (0.5)
    ),
    #poor_black_static = Black households <20th percentile of $20453
    rich_white_static = (
      w_inc_100k_125k * (0.5) + w_inc_125k_150k + w_inc_150k_200k + w_inc_200k_more
    )
  ) %>%
  #rich_white_static = White households >80th percentile of $100000
  mutate(static_ice_race = (rich_white_static - poor_black_static) / tot_hh)
summary(cbsa_race_07_11$static_ice_race)

# Save Data ---------------------------------------------------------------

save(cbsa_race_07_11, file = "data/processed/cbsa_race_static_ice_07_11.rdata")
save(cbsa_07_11, file = "data/processed/cbsa_static_ice_07_11.rdata")
