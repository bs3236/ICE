# Description -------------------------------------------------------------

## ZCTA Level Index of Concentrations at the Extremes (ICE):
#
# 2007-11 US Census Data
#
# Combine county_state_race and county_state ICE datasets
#
# Combine cbsa_race and cbsa ICE datasets

# Load Packages ----------------------------------------------------------

library(tidyverse)
library(dplyr)
library(here)

# Load Data ---------------------------------------------------------------
#County ICE race data 2007-11
load(here("data",
          "processed",
          "county_state_race_ice_07_11.rdata"))

#County ICE race independent data 2007-11
load(here("data",
          "processed",
          "county_state_ice_income_07_11.rdata"))

#CBSA ICE race data 2007-11
load(here("data",
          "processed",
          "cbsa_race_ice_07_11.rdata"))

#CBSA ICE race independent data 2007-11
load(here("data",
          "processed",
          "cbsa_ice_income_07_11.rdata"))


# Combine County ICE Datasets ---------------------------------------------

county_state_ice_07_11 <- county_state_07_11 %>%
  select(GISJOIN_county, poor_income_static, rich_income_static, static_ice_income, poor, rich, ice_income) %>%
  left_join(county_state_race_07_11, county_state_07_11, by = "GISJOIN_county") %>%
  select(
    STATE,
    STATEA,
    COUNTY,
    COUNTYA,
    GISJOIN_county,
    YEAR,
    tot_hh,
    p20_hh_state,
    p80_hh_state,
    p20_cutoff_state,
    p80_cutoff_state,
    p20_static,
    p80_static,
    threshold_bin_p20_state,
    threshold_bin_p80_state,
    poor_income_static,
    rich_income_static,
    static_ice_income,
    poor,
    rich,
    ice_income,
    poor_black_static,
    rich_white_static,
    static_ice_race,
    poor_black,
    rich_white,
    ice_race
  )


# Combine CBSA ICE Datasets -----------------------------------------------

cbsa_ice_07_11 <- cbsa_07_11 %>%
  select(GISJOIN_CBSA, poor_income_static, rich_income_static, static_ice_income, poor, rich, ice_income) %>%
  left_join(cbsa_race_07_11, cbsa_07_11, by = "GISJOIN_CBSA") %>%
  select(
    CBSA,
    GISJOIN_CBSA,
    YEAR,
    tot_hh,
    p20_hh_cbsa,
    p80_hh_cbsa,
    p20_cutoff_cbsa,
    p80_cutoff_cbsa,
    p20_static,
    p80_static,
    threshold_bin_p20_cbsa,
    threshold_bin_p80_cbsa,
    poor_income_static,
    rich_income_static,
    static_ice_income,
    poor,
    rich,
    ice_income,
    poor_black_static,
    rich_white_static,
    static_ice_race,
    poor_black,
    rich_white,
    ice_race
  )


# Save Data ---------------------------------------------------------------

save(county_state_ice_07_11, file = "data/processed/county_state_ice_07_11.rdata")
save(cbsa_ice_07_11, file = "data/processed/cbsa_ice_07_11.rdata")
