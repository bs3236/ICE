# Description -------------------------------------------------------------

## ZCTA Level Index of Concentrations at the Extremes (ICE):
#
# 2007-11 US Census Data
#
# County Level Household Income in Past 12 Months Data Cleaning
#   Black only and White only Households


# Load Packages ----------------------------------------------------------

library(tidyverse)
library(dplyr)
library(here)

# Load Data ---------------------------------------------------------------

#County level HH income data, Black only and White only households 2007-11
county_race_07_11 <- read_csv(here(
  "data",
  "raw",
  "nhgis0046_csv",
  "nhgis0046_ds185_20115_county.csv"
))


# Data Cleaning -----------------------------------------------------------

#HH income in past 12 months - White alone households: M5QE
#HH income in past 12 months - Black or African American alone households: M5RE

#select and rename variables
county_race_07_11 <- county_race_07_11 %>%
  select(
    GISJOIN,
    YEAR,
    STATE,
    COUNTY,
    COUNTYA,
    M5RE001,
    M5RE002,
    M5RE003,
    M5RE004,
    M5RE005,
    M5RE006,
    M5RE007,
    M5RE008,
    M5RE009,
    M5RE010,
    M5RE011,
    M5RE012,
    M5RE013,
    M5RE014,
    M5RE015,
    M5RE016,
    M5RE017,
    M5QE001,
    M5QE002,
    M5QE003,
    M5QE004,
    M5QE005,
    M5QE006,
    M5QE007,
    M5QE008,
    M5QE009,
    M5QE010,
    M5QE011,
    M5QE012,
    M5QE013,
    M5QE014,
    M5QE015,
    M5QE016,
    M5QE017
  ) %>%
  rename(
    GISJOIN_county = GISJOIN,
    b_tot_hh = M5RE001,
    b_inc_less_10k = M5RE002,
    b_inc_10k_15k = M5RE003,
    b_inc_15k_20k = M5RE004,
    b_inc_20k_25k = M5RE005,
    b_inc_25k_30k = M5RE006,
    b_inc_30k_35k = M5RE007,
    b_inc_35k_40k = M5RE008,
    b_inc_40k_45k = M5RE009,
    b_inc_45k_50k = M5RE010,
    b_inc_50k_60k = M5RE011,
    b_inc_60k_75k = M5RE012,
    b_inc_75k_100k = M5RE013,
    b_inc_100k_125k = M5RE014,
    b_inc_125k_150k = M5RE015,
    b_inc_150k_200k = M5RE016,
    b_inc_200k_more = M5RE017,
    w_tot_hh = M5QE001,
    w_inc_less_10k = M5QE002,
    w_inc_10k_15k = M5QE003,
    w_inc_15k_20k = M5QE004,
    w_inc_20k_25k = M5QE005,
    w_inc_25k_30k = M5QE006,
    w_inc_30k_35k = M5QE007,
    w_inc_35k_40k = M5QE008,
    w_inc_40k_45k = M5QE009,
    w_inc_45k_50k = M5QE010,
    w_inc_50k_60k = M5QE011,
    w_inc_60k_75k = M5QE012,
    w_inc_75k_100k = M5QE013,
    w_inc_100k_125k = M5QE014,
    w_inc_125k_150k = M5QE015,
    w_inc_150k_200k = M5QE016,
    w_inc_200k_more = M5QE017
  )

# Save Cleaned Data -------------------------------------------------------

save(county_race_07_11, file = "data/processed/county_race_hh_income_07_11.rdata")
