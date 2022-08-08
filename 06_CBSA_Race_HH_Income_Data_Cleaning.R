# Description -------------------------------------------------------------

## ZCTA Level Index of Concentrations at the Extremes (ICE):
#
# 2007-11 US Census Data
#
# CBSA Level Household Income in Past 12 Months Data Cleaning
#   Black only and White only Households

# Load Packages ----------------------------------------------------------

library(tidyverse)
library(dplyr)
library(here)


# Load Data ---------------------------------------------------------------

#CBSA level HH income data 2007-11, Black only and White only households
cbsa_race_07_11 <- read_csv(here(
  "data",
  "raw",
  "nhgis0026_csv",
  "nhgis0026_ds185_20115_2011_cbsa.csv"
))


# Data Cleaning -----------------------------------------------------------


#HH income in past 12 months - White alone households: M5XE
#HH income in past 12 months - Black or African American alone households: M5RE
#select and rename variables

cbsa_race_07_11 <- cbsa_race_07_11 %>%
  select(
    GISJOIN,
    YEAR,
    CBSA,
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
    M5XE001,
    M5XE002,
    M5XE003,
    M5XE004,
    M5XE005,
    M5XE006,
    M5XE007,
    M5XE008,
    M5XE009,
    M5XE010,
    M5XE011,
    M5XE012,
    M5XE013,
    M5XE014,
    M5XE015,
    M5XE016,
    M5XE017
  ) %>%
  rename(
    GISJOIN_CBSA = GISJOIN,
    b_inc_total = M5RE001,
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
    w_inc_total = M5XE001,
    w_inc_less_10k = M5XE002,
    w_inc_10k_15k = M5XE003,
    w_inc_15k_20k = M5XE004,
    w_inc_20k_25k = M5XE005,
    w_inc_25k_30k = M5XE006,
    w_inc_30k_35k = M5XE007,
    w_inc_35k_40k = M5XE008,
    w_inc_40k_45k = M5XE009,
    w_inc_45k_50k = M5XE010,
    w_inc_50k_60k = M5XE011,
    w_inc_60k_75k = M5XE012,
    w_inc_75k_100k = M5XE013,
    w_inc_100k_125k = M5XE014,
    w_inc_125k_150k = M5XE015,
    w_inc_150k_200k = M5XE016,
    w_inc_200k_more = M5XE017
  )

# Add Total Households to CBSA HH Income Data by Race -------------------

cbsa_race_07_11 <- cbsa_07_11 %>%
  select(tot_hh, CBSA) %>%
  left_join(cbsa_race_07_11, county_07_11, by = c("CBSA"))

# Save Cleaned Data -------------------------------------------------------

save(cbsa_race_07_11, file = "data/processed/cbsa_race_hh_income_07_11.rdata")
