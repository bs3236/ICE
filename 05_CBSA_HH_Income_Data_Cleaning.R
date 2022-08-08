# Description -------------------------------------------------------------

## ZCTA Level Index of Concentrations at the Extremes (ICE):
#
# 2007-11 US Census Data
#
# CBSA Level Household Income in Past 12 Months Data Cleaning


# Load Packages ----------------------------------------------------------

library(tidyverse)
library(dplyr)
library(here)

# Load Data ---------------------------------------------------------------

#CBSA level HH income data 2007-11
cbsa_07_11 <- read_csv(here(
  "data",
  "raw",
  "nhgis0017_csv",
  "nhgis0017_ds184_20115_2011_cbsa.csv"
))

# Data Cleaning -----------------------------------------------------------

#HH income past 12 months: MP0E

#select and rename variables
cbsa_07_11 <- cbsa_07_11 %>%
  select(
    GISJOIN,
    YEAR,
    CBSA,
    MP0E001,
    MP0E002,
    MP0E003,
    MP0E004,
    MP0E005,
    MP0E006,
    MP0E007,
    MP0E008,
    MP0E009,
    MP0E010,
    MP0E011,
    MP0E012,
    MP0E013,
    MP0E014,
    MP0E015,
    MP0E016,
    MP0E017
  ) %>%
  rename(
    GISJOIN_CBSA = GISJOIN,
    tot_hh = MP0E001,
    inc_less_10k = MP0E002,
    inc_10k_15k = MP0E003,
    inc_15k_20k = MP0E004,
    inc_20k_25k = MP0E005,
    inc_25k_30k = MP0E006,
    inc_30k_35k = MP0E007,
    inc_35k_40k = MP0E008,
    inc_40k_45k = MP0E009,
    inc_45k_50k = MP0E010,
    inc_50k_60k = MP0E011,
    inc_60k_75k = MP0E012,
    inc_75k_100k = MP0E013,
    inc_100k_125k = MP0E014,
    inc_125k_150k = MP0E015,
    inc_150k_200k = MP0E016,
    inc_200k_more = MP0E017
  )

# Save Cleaned Data -------------------------------------------------------

save(cbsa_07_11, file = "data/processed/cbsa_hh_income_07_11.rdata")