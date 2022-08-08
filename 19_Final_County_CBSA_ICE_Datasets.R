# Description -------------------------------------------------------------

## ZCTA Level Index of Concentrations at the Extremes (ICE):
#
# 2007-11 US Census Data
#
## Join ACS Data to County and CBSA Ice Files
#
## Save Final ICE Datasets

# Load Packages -----------------------------------------------------------

library(sf)
library(tidyverse)
library(dplyr)
library(here)

# Load Data ---------------------------------------------------------------

#ZCTA ACS Data
load(here("data",
     "processed",
     "acs_zcta_07_11.rdata"))

#ZCTA County ICE Data
load(here("data",
          "processed",
          "zcta_county_ice_07_11.rdata"))

#ZCTA CBSA ICE Data
load(here("data",
          "processed",
          "zcta_cbsa_ice_07_11.rdata"))


# Merge ACS Data with ZCTA ICE DAta ---------------------------------------

#join ZCTA ACS data to zcta_county_ice data
zcta_county_ice_acs_07_11 <-
  merge(
    zcta_county_ice_07_11,
    acs_zcta_df_07_11,
    by = "GISJOIN_ZCTA",
    duplicateGeoms = T
  ) 

#drop geometry
zcta_county_ice_acs_df_07_11 <- zcta_county_ice_acs_07_11 %>%
  st_drop_geometry()

#join ZCTA ACS data to zcta_cbsa_ice data
zcta_cbsa_ice_acs_07_11 <-
  merge(zcta_cbsa_ice_07_11,
        acs_zcta_df_07_11,
        by = "GISJOIN_ZCTA",
        duplicateGeoms = T)

#drop geometry
zcta_cbsa_ice_acs_df_07_11 <- zcta_cbsa_ice_acs_07_11 %>%
  st_drop_geometry()


# Write Final ZCTA ICE Files ----------------------------------------------

#rural/county ICE .csv
write_csv(zcta_county_ice_acs_df_07_11,
          "data/processed/zcta_county_ice_2007_2011.csv")

#urban/CBSA ICE .csv
write_csv(zcta_cbsa_ice_acs_df_07_11,
          "data/processed/zcta_cbsa_ice_acs_df_07_11.csv")


# Write Final .rdata File -------------------------------------------------

save(zcta_county_ice_acs_df_07_11,
     zcta_cbsa_ice_acs_df_07_11,
     file = "data/processed/zcta_ice_final_2007_2011.rdata")

