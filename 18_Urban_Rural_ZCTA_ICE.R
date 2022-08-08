# Description -------------------------------------------------------------

## ZCTA Level Index of Concentrations at the Extremes (ICE):
#
# 2007-11 US Census Data
#
## Assign ICE values to urban and rural ZCTAs
#     Rural ZCTAs - use county level ICE
#     Urban ZCTAs - use CBSA level ICE


# Load Packages -----------------------------------------------------------

library(sf)
library(tidyverse)
library(dplyr)
library(here)


# Load Data ---------------------------------------------------------------

#Rural and Urban ZCTA Data
load(here("data",
          "processed",
          "zcta_urban_rural.rdata"))

#CBSA ICE Data
load(here("data",
          "processed",
          "cbsa_ice_07_11.rdata"))

#County ICE Data
load(here("data",
          "processed",
          "county_state_ice_07_11.rdata"))

#Updated ZCTA Shapefile
zcta_sf <- st_read(here("data",
                        "processed",
                        "zcta_sf.rdata"))

#Raw County Shapefile
county_sf <- st_read(
  here(
    "data",
    "raw",
    "nhgis0048_shape",
    "nhgis0048_shapefile_tl2010_us_county_2010",
    "US_county_2010.shp"
  )
)

#Use CBSA Income Thresholds in Urban ZCTAs -------------------------------

zcta_cbsa_ice_07_11 <-
  merge(zcta_urban,
        cbsa_ice_07_11,
        by = "GISJOIN_CBSA",
        duplicateGeoms = T)


# Spatial Join ZCTA and County Shapefiles ---------------------------------

#to get county information for ZCTAs
zcta_county_sf <- st_join(zcta_sf, county_sf)

#select variables for merge with rural zctas
zcta_county_sf <- zcta_county_sf %>%
  select(GISJOIN_ZCTA, ZCTA5CE10, STATEFP10, COUNTYFP10)

# Merge ZCTA/County Shapefile and Rural ZCTA File -------------------------

#drop geometry from ZCTA_rural
zcta_rural <- zcta_rural %>%
  st_drop_geometry()

#merge
zcta_county_rural <-
  merge(zcta_county_sf,
        zcta_rural,
        by = "GISJOIN_ZCTA",
        duplicateGeoms = T)


# Use County Income Thresholds in Rural ZCTAs -----------------------------

#rename variables for matching
county_state_ice_07_11 <- county_state_ice_07_11 %>%
  rename(STATEFP10 = STATEA, COUNTYFP10 = COUNTYA)

#merge county_ice and urban ZCTA files by county identifier
#keep ZCTA with largest overlap
zcta_county_ice_07_11 <-
  merge(
    zcta_county_rural,
    county_state_ice_07_11,
    by = c("COUNTYFP10", "STATEFP10"),
    duplicateGeoms = T
  ) %>%
  group_by(GISJOIN_ZCTA) %>%
  arrange(desc(p_overlap)) %>%
  slice(1) %>%
  ungroup()

# Save Data ---------------------------------------------------------------

save(zcta_county_ice_07_11, file = "data/processed/zcta_county_ice_07_11.rdata")
save(zcta_cbsa_ice_07_11, file = "data/processed/zcta_cbsa_ice_07_11.rdata")
