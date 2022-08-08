# Description -------------------------------------------------------------

## ZCTA Level Index of Concentrations at the Extremes (ICE):
#
# 2007-11 US Census Data
#
# ACS ZCTA Data Cleaning


# Load Packages ----------------------------------------------------------

library(sf)
library(tidyverse)
library(dplyr)
library(here)

# Load Data ---------------------------------------------------------------

#ZCTA level ACS data 2007-11
acs_zcta_07_11 <- read_csv(here(
  "data",
  "raw",
  "nhgis0052_csv",
  "nhgis0052_ds184_20115_zcta.csv"
))
#ZCTA shapefile
zcta_sf <- st_read(
  here(
    "data",
    "raw",
    "nhgis0042_shape",
    "nhgis0042_shapefile_tl2010_us_zcta_2010",
    "US_zcta_2010.shp"
  )
)


# Data Cleaning -----------------------------------------------------------

#total population: MNTE
#Hispanic or Latino by race: MN2E
#Sex by educational attainment for population 25 years and over: MPSE
#Linguistically isolated: MPTE
#poverty level: MPVE
#Median HH income: MP1E

#Tenure: MS4E

#merge zcta shapefile to ACS 2007-11 data
acs_zcta_sf_07_11 <- merge(zcta_sf, acs_zcta_07_11, by = "GISJOIN")

#select variables, add percentages
acs_zcta_df_07_11 <- acs_zcta_sf_07_11 %>%
  rename(GISJOIN_ZCTA = GISJOIN,
         tot_pop = MNTE001,
         med_hh_inc = MP1E001) %>%
  mutate(
    p_nh_white = (MN2E003 / MN2E001) * 100,
    p_nh_black = (MN2E004 / MN2E001) * 100,
    p_nh_ai = (MN2E005 / MN2E001) * 100,
    p_nh_asian = (MN2E006 / MN2E001) * 100,
    p_hisp = (MN2E012 / MN2E001) * 100,
    p_no_hs = ((
      MPSE003 + MPSE004 + MPSE005 + MPSE006 + MPSE007 + MPSE008 + MPSE009 + MPSE010 +
        MPSE020 + MPSE021 + MPSE022 + MPSE023 + MPSE024 + MPSE025 + MPSE026 + MPSE027
    ) / MPSE001
    ) * 100,
    p_ling_iso = ((MPTE004 + MPTE007 + MPTE010 + MPTE013) / MPTE001) *
      100,
    p_pov = ((MPVE002 + MPVE003) / MPVE001) * 100,
    p_rent_occ = (MS4E003 / MS4E001) * 100,
    pop_density = tot_pop / (Shape_area * 0.001)
  ) %>%
  select(
    GISJOIN_ZCTA,
    tot_pop,
    med_hh_inc,
    p_nh_white,
    p_nh_black,
    p_nh_ai,
    p_nh_asian,
    p_hisp,
    p_no_hs,
    p_ling_iso,
    p_pov,
    p_rent_occ,
    pop_density
  ) %>%
  st_drop_geometry()


# Save Cleaned Data -------------------------------------------------------

save(acs_zcta_df_07_11, file = "data/processed/acs_zcta_07_11.rdata")
