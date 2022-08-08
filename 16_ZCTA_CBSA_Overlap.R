# Description -------------------------------------------------------------

## ZCTA Level Index of Concentrations at the Extremes (ICE):
#
# 2007-11 US Census Data
#
# Calculate overlap of ZCTAs and CBSAs
#   To determine rural and urban ZCTAS

# Load Packages ----------------------------------------------------------

library(sf)
library(tidyverse)
library(dplyr)
library(here)


# Load Data ---------------------------------------------------------------

#ZCTA Shapefile
zcta_sf <- st_read(
  here(
    "data",
    "raw",
    "nhgis0042_shape",
    "nhgis0042_shapefile_tl2010_us_zcta_2010",
    "US_zcta_2010.shp"
  )
)

#CBSA Shapefile
cbsa_sf <- st_read(
  here(
    "data",
    "raw",
    "nhgis0042_shape",
    "nhgis0042_shapefile_tl2010_us_cbsa_2010",
    "US_cbsa_2010.shp"
  )
)

# Rename GISJOIN Variables ------------------------------------------------

#ZCTA 
zcta_sf <- zcta_sf %>%
  rename(GISJOIN_ZCTA = GISJOIN)

#CBSA
cbsa_sf <- cbsa_sf %>%
  rename(GISJOIN_CBSA = GISJOIN)


# Spatially Intersect ZCTA and CBSA sf's (TIME CONSUMING) -----------------

#complete spatial intersection to determine overlap of ZCTAs with CBSAs
zcta_cbsa_overlap_pct <- st_intersection(st_make_valid(zcta_sf), st_make_valid(cbsa_sf)) %>%
  mutate(intersect_area = st_area(.)) %>% #create new column with shape area
  select(ZCTA5CE10, intersect_area) %>%  #select only ZCTAs and intersect area for join
  st_drop_geometry() #convert sf to a df

#Merge by GISJOIN_ZCTA
zcta_sf <- merge(zcta_sf, zcta_cbsa_overlap_pct, by = "GISJOIN_ZCTA", all.x = TRUE)


# Calculate Overlap Percentage of CBSA and ZCTA ---------------------------

zcta_sf <- zcta_sf %>% 
  mutate(p_overlap = as.numeric(intersect_area/Shape_area)*100)


# Spatial Join CBSAs and ZCTAs --------------------------------------------

zcta_cbsa_join <- st_join(zcta_sf, cbsa_sf)

#group by ZCTA and keep those with highest overlap with cbsa
zcta_cbsa_join <- zcta_cbsa_join %>%
  group_by(GISJOIN_ZCTA) %>%  
  arrange(desc(p_overlap)) %>%
  slice(1) %>%
  ungroup() 


# Select Variables --------------------------------------------------------

zcta_cbsa_join <- zcta_cbsa_join %>%
  select(GISJOIN_ZCTA, GISJOIN_CBSA, p_overlap) 


# Save Data ---------------------------------------------------------------

save(zcta_cbsa_join, file = "data/processed/zcta_cbsa_overlap.rdata")
save(zcta_sf, file = "data/processed/zcta_sf.rdata")
