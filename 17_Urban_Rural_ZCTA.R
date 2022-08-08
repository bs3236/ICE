# Description -------------------------------------------------------------

## ZCTA Level Index of Concentrations at the Extremes (ICE):
#
# 2007-11 US Census Data
#
# Separate urban and rural ZCTAs


# Load Packages -----------------------------------------------------------

library(sf)
library(tidyverse)
library(dplyr)
library(here)


# Load Data ---------------------------------------------------------------

#ZCTA-CBSA overlap
load(here("data",
          "processed",
          "zcta_cbsa_overlap.rdata"))

# Create Urban Variable ---------------------------------------------------

#if ZCTA overlaps at least 50% with a CBSA, mark as urban
#if ZCTA overlaps < 50% with a CBSA, not urban
zcta_cbsa_join <- zcta_cbsa_join %>%
  mutate(urban = if_else(p_overlap >= 50, 1, 0))

# Separating Urban and Rural ZCTAs ----------------------------------------

#rural zip codes = not urban
zcta_rural <- zcta_cbsa_join %>%
  filter(is.na(urban) | urban == 0)

#urban zip codes = urban
zcta_urban <- zcta_cbsa_join %>%
  filter(urban == 1) 


# Save Data ---------------------------------------------------------------

save(zcta_rural, zcta_urban, file = "data/processed/zcta_urban_rural.rdata")
