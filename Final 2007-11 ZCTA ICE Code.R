##Final ZCTA Level ICE Code - Static, Income Alone, and Race ICE for 2007-11 Census Years##
####set working directory####
setwd("~/Documents/Work_Documents/ICE/nhgis_files/")

####load libraries####
library(sf)
library(tidyverse)
library(stringr)
library(dplyr)

####load files####
acs_zcta_07_11 <- read_csv("nhgis0052_csv/nhgis0052_ds184_20115_zcta.csv") #ZCTA level ACS data 2007-11
cbsa_sf <- st_read("nhgis0042_shape/nhgis0042_shapefile_tl2010_us_cbsa_2010/US_cbsa_2010.shp") #CBSA shapefile
zcta_sf <- st_read("nhgis0042_shape/nhgis0042_shapefile_tl2010_us_zcta_2010/US_zcta_2010.shp") #ZCTA shapefile
county_sf <- st_read("nhgis0048_shape/nhgis0048_shapefile_tl2010_us_county_2010/US_county_2010.shp") #County shapefile
state_07_11 <- read_csv("nhgis0045_csv/nhgis0045_ds184_20115_state.csv") #State level HH income data 2007-11
county_07_11 <- read_csv("nhgis0047_csv/nhgis0047_ds184_20115_county.csv") #County level HH income data 2007-11
county_race_07_11 <- read_csv("nhgis0046_csv/nhgis0046_ds185_20115_county.csv") #County level HH income data, Black only and White only households 2007-11
cbsa_07_11 <- read_csv("nhgis0017_csv/nhgis0017_ds184_20115_2011_cbsa.csv") #CBSA level HH income data 2007-11
cbsa_race_07_11 <- read_csv("nhgis0026_csv/nhgis0026_ds185_20115_2011_cbsa.csv") #CBSA level HH income data 2007-11, Black only and White only households
zcta_cbsa_overlap_pct <- readRDS("zcta_cbsa_overlap/zcta_cbsa_overlap.rds") #spatial overlap of ZCTAs and CBSAs

####ZCTA ACS Data Cleaning####
#total population: MNTE
#Hispanic or Latino by race: MN2E
#Sex by educational attainment for population 25 years and over: MPSE
#Linguistically isolated: MPTE
#poverty level: MPVE
#Median HH income: MP1E
#Tenure: MS4E

#merge zcta shapefile to ACS 2007-11 data
acs_zcta_07_11_sf <- merge(zcta_sf, acs_zcta_07_11, by = "GISJOIN")

#select variables, add percentages
acs_zcta_07_11_df <- acs_zcta_07_11_sf %>%
  rename(GISJOIN_ZCTA = GISJOIN, tot_pop = MNTE001, med_hh_inc = MP1E001) %>% 
  mutate(p_nh_white = (MN2E003/MN2E001)*100, p_nh_black = (MN2E004/MN2E001)*100,
         p_nh_ai = (MN2E005/MN2E001)*100, p_nh_asian = (MN2E006/MN2E001)*100, p_hisp = (MN2E012/MN2E001)*100,
         p_no_hs = ((MPSE003 + MPSE004 + MPSE005 + MPSE006 + MPSE007 + MPSE008 + MPSE009 + MPSE010 + 
                        MPSE020 + MPSE021 + MPSE022 + MPSE023 + MPSE024 + MPSE025 + MPSE026 + MPSE027)/MPSE001)*100,
         p_ling_iso = ((MPTE004 + MPTE007 + MPTE010 + MPTE013)/MPTE001)*100,
         p_pov = ((MPVE002 + MPVE003)/MPVE001)*100, p_rent_occ = (MS4E003/MS4E001)*100,
         pop_density = tot_pop/(Shape_area*0.001)) %>%
  select(GISJOIN_ZCTA, tot_pop, med_hh_inc, p_nh_white, p_nh_black, p_nh_ai, p_nh_asian, p_hisp,
         p_no_hs, p_ling_iso, p_pov, p_rent_occ, pop_density) %>%
  st_drop_geometry()

####State Income Data Cleaning####
#select and rename variables
#HH income past 12 months: MP0E

state_07_11 <- state_07_11 %>%
  select(GISJOIN, YEAR, STATE, STATEA, MP0E001, MP0E002, MP0E003, MP0E004, MP0E005, MP0E006, 
         MP0E007, MP0E008, MP0E009, MP0E010, MP0E011, MP0E012, MP0E013, MP0E014, MP0E015, MP0E016, MP0E017) %>%
  rename(GISJOIN_state = GISJOIN, tot_hh = MP0E001, inc_less_10k = MP0E002, inc_10k_15k = MP0E003, inc_15k_20k = MP0E004, 
         inc_20k_25k = MP0E005, inc_25k_30k = MP0E006, inc_30k_35k = MP0E007, inc_35k_40k = MP0E008,
         inc_40k_45k = MP0E009, inc_45k_50k = MP0E010, inc_50k_60k = MP0E011, inc_60k_75k = MP0E012, 
         inc_75k_100k = MP0E013, inc_100k_125k = MP0E014, inc_125k_150k = MP0E015, inc_150k_200k = MP0E016,
         inc_200k_more = MP0E017)
####County Income Data Cleaning####
#select and rename variables
county_07_11 <- county_07_11 %>%
  select(GISJOIN, YEAR, STATE, COUNTY, COUNTYA, MP0E001, MP0E002, MP0E003, MP0E004, MP0E005, MP0E006, 
         MP0E007, MP0E008, MP0E009, MP0E010, MP0E011, MP0E012, MP0E013, MP0E014, MP0E015, MP0E016, MP0E017) %>%
  rename(GISJOIN_county = GISJOIN, tot_hh = MP0E001, inc_less_10k = MP0E002, inc_10k_15k = MP0E003, inc_15k_20k = MP0E004, 
         inc_20k_25k = MP0E005, inc_25k_30k = MP0E006, inc_30k_35k = MP0E007, inc_35k_40k = MP0E008,
         inc_40k_45k = MP0E009, inc_45k_50k = MP0E010, inc_50k_60k = MP0E011, inc_60k_75k = MP0E012, 
         inc_75k_100k = MP0E013, inc_100k_125k = MP0E014, inc_125k_150k = MP0E015, inc_150k_200k = MP0E016,
         inc_200k_more = MP0E017)

####County Race Income Data Cleaning####
#select and rename variables
#HH income in past 12 months - White alone households: M5QE
#HH income in past 12 months - Black or African American alone households: M5RE

county_race_07_11 <- county_race_07_11 %>%
  select(GISJOIN, YEAR, STATE, COUNTY, COUNTYA, M5RE001, M5RE002, M5RE003, M5RE004, M5RE005, M5RE006, 
         M5RE007, M5RE008, M5RE009, M5RE010, M5RE011, M5RE012, M5RE013, M5RE014, M5RE015, M5RE016, M5RE017, 
         M5QE001, M5QE002, M5QE003, M5QE004, M5QE005, M5QE006, 
         M5QE007, M5QE008, M5QE009, M5QE010, M5QE011, M5QE012, M5QE013, M5QE014, M5QE015, M5QE016, M5QE017) %>%
  rename(GISJOIN_county = GISJOIN, b_tot_hh = M5RE001, b_inc_less_10k = M5RE002, b_inc_10k_15k = M5RE003, b_inc_15k_20k = M5RE004, 
         b_inc_20k_25k = M5RE005, b_inc_25k_30k = M5RE006, b_inc_30k_35k = M5RE007, b_inc_35k_40k = M5RE008,
         b_inc_40k_45k = M5RE009, b_inc_45k_50k = M5RE010, b_inc_50k_60k = M5RE011, b_inc_60k_75k = M5RE012, 
         b_inc_75k_100k = M5RE013, b_inc_100k_125k = M5RE014, b_inc_125k_150k = M5RE015, b_inc_150k_200k = M5RE016,
         b_inc_200k_more = M5RE017, 
         w_tot_hh = M5QE001, w_inc_less_10k = M5QE002, w_inc_10k_15k = M5QE003, w_inc_15k_20k = M5QE004, 
         w_inc_20k_25k = M5QE005, w_inc_25k_30k = M5QE006, w_inc_30k_35k = M5QE007, w_inc_35k_40k = M5QE008,
         w_inc_40k_45k = M5QE009, w_inc_45k_50k = M5QE010, w_inc_50k_60k = M5QE011, w_inc_60k_75k = M5QE012, 
         w_inc_75k_100k = M5QE013, w_inc_100k_125k = M5QE014, w_inc_125k_150k = M5QE015, w_inc_150k_200k = M5QE016,
         w_inc_200k_more = M5QE017)

####CBSA Income Data Cleaning####
#select and rename variables
cbsa_07_11 <- cbsa_07_11 %>%
  select(GISJOIN, YEAR, CBSA, MP0E001, MP0E002, MP0E003, MP0E004, MP0E005, MP0E006, 
         MP0E007, MP0E008, MP0E009, MP0E010, MP0E011, MP0E012, MP0E013, MP0E014, MP0E015, MP0E016, MP0E017) %>%
  rename(GISJOIN_CBSA = GISJOIN, tot_hh = MP0E001, inc_less_10k = MP0E002, inc_10k_15k = MP0E003, inc_15k_20k = MP0E004, 
         inc_20k_25k = MP0E005, inc_25k_30k = MP0E006, inc_30k_35k = MP0E007, inc_35k_40k = MP0E008,
         inc_40k_45k = MP0E009, inc_45k_50k = MP0E010, inc_50k_60k = MP0E011, inc_60k_75k = MP0E012, 
         inc_75k_100k = MP0E013, inc_100k_125k = MP0E014, inc_125k_150k = MP0E015, inc_150k_200k = MP0E016,
         inc_200k_more = MP0E017)
####CBSA Income Race Data Cleaning####
#select and rename variables
cbsa_race_07_11 <- cbsa_race_07_11 %>%
  select(GISJOIN, YEAR, CBSA, M5RE001, M5RE002, M5RE003, M5RE004, M5RE005, M5RE006, 
         M5RE007, M5RE008, M5RE009, M5RE010, M5RE011, M5RE012, M5RE013, M5RE014, M5RE015, M5RE016, M5RE017, 
         M5XE001, M5XE002, M5XE003, M5XE004, M5XE005, M5XE006, 
         M5XE007, M5XE008, M5XE009, M5XE010, M5XE011, M5XE012, M5XE013, M5XE014, M5XE015, M5XE016, M5XE017) %>%
  rename(GISJOIN_CBSA = GISJOIN, b_inc_total = M5RE001, b_inc_less_10k = M5RE002, b_inc_10k_15k = M5RE003, b_inc_15k_20k = M5RE004, 
         b_inc_20k_25k = M5RE005, b_inc_25k_30k = M5RE006, b_inc_30k_35k = M5RE007, b_inc_35k_40k = M5RE008,
         b_inc_40k_45k = M5RE009, b_inc_45k_50k = M5RE010, b_inc_50k_60k = M5RE011, b_inc_60k_75k = M5RE012, 
         b_inc_75k_100k = M5RE013, b_inc_100k_125k = M5RE014, b_inc_125k_150k = M5RE015, b_inc_150k_200k = M5RE016,
         b_inc_200k_more = M5RE017, 
         w_inc_total = M5XE001, w_inc_less_10k = M5XE002, w_inc_10k_15k = M5XE003, w_inc_15k_20k = M5XE004, 
         w_inc_20k_25k = M5XE005, w_inc_25k_30k = M5XE006, w_inc_30k_35k = M5XE007, w_inc_35k_40k = M5XE008,
         w_inc_40k_45k = M5XE009, w_inc_45k_50k = M5XE010, w_inc_50k_60k = M5XE011, w_inc_60k_75k = M5XE012, 
         w_inc_75k_100k = M5XE013, w_inc_100k_125k = M5XE014, w_inc_125k_150k = M5XE015, w_inc_150k_200k = M5XE016,
         w_inc_200k_more = M5XE017)
####Determine 20th and 80th Percentile Household: State####
#use the 20th & 80th percentile of the total households in each State to determine the household number
state_07_11 <- state_07_11 %>%
  mutate(p20_hh_state = tot_hh * 0.2) %>%
  mutate(p80_hh_state = tot_hh * 0.8)

####Determine 20th and 80th Percentile Household Income Cutoff Points: State####
##State 20th percentile HH cutoff
#if the 20th percentile household is in given income bucket, set the cutoff to the midpoint of that income bucket
state_07_11 <- state_07_11 %>%
  mutate(p20_cutoff_state = if_else(p20_hh_state < inc_less_10k, 5000, 
                                    if_else(p20_hh_state >= inc_less_10k & p20_hh_state < inc_less_10k + inc_10k_15k, 12500, 
                                            if_else(p20_hh_state >= inc_less_10k + inc_10k_15k & p20_hh_state < inc_less_10k + inc_10k_15k + inc_15k_20k, 17500, 
                                                    if_else(p20_hh_state >= inc_less_10k + inc_10k_15k + inc_15k_20k & p20_hh_state < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k, 22500, 
                                                            if_else(p20_hh_state >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k & p20_hh_state < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k, 27500,
                                                                    if_else(p20_hh_state >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k & p20_hh_state < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k, 32500,
                                                                            if_else(p20_hh_state >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k & p20_hh_state < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k, 37500,
                                                                                    if_else(p20_hh_state >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k & p20_hh_state < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k, 42500,
                                                                                            if_else(p20_hh_state >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k & p20_hh_state < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k, 47500,
                                                                                                    if_else(p20_hh_state >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k & p20_hh_state < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k, 55000,
                                                                                                            if_else(p20_hh_state >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k & p20_hh_state < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k, 67500,
                                                                                                                    if_else(p20_hh_state >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k & p20_hh_state < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k + inc_75k_100k, 87500,
                                                                                                                            if_else(p20_hh_state >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k + inc_75k_100k & p20_hh_state < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k + inc_75k_100k + inc_100k_125k, 112500,
                                                                                                                                    if_else(p20_hh_state >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k + inc_75k_100k + inc_100k_125k & p20_hh_state < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k + inc_75k_100k + inc_100k_125k + inc_125k_150k, 137500,
                                                                                                                                            if_else(p20_hh_state >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k + inc_75k_100k + inc_100k_125k + inc_125k_150k & p20_hh_state < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k + inc_75k_100k + inc_100k_125k + inc_125k_150k + inc_150k_200k, 175000,
                                                                                                                                                    if_else(p20_hh_state >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k + inc_75k_100k + inc_100k_125k + inc_125k_150k + inc_150k_200k, 200000, NA_real_
                                                                                                                                                    )))))))))))))))))
##State 80th percentile HH cutoff
##if the 80th percentile household is in given income bucket, set the cutoff to the midpoint of that income bucket
state_07_11 <- state_07_11 %>%
  mutate(p80_cutoff_state = if_else(p80_hh_state < inc_less_10k, 5000, 
                                    if_else(p80_hh_state >= inc_less_10k & p80_hh_state < inc_less_10k + inc_10k_15k, 12500, 
                                            if_else(p80_hh_state >= inc_less_10k + inc_10k_15k & p80_hh_state < inc_less_10k + inc_10k_15k + inc_15k_20k, 17500, 
                                                    if_else(p80_hh_state >= inc_less_10k + inc_10k_15k + inc_15k_20k & p80_hh_state < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k, 22500, 
                                                            if_else(p80_hh_state >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k & p80_hh_state < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k, 27500,
                                                                    if_else(p80_hh_state >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k & p80_hh_state < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k, 32500,
                                                                            if_else(p80_hh_state >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k & p80_hh_state < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k, 37500,
                                                                                    if_else(p80_hh_state >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k & p80_hh_state < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k, 42500,
                                                                                            if_else(p80_hh_state >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k & p80_hh_state < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k, 47500,
                                                                                                    if_else(p80_hh_state >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k & p80_hh_state < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k, 55000,
                                                                                                            if_else(p80_hh_state >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k & p80_hh_state < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k, 67500,
                                                                                                                    if_else(p80_hh_state >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k & p80_hh_state < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k + inc_75k_100k, 87500,
                                                                                                                            if_else(p80_hh_state >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k + inc_75k_100k & p80_hh_state < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k + inc_75k_100k + inc_100k_125k, 112500,
                                                                                                                                    if_else(p80_hh_state >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k + inc_75k_100k + inc_100k_125k & p80_hh_state < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k + inc_75k_100k + inc_100k_125k + inc_125k_150k, 137500,
                                                                                                                                            if_else(p80_hh_state >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k + inc_75k_100k + inc_100k_125k + inc_125k_150k & p80_hh_state < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k + inc_75k_100k + inc_100k_125k + inc_125k_150k + inc_150k_200k, 175000,
                                                                                                                                                    if_else(p80_hh_state >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k + inc_75k_100k + inc_100k_125k + inc_125k_150k + inc_150k_200k, 200000, NA_real_
                                                                                                                                                    )))))))))))))))))
####Create Ascending Threshold Bins, Where Each Bin is the Income Bin the 20th and 80th percentile Fell In for Income Alone####
#State 20th percentile bin
state_07_11 <- state_07_11 %>%
  mutate(threshold_bin_p20_state = if_else(p20_cutoff_state == 5000, 1,
                                           if_else(p20_cutoff_state == 12500, 2,
                                                   if_else(p20_cutoff_state == 17500, 3,
                                                           if_else(p20_cutoff_state == 22500, 4,
                                                                   if_else(p20_cutoff_state == 27500, 5,
                                                                           if_else(p20_cutoff_state == 32500, 6,
                                                                                   if_else(p20_cutoff_state == 37500, 7,
                                                                                           if_else(p20_cutoff_state == 42500, 8,
                                                                                                   if_else(p20_cutoff_state == 47500, 9,
                                                                                                           if_else(p20_cutoff_state == 55000, 10,
                                                                                                                   if_else(p20_cutoff_state == 67500, 11,
                                                                                                                           if_else(p20_cutoff_state == 87500, 12,
                                                                                                                                   if_else(p20_cutoff_state == 112500, 13,
                                                                                                                                           if_else(p20_cutoff_state == 137500, 14, 
                                                                                                                                                   if_else(p20_cutoff_state == 175000, 15,
                                                                                                                                                           if_else(p20_cutoff_state == 200000, 16, NA_real_
                                                                                                                                                           )))))))))))))))))
#State 80th percentile bin
state_07_11 <- state_07_11 %>%
  mutate(threshold_bin_p80_state = if_else(p80_cutoff_state == 5000, 1,
                                           if_else(p80_cutoff_state == 12500, 2,
                                                   if_else(p80_cutoff_state == 17500, 3,
                                                           if_else(p80_cutoff_state == 22500, 4,
                                                                   if_else(p80_cutoff_state == 27500, 5,
                                                                           if_else(p80_cutoff_state == 32500, 6,
                                                                                   if_else(p80_cutoff_state == 37500, 7,
                                                                                           if_else(p80_cutoff_state == 42500, 8,
                                                                                                   if_else(p80_cutoff_state == 47500, 9,
                                                                                                           if_else(p80_cutoff_state == 55000, 10,
                                                                                                                   if_else(p80_cutoff_state == 67500, 11,
                                                                                                                           if_else(p80_cutoff_state == 87500, 12,
                                                                                                                                   if_else(p80_cutoff_state == 112500, 13,
                                                                                                                                           if_else(p80_cutoff_state == 137500, 14, 
                                                                                                                                                   if_else(p80_cutoff_state == 175000, 15,
                                                                                                                                                           if_else(p80_cutoff_state == 200000, 16, NA_real_
                                                                                                                                                           )))))))))))))))))


####Combine County Income and State df's####
county_state_07_11 <- state_07_11 %>%
  select(STATE, STATEA, p20_hh_state, p80_hh_state, p20_cutoff_state, p80_cutoff_state,
         threshold_bin_p20_state, threshold_bin_p80_state) %>%
  left_join(county_07_11, state_07_11, by = "STATE")

####Combine County Race Income and State df's####
county_state_race_07_11 <- state_07_11 %>%
  select(STATE, STATEA, p20_hh_state, p80_hh_state, p20_cutoff_state, p80_cutoff_state,
         threshold_bin_p20_state, threshold_bin_p80_state) %>%
  left_join(county_race_07_11, state_07_11, by = "STATE") 

####Add total households from County Income to county_state_race_07_11####
county_state_race_07_11 <- county_07_11 %>%
  select(tot_hh, STATE, COUNTY) %>%
  left_join(county_state_race_07_11, county_07_11, by = c("STATE", "COUNTY"))

####Add total households from CBSA Income to cbsa_race_07_11####
cbsa_race_07_11 <- cbsa_07_11 %>%
  select(tot_hh, CBSA) %>%
  left_join(cbsa_race_07_11, county_07_11, by = c("CBSA"))

####Add 2007-11 Static Income Cutoffs from https://www2.census.gov/library/publications/2010/demo/p60-238/p60-238.pdf####
#county 07-11 no race
county_state_07_11 <- county_state_07_11  %>%
  mutate(p20_static = 20453, p80_static = 100000)

#county 07-11 with race
county_state_race_07_11 <- county_state_race_07_11 %>%
  mutate(p20_static = 20453, p80_static = 100000)

#cbsa 07-11 no race
cbsa_07_11 <- cbsa_07_11 %>%
  mutate(p20_static = 20453, p80_static = 100000)

#cbsa 07-11 with race
cbsa_race_07_11 <- cbsa_race_07_11 %>%
  mutate(p20_static = 20453, p80_static = 100000)

####Income Concentrations at the Extremes (ICE) Income Alone, Independent of Race - Static Thresholds: County and CBSA####
#county 2007-11
county_state_07_11 <- county_state_07_11 %>%
  mutate(poor_income_static = (inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k*(0.5)),
         #poor_income_static = All households <20th percentile of $20453
         rich_income_static = (inc_100k_125k*(0.5) + inc_125k_150k + inc_150k_200k + inc_200k_more)) %>%
  #rich_income_static = All households >80th percentile of $100000
  mutate(static_ice_income = (rich_income_static - poor_income_static)/tot_hh) 
summary(county_state_07_11$static_ice_income)

#cbsa 2007-11
cbsa_07_11 <- cbsa_07_11 %>%
  mutate(poor_income_static = (inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k*(0.5)),
         #poor_income_static = All households <20th percentile of $20453
         rich_income_static = (inc_100k_125k*(0.5) + inc_125k_150k + inc_150k_200k + inc_200k_more)) %>%
  #rich_income_static = All households >80th percentile of $100000
  mutate(static_ice_income = (rich_income_static - poor_income_static)/tot_hh) 
summary(cbsa_07_11$static_ice_income)

####Income Concentrations at the Extremes (ICE) with Race - Static Thresholds: County and CBSA####
##Create poor_black_static variable (all Black households <20th percentile income) 
##Create rich_white_static variable (all NH-White households >80th percentile income)
##Create static_ice_race (all NH-White households >80th percentile income - all Black households <20th percentile income)/Total Households)
#county 2007-11
county_state_race_07_11 <- county_state_race_07_11 %>%
  mutate(poor_black_static = (b_inc_less_10k + b_inc_10k_15k + b_inc_15k_20k + b_inc_20k_25k*(0.5)),
         #poor_black_static = Black households <20th percentile of $20453
         rich_white_static = (w_inc_100k_125k*(0.5) + w_inc_125k_150k + w_inc_150k_200k + w_inc_200k_more)) %>%
  #rich_white_static = White households >80th percentile of $100000
  mutate(static_ice_race = (rich_white_static - poor_black_static)/tot_hh) 
summary(county_state_race_07_11$static_ice_race)

#CBSA 2007-11
cbsa_race_07_11 <- cbsa_race_07_11 %>%
  mutate(poor_black_static = (b_inc_less_10k + b_inc_10k_15k + b_inc_15k_20k + b_inc_20k_25k*(0.5)),
         #poor_black_static = Black households <20th percentile of $20453
         rich_white_static = (w_inc_100k_125k*(0.5) + w_inc_125k_150k + w_inc_150k_200k + w_inc_200k_more)) %>%
  #rich_white_static = White households >80th percentile of $100000
  mutate(static_ice_race = (rich_white_static - poor_black_static)/tot_hh) 
summary(cbsa_race_07_11$static_ice_race)

####Determine 20th and 80th Percentile Household: CBSA####
#use the 20th & 80th percentile of the total households in each CBSA to determine the household number
#for Counties, use State 20th and 80th percentile

#CBSA 07-11 no race
cbsa_07_11 <- cbsa_07_11 %>%
  mutate(p20_hh_cbsa = tot_hh * 0.2) %>%
  mutate(p80_hh_cbsa = tot_hh * 0.8)

#CBSA 07-11 race
cbsa_race_07_11 <- cbsa_race_07_11 %>%
  mutate(p20_hh_cbsa = tot_hh * 0.2) %>%
  mutate(p80_hh_cbsa = tot_hh * 0.8)
####Determine 20th and 80th Percentile Household Income Cutoff Points: CBSA####
##if the 20th percentile household is in given income bucket, set the cutoff to the midpoint of that income bucket
#cbsa 2007-11
cbsa_07_11 <- cbsa_07_11 %>%
  mutate(p20_cutoff_cbsa = if_else(p20_hh_cbsa < inc_less_10k, 5000, 
                                   if_else(p20_hh_cbsa >= inc_less_10k & p20_hh_cbsa < inc_less_10k + inc_10k_15k, 12500, 
                                           if_else(p20_hh_cbsa >= inc_less_10k + inc_10k_15k & p20_hh_cbsa < inc_less_10k + inc_10k_15k + inc_15k_20k, 17500, 
                                                   if_else(p20_hh_cbsa >= inc_less_10k + inc_10k_15k + inc_15k_20k & p20_hh_cbsa < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k, 22500, 
                                                           if_else(p20_hh_cbsa >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k & p20_hh_cbsa < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k, 27500,
                                                                   if_else(p20_hh_cbsa >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k & p20_hh_cbsa < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k, 32500,
                                                                           if_else(p20_hh_cbsa >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k & p20_hh_cbsa < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k, 37500,
                                                                                   if_else(p20_hh_cbsa >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k & p20_hh_cbsa < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k, 42500,
                                                                                           if_else(p20_hh_cbsa >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k & p20_hh_cbsa < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k, 47500,
                                                                                                   if_else(p20_hh_cbsa >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k & p20_hh_cbsa < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k, 55000,
                                                                                                           if_else(p20_hh_cbsa >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k & p20_hh_cbsa < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k, 67500,
                                                                                                                   if_else(p20_hh_cbsa >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k & p20_hh_cbsa < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k + inc_75k_100k, 87500,
                                                                                                                           if_else(p20_hh_cbsa >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k + inc_75k_100k & p20_hh_cbsa < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k + inc_75k_100k + inc_100k_125k, 112500,
                                                                                                                                   if_else(p20_hh_cbsa >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k + inc_75k_100k + inc_100k_125k & p20_hh_cbsa < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k + inc_75k_100k + inc_100k_125k + inc_125k_150k, 137500,
                                                                                                                                           if_else(p20_hh_cbsa >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k + inc_75k_100k + inc_100k_125k + inc_125k_150k & p20_hh_cbsa < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k + inc_75k_100k + inc_100k_125k + inc_125k_150k + inc_150k_200k, 175000,
                                                                                                                                                   if_else(p20_hh_cbsa >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k + inc_75k_100k + inc_100k_125k + inc_125k_150k + inc_150k_200k, 200000, NA_real_
                                                                                                                                                   )))))))))))))))))

##if the 80th percentile household is in given income bucket, set the cutoff to the midpoint of that income bucket
#cbsa 2007-11
cbsa_07_11 <- cbsa_07_11 %>%
  mutate(p80_cutoff_cbsa = if_else(p80_hh_cbsa < inc_less_10k, 5000, 
                                   if_else(p80_hh_cbsa >= inc_less_10k & p80_hh_cbsa < inc_less_10k + inc_10k_15k, 12500, 
                                           if_else(p80_hh_cbsa >= inc_less_10k + inc_10k_15k & p80_hh_cbsa < inc_less_10k + inc_10k_15k + inc_15k_20k, 17500, 
                                                   if_else(p80_hh_cbsa >= inc_less_10k + inc_10k_15k + inc_15k_20k & p80_hh_cbsa < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k, 22500, 
                                                           if_else(p80_hh_cbsa >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k & p80_hh_cbsa < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k, 27500,
                                                                   if_else(p80_hh_cbsa >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k & p80_hh_cbsa < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k, 32500,
                                                                           if_else(p80_hh_cbsa >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k & p80_hh_cbsa < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k, 37500,
                                                                                   if_else(p80_hh_cbsa >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k & p80_hh_cbsa < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k, 42500,
                                                                                           if_else(p80_hh_cbsa >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k & p80_hh_cbsa < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k, 47500,
                                                                                                   if_else(p80_hh_cbsa >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k & p80_hh_cbsa < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k, 55000,
                                                                                                           if_else(p80_hh_cbsa >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k & p80_hh_cbsa < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k, 67500,
                                                                                                                   if_else(p80_hh_cbsa >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k & p80_hh_cbsa < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k + inc_75k_100k, 87500,
                                                                                                                           if_else(p80_hh_cbsa >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k + inc_75k_100k & p80_hh_cbsa < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k + inc_75k_100k + inc_100k_125k, 112500,
                                                                                                                                   if_else(p80_hh_cbsa >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k + inc_75k_100k + inc_100k_125k & p80_hh_cbsa < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k + inc_75k_100k + inc_100k_125k + inc_125k_150k, 137500,
                                                                                                                                           if_else(p80_hh_cbsa >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k + inc_75k_100k + inc_100k_125k + inc_125k_150k & p80_hh_cbsa < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k + inc_75k_100k + inc_100k_125k + inc_125k_150k + inc_150k_200k, 175000,
                                                                                                                                                   if_else(p80_hh_cbsa >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k + inc_75k_100k + inc_100k_125k + inc_125k_150k + inc_150k_200k, 200000, NA_real_
                                                                                                                                                   )))))))))))))))))


####Create Ascending Threshold Bins, Where Each Bin is the Income Bin the 20th and 80th percentile Fell In: CBSA####

#Create threshold bins in order: 5000 = 1, 12500 = 2, 17500 = 3, 22500 = 4, 27500 = 5, 32500 = 6, 37500 = 7, 
#42500 = 8, 47500 = 9, 55000 = 10, 67500 = 11, 87500 = 12, 112500 = 13, 137500 = 14, 175000 = 15, 200000 = 16

##20th Percentile Threshold Bin
#CBSA 2007-11
cbsa_07_11 <- cbsa_07_11 %>%
  mutate(threshold_bin_p20_cbsa = if_else(p20_cutoff_cbsa == 5000, 1,
                                          if_else(p20_cutoff_cbsa == 12500, 2,
                                                  if_else(p20_cutoff_cbsa == 17500, 3,
                                                          if_else(p20_cutoff_cbsa == 22500, 4,
                                                                  if_else(p20_cutoff_cbsa == 27500, 5,
                                                                          if_else(p20_cutoff_cbsa == 32500, 6,
                                                                                  if_else(p20_cutoff_cbsa == 37500, 7,
                                                                                          if_else(p20_cutoff_cbsa == 42500, 8,
                                                                                                  if_else(p20_cutoff_cbsa == 47500, 9,
                                                                                                          if_else(p20_cutoff_cbsa == 55000, 10,
                                                                                                                  if_else(p20_cutoff_cbsa == 67500, 11,
                                                                                                                          if_else(p20_cutoff_cbsa == 87500, 12,
                                                                                                                                  if_else(p20_cutoff_cbsa == 112500, 13,
                                                                                                                                          if_else(p20_cutoff_cbsa == 137500, 14, 
                                                                                                                                                  if_else(p20_cutoff_cbsa == 175000, 15,
                                                                                                                                                          if_else(p20_cutoff_cbsa == 200000, 16, NA_real_
                                                                                                                                                          )))))))))))))))))
##80th Percentile Threshold Bin
#CBSA 2007-11
cbsa_07_11 <- cbsa_07_11 %>%
  mutate(threshold_bin_p80_cbsa = if_else(p80_cutoff_cbsa == 5000, 1,
                                          if_else(p80_cutoff_cbsa == 12500, 2,
                                                  if_else(p80_cutoff_cbsa == 17500, 3,
                                                          if_else(p80_cutoff_cbsa == 22500, 4,
                                                                  if_else(p80_cutoff_cbsa == 27500, 5,
                                                                          if_else(p80_cutoff_cbsa == 32500, 6,
                                                                                  if_else(p80_cutoff_cbsa == 37500, 7,
                                                                                          if_else(p80_cutoff_cbsa == 42500, 8,
                                                                                                  if_else(p80_cutoff_cbsa == 47500, 9,
                                                                                                          if_else(p80_cutoff_cbsa == 55000, 10,
                                                                                                                  if_else(p80_cutoff_cbsa == 67500, 11,
                                                                                                                          if_else(p80_cutoff_cbsa == 87500, 12,
                                                                                                                                  if_else(p80_cutoff_cbsa == 112500, 13,
                                                                                                                                          if_else(p80_cutoff_cbsa == 137500, 14, 
                                                                                                                                                  if_else(p80_cutoff_cbsa == 175000, 15,
                                                                                                                                                          if_else(p80_cutoff_cbsa == 200000, 16, NA_real_
                                                                                                                                                          )))))))))))))))))

####Pivot All HH Income Dataset from Wide to Long in Order to Sum Across Rows: County and CBSA####
#county 2007-11
county_state_pivot_07_11 <- county_state_07_11 %>%
  select(GISJOIN_county, tot_hh, threshold_bin_p20_state, threshold_bin_p80_state, inc_less_10k:inc_200k_more) %>%
  pivot_longer(cols = starts_with("inc_"), names_to = "income", names_prefix = "", values_to = "count", values_drop_na = TRUE)

#CBSA 2007-11
cbsa_pivot_07_11 <- cbsa_07_11 %>%
  select(GISJOIN_CBSA, tot_hh, threshold_bin_p20_cbsa, threshold_bin_p80_cbsa, inc_less_10k:inc_200k_more) %>%
  pivot_longer(cols = starts_with("inc_"), names_to = "income", names_prefix = "", values_to = "count", values_drop_na = TRUE)
####Create pivoted income bins for all HH income file: County and CBSA####
#County 2007-11
county_state_pivot_07_11 <- county_state_pivot_07_11 %>%
  mutate(income_bin = if_else(income == "inc_less_10k", 1, 
                              if_else(income == "inc_10k_15k", 2, 
                                      if_else(income == "inc_15k_20k", 3,
                                              if_else(income == "inc_20k_25k", 4,
                                                      if_else(income == "inc_25k_30k", 5,
                                                              if_else(income == "inc_30k_35k", 6,
                                                                      if_else(income == "inc_35k_40k", 7,
                                                                              if_else(income == "inc_40k_45k", 8,
                                                                                      if_else(income == "inc_45k_50k", 9,
                                                                                              if_else(income == "inc_50k_60k", 10,
                                                                                                      if_else(income == "inc_60k_75k", 11,
                                                                                                              if_else(income == "inc_75k_100k", 12,
                                                                                                                      if_else(income == "inc_100k_125k", 13,
                                                                                                                              if_else(income == "inc_125k_150k", 14,
                                                                                                                                      if_else(income == "inc_150k_200k", 15,
                                                                                                                                              if_else(income == "inc_200k_more", 16, NA_real_)))))))))))))))))
#CBSA 2007-11
cbsa_pivot_07_11 <- cbsa_pivot_07_11 %>%
  mutate(income_bin = if_else(income == "inc_less_10k", 1, 
                              if_else(income == "inc_10k_15k", 2, 
                                      if_else(income == "inc_15k_20k", 3,
                                              if_else(income == "inc_20k_25k", 4,
                                                      if_else(income == "inc_25k_30k", 5,
                                                              if_else(income == "inc_30k_35k", 6,
                                                                      if_else(income == "inc_35k_40k", 7,
                                                                              if_else(income == "inc_40k_45k", 8,
                                                                                      if_else(income == "inc_45k_50k", 9,
                                                                                              if_else(income == "inc_50k_60k", 10,
                                                                                                      if_else(income == "inc_60k_75k", 11,
                                                                                                              if_else(income == "inc_75k_100k", 12,
                                                                                                                      if_else(income == "inc_100k_125k", 13,
                                                                                                                              if_else(income == "inc_125k_150k", 14,
                                                                                                                                      if_else(income == "inc_150k_200k", 15,
                                                                                                                                              if_else(income == "inc_200k_more", 16, NA_real_)))))))))))))))))

####Create Rich Variable for all HH income: County and CBSA####
##all HH Above the Overall County/CBSA HH 80th percentile for Income and 50% of HH Where Income = 80th percentile

#County 2007-11
county_state_pivot_rich_07_11 <- county_state_pivot_07_11 %>%
  mutate(count = as.double(as.character(count))) %>%
  mutate(sum_temp = case_when(income_bin > threshold_bin_p80_state ~ count, 
                              income_bin == threshold_bin_p80_state ~ count*0.5,
                              TRUE ~ NA_real_))

county_state_pivot_rich_07_11 <- county_state_pivot_rich_07_11 %>%
  group_by(GISJOIN_county) %>%
  summarise(rich = sum(sum_temp, na.rm = TRUE)) %>%
  group_by()
#join rich variable to ice dataset
county_state_07_11 <- left_join(county_state_07_11, county_state_pivot_rich_07_11, by = c("GISJOIN_county"))

#CBSA 2007-11
cbsa_pivot_rich_07_11 <- cbsa_pivot_07_11 %>%
  mutate(count = as.double(as.character(count))) %>%
  mutate(sum_temp = case_when(income_bin > threshold_bin_p80_cbsa ~ count, 
                              income_bin == threshold_bin_p80_cbsa ~ count*0.5,
                              TRUE ~ NA_real_))
cbsa_pivot_rich_07_11 <- cbsa_pivot_rich_07_11 %>%
  group_by(GISJOIN_CBSA) %>%
  summarise(rich = sum(sum_temp, na.rm = TRUE)) %>%
  group_by()
#join rich variable to ice dataset
cbsa_07_11 <- left_join(cbsa_07_11, cbsa_pivot_rich_07_11, by = c("GISJOIN_CBSA"))

####Create Poor Variable for all HH income Using case_when: County and CBSA####
##all HH below the Overall County/CBSA HH 20th percentile for Income and 50% of HH Where Income = 20th percentile


#County 2007-11
county_state_pivot_poor_07_11 <- county_state_pivot_07_11 %>%
  mutate(count = as.double(as.character(count))) %>%
  mutate(sum_temp = case_when(income_bin < threshold_bin_p20_state ~ count, 
                              income_bin == threshold_bin_p20_state ~ count*0.5,
                              TRUE ~ NA_real_))

county_state_pivot_poor_07_11 <- county_state_pivot_poor_07_11 %>%
  group_by(GISJOIN_county) %>%
  summarise(poor = sum(sum_temp, na.rm = TRUE)) %>%
  group_by()
#join poor variable to ice dataset
county_state_07_11 <- left_join(county_state_07_11, county_state_pivot_poor_07_11, by = c("GISJOIN_county"))

#CBSA 2007-11
cbsa_pivot_poor_07_11 <- cbsa_pivot_07_11 %>%
  mutate(count = as.double(as.character(count))) %>%
  mutate(sum_temp = case_when(income_bin < threshold_bin_p20_cbsa ~ count, 
                              income_bin == threshold_bin_p20_cbsa ~ count*0.5,
                              TRUE ~ NA_real_))

cbsa_pivot_poor_07_11 <- cbsa_pivot_poor_07_11 %>%
  group_by(GISJOIN_CBSA) %>%
  summarise(poor = sum(sum_temp, na.rm = TRUE)) %>%
  group_by()
#join poor variable to ice dataset
cbsa_07_11 <- left_join(cbsa_07_11, cbsa_pivot_poor_07_11, by = c("GISJOIN_CBSA"))

####Income Concentrations at the Extremes (ICE) Income Alone, Independent of Race - County and CBSA####
#County 2007-11
county_state_07_11 <- county_state_07_11 %>%
  mutate(ice_income = (rich - poor)/tot_hh)

#CBSA 2007-11
cbsa_07_11 <- cbsa_07_11 %>%
  mutate(ice_income = (rich - poor)/tot_hh)

####Join CBSA threshold bins to CBSA race data####
#CBSA 2007-11
cbsa_race_07_11 <- cbsa_07_11 %>%
  select(GISJOIN_CBSA, threshold_bin_p20_cbsa, threshold_bin_p80_cbsa) %>%
  left_join(cbsa_race_07_11, cbsa_07_11, by = c("GISJOIN_CBSA"))

####Pivot Black HH Dataset from Wide to Long in Order to Sum Across Rows: County and CBSA####
#county 2007-11
county_b_inc_07_11 <- county_state_race_07_11 %>%
  select(GISJOIN_county, tot_hh, threshold_bin_p20_state, b_inc_less_10k:b_inc_200k_more) %>%
  pivot_longer(cols = starts_with("b_inc"), names_to = "b_income", names_prefix = "inc", values_to = "count", values_drop_na = TRUE)

#cbsa 2007-11
cbsa_b_inc_07_11 <- cbsa_race_07_11 %>%
  select(GISJOIN_CBSA, tot_hh, threshold_bin_p20_cbsa, b_inc_less_10k:b_inc_200k_more) %>%
  pivot_longer(cols = starts_with("b_inc"), names_to = "b_income", names_prefix = "inc", values_to = "count", values_drop_na = TRUE)

####Code Black Household Income Buckets in Ascending Order: County and CBSA####
#County 2007-11
county_b_inc_07_11 <- county_b_inc_07_11 %>%
  mutate(b_income_bin = if_else(b_income == "b_inc_less_10k", 1, 
                                if_else(b_income == "b_inc_10k_15k", 2, 
                                        if_else(b_income == "b_inc_15k_20k", 3,
                                                if_else(b_income == "b_inc_20k_25k", 4,
                                                        if_else(b_income == "b_inc_25k_30k", 5,
                                                                if_else(b_income == "b_inc_30k_35k", 6,
                                                                        if_else(b_income == "b_inc_35k_40k", 7,
                                                                                if_else(b_income == "b_inc_40k_45k", 8,
                                                                                        if_else(b_income == "b_inc_45k_50k", 9,
                                                                                                if_else(b_income == "b_inc_50k_60k", 10,
                                                                                                        if_else(b_income == "b_inc_60k_75k", 11,
                                                                                                                if_else(b_income == "b_inc_75k_100k", 12,
                                                                                                                        if_else(b_income == "b_inc_100k_125k", 13,
                                                                                                                                if_else(b_income == "b_inc_125k_150k", 14,
                                                                                                                                        if_else(b_income == "b_inc_150k_200k", 15,
                                                                                                                                                if_else(b_income == "b_inc_200k_more", 16, NA_real_)))))))))))))))))

#CBSA 2007-11
cbsa_b_inc_07_11 <- cbsa_b_inc_07_11 %>%
  mutate(b_income_bin = if_else(b_income == "b_inc_less_10k", 1, 
                                if_else(b_income == "b_inc_10k_15k", 2, 
                                        if_else(b_income == "b_inc_15k_20k", 3,
                                                if_else(b_income == "b_inc_20k_25k", 4,
                                                        if_else(b_income == "b_inc_25k_30k", 5,
                                                                if_else(b_income == "b_inc_30k_35k", 6,
                                                                        if_else(b_income == "b_inc_35k_40k", 7,
                                                                                if_else(b_income == "b_inc_40k_45k", 8,
                                                                                        if_else(b_income == "b_inc_45k_50k", 9,
                                                                                                if_else(b_income == "b_inc_50k_60k", 10,
                                                                                                        if_else(b_income == "b_inc_60k_75k", 11,
                                                                                                                if_else(b_income == "b_inc_75k_100k", 12,
                                                                                                                        if_else(b_income == "b_inc_100k_125k", 13,
                                                                                                                                if_else(b_income == "b_inc_125k_150k", 14,
                                                                                                                                        if_else(b_income == "b_inc_150k_200k", 15,
                                                                                                                                                if_else(b_income == "b_inc_200k_more", 16, NA_real_)))))))))))))))))

####Create poor_black variable Using case_when: County and CBSA####
#all Black HH below the Overall County/CBSA HH 20th percentile for Income and 50% of HH Where Income = 20th percentile
#County 2007-11
county_poor_black_07_11 <- county_b_inc_07_11 %>%
  mutate(count = as.double(as.character(count))) %>%
  mutate(sum_temp = case_when(b_income_bin < threshold_bin_p20_state ~ count, 
                              b_income_bin == threshold_bin_p20_state ~ count*0.5,
                              TRUE ~ NA_real_))
county_poor_black_07_11 <- county_poor_black_07_11 %>%
  group_by(GISJOIN_county) %>%
  summarise(poor_black = sum(sum_temp, na.rm = TRUE))
#join poor_black variable to race dataset
county_state_race_07_11 <- county_poor_black_07_11 %>%
  left_join(county_state_race_07_11, county_poor_black_07_11, by = c("GISJOIN_county"))

#CBSA 2007-11
cbsa_poor_black_07_11 <- cbsa_b_inc_07_11 %>%
  mutate(count = as.double(as.character(count))) %>%
  mutate(sum_temp = case_when(b_income_bin < threshold_bin_p20_cbsa ~ count, 
                              b_income_bin == threshold_bin_p20_cbsa ~ count*0.5,
                              TRUE ~ NA_real_))
cbsa_poor_black_07_11 <- cbsa_poor_black_07_11 %>%
  group_by(GISJOIN_CBSA) %>%
  summarise(poor_black = sum(sum_temp, na.rm = TRUE))
#join poor_black variable to race dataset
cbsa_race_07_11 <- cbsa_poor_black_07_11 %>%
  left_join(cbsa_race_07_11, cbsa_07_11_poor_black, by = c("GISJOIN_CBSA"))

####Pivot White HH Dataset from Wide to Long in Order to Sum Across Rows: County and CBSA####
#county 2007-11
county_w_inc_07_11 <- county_state_race_07_11 %>%
  select(GISJOIN_county, tot_hh, threshold_bin_p80_state , w_inc_less_10k:w_inc_200k_more) %>%
  pivot_longer(cols = starts_with("w_inc"), names_to = "w_income", names_prefix = "inc", values_to = "count", values_drop_na = TRUE)

#cbsa 2007-11
cbsa_w_inc_07_11 <- cbsa_race_07_11 %>%
  select(GISJOIN_CBSA, tot_hh, threshold_bin_p80_cbsa, w_inc_less_10k:w_inc_200k_more) %>%
  pivot_longer(cols = starts_with("w_inc"), names_to = "w_income", names_prefix = "inc", values_to = "count", values_drop_na = TRUE)

####Code White Household Income Buckets in Ascending Order: County and CBSA####
#County 2007-11
county_w_inc_07_11 <- county_w_inc_07_11 %>%
  mutate(w_income_bin = if_else(w_income == "w_inc_less_10k", 1, 
                                if_else(w_income == "w_inc_10k_15k", 2, 
                                        if_else(w_income == "w_inc_15k_20k", 3,
                                                if_else(w_income == "w_inc_20k_25k", 4,
                                                        if_else(w_income == "w_inc_25k_30k", 5,
                                                                if_else(w_income == "w_inc_30k_35k", 6,
                                                                        if_else(w_income == "w_inc_35k_40k", 7,
                                                                                if_else(w_income == "w_inc_40k_45k", 8,
                                                                                        if_else(w_income == "w_inc_45k_50k", 9,
                                                                                                if_else(w_income == "w_inc_50k_60k", 10,
                                                                                                        if_else(w_income == "w_inc_60k_75k", 11,
                                                                                                                if_else(w_income == "w_inc_75k_100k", 12,
                                                                                                                        if_else(w_income == "w_inc_100k_125k", 13,
                                                                                                                                if_else(w_income == "w_inc_125k_150k", 14,
                                                                                                                                        if_else(w_income == "w_inc_150k_200k", 15,
                                                                                                                                                if_else(w_income == "w_inc_200k_more", 16, NA_real_)))))))))))))))))
#CBSA 2007-11
cbsa_w_inc_07_11 <- cbsa_w_inc_07_11 %>%
  mutate(w_income_bin = if_else(w_income == "w_inc_less_10k", 1, 
                                if_else(w_income == "w_inc_10k_15k", 2, 
                                        if_else(w_income == "w_inc_15k_20k", 3,
                                                if_else(w_income == "w_inc_20k_25k", 4,
                                                        if_else(w_income == "w_inc_25k_30k", 5,
                                                                if_else(w_income == "w_inc_30k_35k", 6,
                                                                        if_else(w_income == "w_inc_35k_40k", 7,
                                                                                if_else(w_income == "w_inc_40k_45k", 8,
                                                                                        if_else(w_income == "w_inc_45k_50k", 9,
                                                                                                if_else(w_income == "w_inc_50k_60k", 10,
                                                                                                        if_else(w_income == "w_inc_60k_75k", 11,
                                                                                                                if_else(w_income == "w_inc_75k_100k", 12,
                                                                                                                        if_else(w_income == "w_inc_100k_125k", 13,
                                                                                                                                if_else(w_income == "w_inc_125k_150k", 14,
                                                                                                                                        if_else(w_income == "w_inc_150k_200k", 15,
                                                                                                                                                if_else(w_income == "w_inc_200k_more", 16, NA_real_)))))))))))))))))
####Create rich_white Variable Using case_when: County and CBSA####
##all White HH Above the Overall County/CBSA HH 80th percentile for Income and 50% of HH Where Income = 80th percentile
#County 2007-11
county_rich_white_07_11 <- county_w_inc_07_11 %>%
  mutate(count = as.double(as.character(count))) %>%
  mutate(sum_temp = case_when(w_income_bin > threshold_bin_p80_state ~ count, 
                              w_income_bin == threshold_bin_p80_state ~ count*0.5,
                              TRUE ~ NA_real_))
county_rich_white_07_11 <- county_rich_white_07_11 %>%
  group_by(GISJOIN_county) %>%
  summarise(rich_white = sum(sum_temp, na.rm = TRUE))
#join rich_white variable to ice dataset
county_state_race_07_11 <- county_rich_white_07_11 %>%
  left_join(county_state_race_07_11, county_rich_white_07_11, by = c("GISJOIN_county"))

#CBSA 2007-11
cbsa_rich_white_07_11 <- cbsa_w_inc_07_11 %>%
  mutate(count = as.double(as.character(count))) %>%
  mutate(sum_temp = case_when(w_income_bin > threshold_bin_p80_cbsa ~ count, 
                              w_income_bin == threshold_bin_p80_cbsa ~ count*0.5,
                              TRUE ~ NA_real_))
cbsa_rich_white_07_11 <- cbsa_rich_white_07_11 %>%
  group_by(GISJOIN_CBSA) %>%
  summarise(rich_white = sum(sum_temp, na.rm = TRUE))
#join rich_white variable to ice dataset
cbsa_race_07_11 <- cbsa_rich_white_07_11 %>%
  left_join(cbsa_race_07_11, cbsa_rich_white_07_11, by = c("GISJOIN_CBSA"))

####Income Concentrations at the Extremes (ICE) with Race - County (State)/CBSA Specific Thresholds: County and CBSA####
#County 2007-11
county_state_race_07_11 <- county_state_race_07_11 %>%
  group_by(GISJOIN_county) %>%
  mutate(ice_race = (rich_white - poor_black)/tot_hh) %>%
  ungroup()
summary(county_state_race_07_11$ice_race)

#cbsa 2007-11
cbsa_race_07_11 <- cbsa_race_07_11 %>%
  group_by(GISJOIN_CBSA) %>%
  mutate(ice_race = (rich_white - poor_black)/tot_hh) %>%
  ungroup()
summary(cbsa_race_07_11$ice_race)

####Create ICE Datasets: County and CBSA####
#County 2007-11 ice income only
county_state_ice_income_07_11 <- county_state_07_11 %>%
  select(GISJOIN_county, STATEA, COUNTYA, YEAR, tot_hh, p20_hh_state, p80_hh_state, p20_cutoff_state, p80_cutoff_state,
         p20_static, p80_static, threshold_bin_p20_state, threshold_bin_p80_state, 
         poor_income_static, rich_income_static, static_ice_income, poor, rich, ice_income)
#County race 2007-11 ice
county_state_ice_race_07_11 <- county_state_race_07_11 %>%
  select(GISJOIN_county, poor_black_static, rich_white_static, static_ice_race, poor_black, rich_white, ice_race) 

#CBSA 2007-11 ice income only
cbsa_ice_income_07_11 <- cbsa_07_11 %>%
  select(GISJOIN_CBSA, CBSA, YEAR, tot_hh, p20_hh_cbsa, p80_hh_cbsa, p20_cutoff_cbsa, p80_cutoff_cbsa, p20_static, p80_static, threshold_bin_p20_cbsa, threshold_bin_p80_cbsa,
         poor_income_static, rich_income_static, static_ice_income, poor, rich, ice_income) 
#CBSA race 2007-11 ice 
cbsa_ice_race_07_11 <- cbsa_race_07_11 %>%
  select(GISJOIN_CBSA, poor_black_static, rich_white_static, static_ice_race, poor_black, rich_white, ice_race) 

#join 2 County df's and 2 CBSA df's
#County 2007-11
county_state_ice_07_11 <- left_join(county_state_ice_income_07_11, county_state_ice_race_07_11, by = "GISJOIN_county")

#CBSA 2007-11
cbsa_ice_07_11 <- left_join(cbsa_ice_income_07_11, cbsa_ice_race_07_11, by = "GISJOIN_CBSA")
####Calculate percentage overlap of ZCTAs and CBSAs####
#rename ZCTA GISJOIN
zcta_sf <- zcta_sf %>%
  rename(GISJOIN_ZCTA = GISJOIN)
#rename CBSA GISJOIN
cbsa_sf <- cbsa_sf %>%
  rename(GISJOIN_CBSA = GISJOIN)

#make geometries valid in both ZCTA and CBSA sf's
zcta_sf <- st_make_valid(zcta_sf)
cbsa_sf <- st_make_valid(cbsa_sf)
#complete spatial intersection to determine overlap of ZCTAs with CBSAs
zcta_cbsa_overlap_pct <- st_intersection(zcta_sf, cbsa_sf) %>%
  mutate(intersect_area = st_area(.)) %>% #create new column with shape area
  select(ZCTA5CE10, intersect_area) %>%  #select only ZCTAs and intersect area for join
  st_drop_geometry() #convert sf to a df

#Merge by zcta name
zcta_sf <- merge(zcta_sf, zcta_cbsa_overlap_pct, by = "GISJOIN_ZCTA", all.x = TRUE)

colnames(zcta_cbsa_overlap_pct)
zcta_cbsa_overlap_pct <- zcta_cbsa_overlap_pct %>%
  rename(GISJOIN_ZCTA = GISJOIN) %>%
  select(GISJOIN_ZCTA, intersect_area) %>%
  st_drop_geometry()

#Calculate overlap percentage
zcta_sf <- zcta_sf %>% 
  mutate(p_overlap = as.numeric(intersect_area/Shape_area)*100)

####Complete spatial join ZCTAs and CBSAs####
zcta_cbsa_join <- st_join(zcta_sf, cbsa_sf) #spatial join

####group by zcta and keep those with highest overlap with cbsa####
zcta_cbsa_join <- zcta_cbsa_join %>%
  group_by(GISJOIN_ZCTA) %>%  
  arrange(desc(p_overlap)) %>%
  slice(1) %>%
  ungroup() 

#select only necessary zcta and CBSA variables for join with ICE
colnames(zcta_cbsa_join)
zcta_cbsa_join <- zcta_cbsa_join %>%
  select(GISJOIN_ZCTA, GISJOIN_CBSA, p_overlap) 

####Separating urban and rural ZCTAs####
#if ZCTA overlaps more than 50% with a CBSA, mark as urban
zcta_cbsa_join <- zcta_cbsa_join %>%
  mutate(urban = if_else(p_overlap >= 50, 1, 0))

#rural zip codes
zcta_rural <- zcta_cbsa_join %>%
  filter(is.na(urban) | urban == 0)

#urban zip codes
zcta_urban <- zcta_cbsa_join %>%
  filter(urban == 1) 

####in urban areas, use CBSA income thresholds####
zcta_cbsa_ice_07_11 <- merge(zcta_urban, cbsa_ice_07_11, by = "GISJOIN_CBSA", duplicateGeoms = T)

####in rural areas, use county income ice but with state level 20th and 80th percentile cutoffs####

#spatial join zcta and county shapefiles to get county information for ZCTAs
zcta_county_sf <- st_join(zcta_sf, county_sf) 
#drop rural geometry
zcta_rural <- zcta_rural %>%
  st_drop_geometry()

#select variables for merge with rural zctas
zcta_county_sf <- zcta_county_sf %>%
  dplyr::select(GISJOIN_ZCTA, ZCTA5CE10, STATEFP10, COUNTYFP10)

#merge rural and zcta/county shapefile
zcta_county_rural <- merge(zcta_county_sf, zcta_rural, by = "GISJOIN_ZCTA", duplicateGeoms = T)

####add county ice information to rural ZCTAs####
#change variable names for matching
county_state_ice_07_11 <- county_state_ice_07_11 %>%
  rename(STATEFP10 = STATEA, COUNTYFP10 = COUNTYA)

#merge county_ice and urban zcta files by county identifier
zcta_county_ice_07_11 <- merge(zcta_county_rural, county_state_ice_07_11, by = c("COUNTYFP10", "STATEFP10"), duplicateGeoms = T) %>%
  group_by(GISJOIN_ZCTA) %>%  
  arrange(desc(p_overlap)) %>%
  slice(1) %>%
  ungroup() 

####join ACS ZCTA data to ZCTA ice data####

#join ZCTA ACS data to zcta_county_ice data
zcta_county_ice_acs_07_11 <- merge(zcta_county_ice_07_11, acs_zcta_07_11_df,
                                   by = "GISJOIN_ZCTA", duplicateGeoms = T)

#join ZCTA ACS data to zcta_cbsa_ice data
zcta_cbsa_ice_acs_07_11 <- merge(zcta_cbsa_ice_07_11, acs_zcta_07_11_df,
                                  by = "GISJOIN_ZCTA", duplicateGeoms = T)
