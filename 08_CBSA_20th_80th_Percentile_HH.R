# Description -------------------------------------------------------------

## ZCTA Level Index of Concentrations at the Extremes (ICE):
#
# 2007-11 US Census Data
#
## Determine 20th and 80th Percentile Household in CBSA
#
## Determine 20th and 80th Percentile Household Income Cutoff Points in CBSA
#

# Load Packages ----------------------------------------------------------

library(tidyverse)
library(dplyr)
library(here)

# Load Data ---------------------------------------------------------------

#Cleaned CBSA HH income data 2007-11
load(here("data",
          "processed",
          "cbsa_hh_income_07_11.rdata"))

#Cleaned CBSA HH income by race data 2007-11
load(here("data",
          "processed",
          "cbsa_race_hh_income_07_11.rdata"))

# Determine CBSA 20th and 80th Percentile Household -----------------------

#use the 20th & 80th percentile of the total households in each state
#to determine the household number

#CBSA race independent
cbsa_07_11 <- cbsa_07_11 %>%
  mutate(p20_hh_cbsa = tot_hh * 0.2) %>%
  mutate(p80_hh_cbsa = tot_hh * 0.8)

#CBSA
cbsa_race_07_11 <- cbsa_race_07_11 %>%
  mutate(p20_hh_cbsa = tot_hh * 0.2) %>%
  mutate(p80_hh_cbsa = tot_hh * 0.8)

# Determine CBSA 20th and 80th Percentile Household Income Cutoff Points --

##State 20th percentile HH cutoff
#if the 20th percentile household is in given income bucket:
#set the cutoff to the midpoint of that income bucket
cbsa_07_11 <- cbsa_07_11 %>%
  mutate(
    p20_cutoff_cbsa = case_when(
      p20_hh_cbsa < inc_less_10k ~ 5000,
      p20_hh_cbsa >= inc_less_10k &
        p20_hh_cbsa < inc_less_10k + inc_10k_15k ~ 12500,
      p20_hh_cbsa >= inc_less_10k + inc_10k_15k &
        p20_hh_cbsa < inc_less_10k + inc_10k_15k + inc_15k_20k ~ 17500,
      p20_hh_cbsa >= inc_less_10k + inc_10k_15k + inc_15k_20k &
        p20_hh_cbsa < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k ~ 22500,
      p20_hh_cbsa >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k &
        p20_hh_cbsa < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k ~ 27500,
      p20_hh_cbsa >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k &
        p20_hh_cbsa < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k ~ 32500,
      p20_hh_cbsa >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k &
        p20_hh_cbsa < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k ~ 37500,
      p20_hh_cbsa >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k &
        p20_hh_cbsa < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k ~ 42500,
      p20_hh_cbsa >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k &
        p20_hh_cbsa < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k ~ 47500,
      p20_hh_cbsa >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k &
        p20_hh_cbsa < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k ~ 55000,
      p20_hh_cbsa >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k &
        p20_hh_cbsa < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k ~ 67500,
      p20_hh_cbsa >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k &
        p20_hh_cbsa < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k + inc_75k_100k ~ 87500,
      p20_hh_cbsa >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k + inc_75k_100k &
        p20_hh_cbsa < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k + inc_75k_100k + inc_100k_125k ~ 112500,
      p20_hh_cbsa >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k + inc_75k_100k + inc_100k_125k &
        p20_hh_cbsa < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k + inc_75k_100k + inc_100k_125k + inc_125k_150k ~ 137500,
      p20_hh_cbsa >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k + inc_75k_100k + inc_100k_125k + inc_125k_150k &
        p20_hh_cbsa < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k + inc_75k_100k + inc_100k_125k + inc_125k_150k + inc_150k_200k ~ 175000,
      p20_hh_cbsa >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k + inc_75k_100k + inc_100k_125k + inc_125k_150k + inc_150k_200k ~ 200000,
      TRUE ~ NA_real_
    )
  )

##State 80th percentile HH cutoff
##if the 80th percentile household is in given income bucket, set the cutoff to the midpoint of that income bucket
cbsa_07_11 <- cbsa_07_11 %>%
  mutate(
    p80_cutoff_cbsa = case_when(
      p80_hh_cbsa < inc_less_10k ~ 5000,
      p80_hh_cbsa >= inc_less_10k &
        p80_hh_cbsa < inc_less_10k + inc_10k_15k ~ 12500,
      p80_hh_cbsa >= inc_less_10k + inc_10k_15k &
        p80_hh_cbsa < inc_less_10k + inc_10k_15k + inc_15k_20k ~ 17500,
      p80_hh_cbsa >= inc_less_10k + inc_10k_15k + inc_15k_20k &
        p80_hh_cbsa < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k ~ 22500,
      p80_hh_cbsa >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k &
        p80_hh_cbsa < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k ~ 27500,
      p80_hh_cbsa >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k &
        p80_hh_cbsa < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k ~ 32500,
      p80_hh_cbsa >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k &
        p80_hh_cbsa < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k ~ 37500,
      p80_hh_cbsa >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k &
        p80_hh_cbsa < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k ~ 42500,
      p80_hh_cbsa >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k &
        p80_hh_cbsa < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k ~ 47500,
      p80_hh_cbsa >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k &
        p80_hh_cbsa < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k ~ 55000,
      p80_hh_cbsa >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k &
        p80_hh_cbsa < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k ~ 67500,
      p80_hh_cbsa >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k &
        p80_hh_cbsa < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k + inc_75k_100k ~ 87500,
      p80_hh_cbsa >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k + inc_75k_100k &
        p80_hh_cbsa < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k + inc_75k_100k + inc_100k_125k ~ 112500,
      p80_hh_cbsa >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k + inc_75k_100k + inc_100k_125k &
        p80_hh_cbsa < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k + inc_75k_100k + inc_100k_125k + inc_125k_150k ~ 137500,
      p80_hh_cbsa >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k + inc_75k_100k + inc_100k_125k + inc_125k_150k &
        p80_hh_cbsa < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k + inc_75k_100k + inc_100k_125k + inc_125k_150k + inc_150k_200k ~ 175000,
      p80_hh_cbsa >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k + inc_75k_100k + inc_100k_125k + inc_125k_150k + inc_150k_200k ~ 200000,
      TRUE ~ NA_real_
    )
  )


# Create Ascending Income Threshold Bins ----------------------------------

#Each Bin is the Income Bin the 20th and 80th percentile Fell In for Income Alone

#CBSA 20th percentile bin
cbsa_07_11 <- cbsa_07_11 %>%
  mutate(
    threshold_bin_p20_cbsa = case_when(
      p20_cutoff_cbsa == 5000 ~ 1,
      p20_cutoff_cbsa == 12500 ~ 2,
      p20_cutoff_cbsa == 17500 ~ 3,
      p20_cutoff_cbsa == 22500 ~ 4,
      p20_cutoff_cbsa == 27500 ~ 5,
      p20_cutoff_cbsa == 32500 ~ 6,
      p20_cutoff_cbsa == 37500 ~ 7,
      p20_cutoff_cbsa == 42500 ~ 8,
      p20_cutoff_cbsa == 47500 ~ 9,
      p20_cutoff_cbsa == 55000 ~ 10,
      p20_cutoff_cbsa == 67500 ~ 11,
      p20_cutoff_cbsa == 87500 ~ 12,
      p20_cutoff_cbsa == 112500 ~ 13,
      p20_cutoff_cbsa == 137500 ~ 14,
      p20_cutoff_cbsa == 175000 ~ 15,
      p20_cutoff_cbsa == 200000 ~ 16,
      TRUE ~ NA_real_
    )
  )

#State 80th percentile bin
cbsa_07_11 <- cbsa_07_11 %>%
  mutate(
    threshold_bin_p80_cbsa = case_when(
      p80_cutoff_cbsa == 5000 ~ 1,
      p80_cutoff_cbsa == 12500 ~ 2,
      p80_cutoff_cbsa == 17500 ~ 3,
      p80_cutoff_cbsa == 22500 ~ 4,
      p80_cutoff_cbsa == 27500 ~ 5,
      p80_cutoff_cbsa == 32500 ~ 6,
      p80_cutoff_cbsa == 37500 ~ 7,
      p80_cutoff_cbsa == 42500 ~ 8,
      p80_cutoff_cbsa == 47500 ~ 9,
      p80_cutoff_cbsa == 55000 ~ 10,
      p80_cutoff_cbsa == 67500 ~ 11,
      p80_cutoff_cbsa == 87500 ~ 12,
      p80_cutoff_cbsa == 112500 ~ 13,
      p80_cutoff_cbsa == 137500 ~ 14,
      p80_cutoff_cbsa == 175000 ~ 15,
      p80_cutoff_cbsa == 200000 ~ 16,
      TRUE ~ NA_real_
    )
  )


# Join CBSA Threshold Bins to CBSA Race Data ------------------------------

#select variables
#left_join CBSA threshold bins to CBSA HH income data by race
cbsa_race_07_11 <- cbsa_07_11 %>%
  select(GISJOIN_CBSA, p20_cutoff_cbsa, p80_cutoff_cbsa, threshold_bin_p20_cbsa, threshold_bin_p80_cbsa) %>%
  left_join(cbsa_race_07_11, cbsa_07_11, by = c("GISJOIN_CBSA"))

# Save Data ---------------------------------------------------------------

save(cbsa_07_11,  file = "data/processed/cbsa_hh_income_07_11.rdata")
save(cbsa_race_07_11, file = "data/processed/cbsa_race_hh_income_07_11.rdata")
