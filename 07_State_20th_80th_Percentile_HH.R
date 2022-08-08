# Description -------------------------------------------------------------

## ZCTA Level Index of Concentrations at the Extremes (ICE):
#
# 2007-11 US Census Data
#
## Determine 20th and 80th Percentile Household in State
#
## Determine 20th and 80th Percentile Household Income Cutoff Points in State
#
# Combine State and County HH Income Data
#   HH Income Data by Race & Race Independent

# Load Packages ----------------------------------------------------------

library(tidyverse)
library(dplyr)
library(here)

# Load Data ---------------------------------------------------------------

#Cleaned state HH income data 2007-11
load(here("data",
          "processed",
          "state_hh_income_07_11.rdata"))

#Cleaned county HH income data 2007-11
load(here("data",
          "processed",
          "county_hh_income_07_11.rdata"))

#Cleaned county HH income by race data 2007-11
load(here("data",
          "processed",
          "county_race_hh_income_07_11.rdata"))

# Determine 20th and 80th Percentile Household ----------------------------

#use the 20th & 80th percentile of the total households in each state
#to determine the household number
state_07_11 <- state_07_11 %>%
  mutate(p20_hh_state = tot_hh * 0.2) %>%
  mutate(p80_hh_state = tot_hh * 0.8)


# Determine 20th and 80th Percentile Household Income Cutoff Points -------

##State 20th percentile HH cutoff
#if the 20th percentile household is in given income bucket:
#set the cutoff to the midpoint of that income bucket
state_07_11 <- state_07_11 %>%
  mutate(
    p20_cutoff_state = case_when(
      p20_hh_state < inc_less_10k ~ 5000,
      p20_hh_state >= inc_less_10k &
        p20_hh_state < inc_less_10k + inc_10k_15k ~ 12500,
      p20_hh_state >= inc_less_10k + inc_10k_15k &
        p20_hh_state < inc_less_10k + inc_10k_15k + inc_15k_20k ~ 17500,
      p20_hh_state >= inc_less_10k + inc_10k_15k + inc_15k_20k &
        p20_hh_state < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k ~ 22500,
      p20_hh_state >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k &
        p20_hh_state < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k ~ 27500,
      p20_hh_state >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k &
        p20_hh_state < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k ~ 32500,
      p20_hh_state >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k &
        p20_hh_state < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k ~ 37500,
      p20_hh_state >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k &
        p20_hh_state < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k ~ 42500,
      p20_hh_state >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k &
        p20_hh_state < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k ~ 47500,
      p20_hh_state >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k &
        p20_hh_state < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k ~ 55000,
      p20_hh_state >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k &
        p20_hh_state < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k ~ 67500,
      p20_hh_state >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k &
        p20_hh_state < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k + inc_75k_100k ~ 87500,
      p20_hh_state >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k + inc_75k_100k &
        p20_hh_state < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k + inc_75k_100k + inc_100k_125k ~ 112500,
      p20_hh_state >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k + inc_75k_100k + inc_100k_125k &
        p20_hh_state < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k + inc_75k_100k + inc_100k_125k + inc_125k_150k ~ 137500,
      p20_hh_state >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k + inc_75k_100k + inc_100k_125k + inc_125k_150k &
        p20_hh_state < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k + inc_75k_100k + inc_100k_125k + inc_125k_150k + inc_150k_200k ~ 175000,
      p20_hh_state >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k + inc_75k_100k + inc_100k_125k + inc_125k_150k + inc_150k_200k ~ 200000,
      TRUE ~ NA_real_
    )
  )

##State 80th percentile HH cutoff
##if the 80th percentile household is in given income bucket, set the cutoff to the midpoint of that income bucket
state_07_11 <- state_07_11 %>%
  mutate(
    p80_cutoff_state = case_when(
      p80_hh_state < inc_less_10k ~ 5000,
      p80_hh_state >= inc_less_10k &
        p80_hh_state < inc_less_10k + inc_10k_15k ~ 12500,
      p80_hh_state >= inc_less_10k + inc_10k_15k &
        p80_hh_state < inc_less_10k + inc_10k_15k + inc_15k_20k ~ 17500,
      p80_hh_state >= inc_less_10k + inc_10k_15k + inc_15k_20k &
        p80_hh_state < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k ~ 22500,
      p80_hh_state >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k &
        p80_hh_state < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k ~ 27500,
      p80_hh_state >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k &
        p80_hh_state < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k ~ 32500,
      p80_hh_state >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k &
        p80_hh_state < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k ~ 37500,
      p80_hh_state >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k &
        p80_hh_state < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k ~ 42500,
      p80_hh_state >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k &
        p80_hh_state < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k ~ 47500,
      p80_hh_state >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k &
        p80_hh_state < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k ~ 55000,
      p80_hh_state >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k &
        p80_hh_state < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k ~ 67500,
      p80_hh_state >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k &
        p80_hh_state < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k + inc_75k_100k ~ 87500,
      p80_hh_state >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k + inc_75k_100k &
        p80_hh_state < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k + inc_75k_100k + inc_100k_125k ~ 112500,
      p80_hh_state >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k + inc_75k_100k + inc_100k_125k &
        p80_hh_state < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k + inc_75k_100k + inc_100k_125k + inc_125k_150k ~ 137500,
      p80_hh_state >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k + inc_75k_100k + inc_100k_125k + inc_125k_150k &
        p80_hh_state < inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k + inc_75k_100k + inc_100k_125k + inc_125k_150k + inc_150k_200k ~ 175000,
      p80_hh_state >= inc_less_10k + inc_10k_15k + inc_15k_20k + inc_20k_25k + inc_25k_30k + inc_30k_35k + inc_35k_40k + inc_40k_45k + inc_45k_50k + inc_50k_60k + inc_60k_75k + inc_75k_100k + inc_100k_125k + inc_125k_150k + inc_150k_200k ~ 200000,
      TRUE ~ NA_real_
    )
  )


# Create Ascending Income Threshold Bins ----------------------------------

#Each Bin is the Income Bin the 20th and 80th percentile Fell In for Income Alone

#State 20th percentile bin
state_07_11 <- state_07_11 %>%
  mutate(
    threshold_bin_p20_state = case_when(
      p20_cutoff_state == 5000 ~ 1,
      p20_cutoff_state == 12500 ~ 2,
      p20_cutoff_state == 17500 ~ 3,
      p20_cutoff_state == 22500 ~ 4,
      p20_cutoff_state == 27500 ~ 5,
      p20_cutoff_state == 32500 ~ 6,
      p20_cutoff_state == 37500 ~ 7,
      p20_cutoff_state == 42500 ~ 8,
      p20_cutoff_state == 47500 ~ 9,
      p20_cutoff_state == 55000 ~ 10,
      p20_cutoff_state == 67500 ~ 11,
      p20_cutoff_state == 87500 ~ 12,
      p20_cutoff_state == 112500 ~ 13,
      p20_cutoff_state == 137500 ~ 14,
      p20_cutoff_state == 175000 ~ 15,
      p20_cutoff_state == 200000 ~ 16,
      TRUE ~ NA_real_
    )
  )

#State 80th percentile bin
state_07_11 <- state_07_11 %>%
  mutate(
    threshold_bin_p80_state = case_when(
      p80_cutoff_state == 5000 ~ 1,
      p80_cutoff_state == 12500 ~ 2,
      p80_cutoff_state == 17500 ~ 3,
      p80_cutoff_state == 22500 ~ 4,
      p80_cutoff_state == 27500 ~ 5,
      p80_cutoff_state == 32500 ~ 6,
      p80_cutoff_state == 37500 ~ 7,
      p80_cutoff_state == 42500 ~ 8,
      p80_cutoff_state == 47500 ~ 9,
      p80_cutoff_state == 55000 ~ 10,
      p80_cutoff_state == 67500 ~ 11,
      p80_cutoff_state == 87500 ~ 12,
      p80_cutoff_state == 112500 ~ 13,
      p80_cutoff_state == 137500 ~ 14,
      p80_cutoff_state == 175000 ~ 15,
      p80_cutoff_state == 200000 ~ 16,
      TRUE ~ NA_real_
    )
  )


# Combine County and State HH Income Data ---------------------------------

#select variables
#left_join county HH income data to state data, matching by state
county_state_07_11 <- state_07_11 %>%
  select(
    STATE,
    STATEA,
    p20_hh_state,
    p80_hh_state,
    p20_cutoff_state,
    p80_cutoff_state,
    threshold_bin_p20_state,
    threshold_bin_p80_state
  ) %>%
  left_join(county_07_11, state_07_11, by = "STATE")


# Combine County Race and State HH Income Data ----------------------------

#select variables
#left_join county HH income by race data to state data, matching by state
county_state_race_07_11 <- state_07_11 %>%
  select(
    STATE,
    STATEA,
    p20_hh_state,
    p80_hh_state,
    p20_cutoff_state,
    p80_cutoff_state,
    threshold_bin_p20_state,
    threshold_bin_p80_state
  ) %>%
  left_join(county_race_07_11, state_07_11, by = "STATE")

# Add Total Households to County HH Income Data by Race -------------------

county_state_race_07_11 <- county_state_07_11 %>%
  select(tot_hh, STATE, COUNTY) %>%
  left_join(county_state_race_07_11, county_07_11, by = c("STATE", "COUNTY"))

# Save Data ---------------------------------------------------------------

save(county_state_07_11,  file = "data/processed/county_state_hh_income_07_11.rdata")
save(county_state_race_07_11, file = "data/processed/county_state_race_hh_income_07_11.rdata")
