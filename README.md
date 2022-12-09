This code quantifies the Index of Concentration at the Extremes (ICE) at the Zip Code Tabulation Area (ZCTA) level. 

ICE is defined as the total number of households in a given geographic area above the 80th percentile household income, minus the total number of households in a given geographic area below the 20th percentile household income, divided by the total number of households in that geographic area 
(ICE_i = (A_i - P_i)/T_i).

This code uses US Census Data from the 2007-11 5 Year Census grouping. All shapefiles are from the 2010 US Census.

ICE is calculated in 3 ways:
  1) Using static household income thresholds for the 20th and 80th percentile incomes from 2009 US Census Data 
  2) Using dynamic household income thresholds for the 20th and 80th percentile incomes from 2009 US Census Data, all households independent of race
  3) Using dynamic household income thresholds for the 20th and 80th percentile incomes from 2009 US Census Data, Black and White only households

ICE was computed at the core-based statistical area (CBSA) and county levels. At the county level, the 20th and 80th percentile household was assigned by state. At CBSA level, the 20th and 80th percentile was assigned per each CBSA.

ZCTAs were divided by urban and rural status. If a ZCTA overlapped 50% or more with a CBSA, it was labelled as "urban." If a ZCTA overlapped less than 50% with a CBSA, the ZCTA was labelled as "rural".

ICE in rural ZCTAs was assigned at the county level. ICE in urban ZCTAs was assigned at the CBSA level.

