#################################################################################
# Title: Reconcile IRHD and new data
# Author: Eric Clute (with assistance from Jesse Warren, King County)
# Date created: 2022-12-07
# Last Updated: 2023-03-02
#################################################################################


## load packages-----------------------------------------------------------------

library(tidyverse)
library(readxl)
library(janitor)


## 1) load data ---------------------------------------------------------------------

#load cleaned 2022 IRHD that has portfolios as of end of 2021
IRHD22raw <- read_csv("J:/Projects/IncomeRestrictedHsgDB/2022_update/Data/1 Working Files/2022 IRHD v3 - ready4reconcilescript.csv")

#load cleaned WSHFC data that has portfolios as of end of 2021
WSHFC22raw <- read_csv("J:/Projects/IncomeRestrictedHsgDB/2022_update/WSHFC/Cleaned Data/WSHFC_2022_cleaned.csv")

#load cleaned KC data that has portfolios as of end of 2021
# KC22raw <- read_csv("J:/Projects/IncomeRestrictedHsgDB/2022_update/Review Files - Received/")


## 2) clean up HOME data in IRHD. Add new fields --------------------------------------------------------------------

# Set field types! The XY coordinates are currently missing data beyond the decimal points




# Create three new HOME fields
IRHD22raw <- IRHD22raw %>%
  mutate(HOMEcity = as.character(NA),
         HOMEcounty = as.character(NA),
         HOMEstate = as.character(NA))

#reorder fields - puts new HOME fields next to the existing HOME field
IRHD22raw <- IRHD22raw[, c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
                           21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
                           41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,
                           61,62,63,64,65,66,67,68,69,78,79,80,70,71,72,73,74,75,76,77)]


## 3) Locate records in WSHFC not in IRHD (likely new records/properties) --------------------------------------------------------------------

newWSHFC22 <- anti_join(WSHFC22raw, IRHD22raw, by = "PropertyID")

## 4) Locate records in IRHD not in WSHFC (No longer in WSHFC data, but once were) --------------------------------------------------------------------

nomatchIRHD22 <- anti_join(IRHD22raw %>% filter(County == "Pierce" | County == "Snohomish" | County == "Kitsap") %>%
                                         filter(DataSource == "WSHFC"),
                           WSHFC22raw, by = "PropertyID")

## 5) Locate records found in both WSHFC and IRHD --------------------------------------------------------------------






## 6) Update fields in IRHD for records found in both WSHFC and IRHD --------------------------------------------------------------------







