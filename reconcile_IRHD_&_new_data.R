#################################################################################
# Title: Reconcile IRHD and new data
# Author: Eric Clute (with assistance from Jesse Warren, King County)
# Date created: 2022-12-07
# Last Updated: 2023-04-13
#################################################################################


## load packages-----------------------------------------------------------------

library(tidyverse)
library(tidyr)
library(readxl)

## 1) load data ---------------------------------------------------------------------

#load cleaned 2021 IRHD that has portfolios as of end of 2021
IRHD_raw <- read_csv("J:/Projects/IncomeRestrictedHsgDB/2021 vintage/Data/1 Working Files/2021 IRHD v3 - ready4reconcilescript.csv")

#load cleaned WSHFC data that has portfolios as of end of 2021
WSHFC_raw <- read_csv("J:/Projects/IncomeRestrictedHsgDB/2021 vintage/WSHFC/Cleaned Data/WSHFC_2021_cleaned.csv")

#load cleaned KC data that has portfolios as of end of 2021
# KC21raw <- read_csv("J:/Projects/IncomeRestrictedHsgDB/2021 vintage/Review Files - Received/")


## 2) clean up fields in IRHD, limit to 3 counties, add/remove fields --------------------------------------------------------------------

# Create three new HOME fields
IRHD_raw <- IRHD_raw %>%
  mutate(HOMEcity = as.character(NA),
         HOMEcounty = as.character(NA),
         HOMEstate = as.character(NA))

# reorder fields - puts new HOME fields next to the existing HOME field
IRHD_raw <- IRHD_raw[, c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
                           21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
                           41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,
                           61,62,63,64,65,66,67,68,69,78,79,80,70,71,72,73,74,75,76,77)]

# Remove summary AMI fields (we can do this all via a script from now on)

IRHD_raw <- IRHD_raw[, -c(40,41,42,43,44)]

# Clean up various fields for matching with WSHFC
IRHD_raw$Manager[IRHD_raw$Manager == 'HASCO'] <- 'Snohomish County Housing Authority'
IRHD_raw$Owner[IRHD_raw$Owner == 'HASCO'] <- 'Snohomish County Housing Authority'

IRHD_raw$Manager[IRHD_raw$Manager == 'Low Income Housing Institute'] <- 'Low Income Housing Institute (LIHI)'
IRHD_raw$Owner[IRHD_raw$Owner == 'Low Income Housing Institute'] <- 'Low Income Housing Institute (LIHI)'

# Limit to just Pierce, Snohomish, and Kitsap
IRHD_raw  <- IRHD_raw %>% filter(County == "Pierce" | County == "Snohomish" | County == "Kitsap")


## 3) Locate records in WSHFC not in IRHD (likely new records/properties) --------------------------------------------------------------------

newWSHFC <- anti_join(WSHFC_raw, IRHD_raw, by = "PropertyID")

## 4) Locate records in IRHD not in WSHFC (No longer in WSHFC data, but once were?) --------------------------------------------------------------------

nomatchIRHD <- anti_join(IRHD_raw, WSHFC_raw, by = "PropertyID")
nomatchIRHD <- nomatchIRHD %>% drop_na(PropertyID)

# setwd("J:/Projects/IncomeRestrictedHsgDB/2021 vintage/WSHFC/Raw Data")
# write.csv(nomatchIRHD, "nomatchIRHD.csv", row.names=FALSE)

## 5) Identify matched records in IRHD and WSHFC --------------------------------------------------------------------

# Pivot the IRHD_raw data to make it long and thin
long_IRHD <- IRHD_raw %>%
  pivot_longer(c('ProjectID',
                 'ProjectName',
                 'PropertyName',
                 'Owner',
                 'Manager',
                 'InServiceDate',
                 'ExpirationDate',
                 'Address',
                 'City',
                 'ZIP',
                 'County',
                 'TotalUnits',
                 'TotalRestrictedUnits',
                 'AMI20',
                 'AMI25',
                 'AMI30',
                 'AMI35',
                 'AMI40',
                 'AMI45',
                 'AMI50',
                 'AMI60',
                 'AMI65',
                 'AMI70',
                 'AMI75',
                 'AMI80',
                 'AMI85',
                 'AMI90',
                 'AMI100',
                 'MarketRate',
                 'ManagerUnit',
                 'Bedroom_0',
                 'Bedroom_1',
                 'Bedroom_2',
                 'Bedroom_3',
                 'Bedroom_4',
                 'Bedroom_5',
                 'Bedroom_Unknown',
                 'BedCount',
                 'Site_Type',
                 'HOMEcity',
                 'HOMEcounty',
                 'HOMEstate',
                 'Confidentiality',
                 'Policy',
                 'Senior',
                 'Disabled',
                 'Homeless',
                 'Transitional',
                 'Veterans',
                 'FundingSources',
                 'Tenure'),
               names_to='variable_class',
               values_to='variable_value',
               values_transform = list(variable_value=as.character))

# Remove some fields that we don't need here
long_IRHD <- long_IRHD[c(5,25,26)]

# Pivot the mocked-up data to make it long and thin
long_WSHFC <- WSHFC_raw %>%
  pivot_longer(c('ProjectID',
                 'ProjectName',
                 'PropertyName',
                 'Owner',
                 'Manager',
                 'InServiceDate',
                 'ExpirationDate',
                 'Address',
                 'City',
                 'ZIP',
                 'County',
                 'TotalUnits',
                 'TotalRestrictedUnits',
                 'AMI20',
                 'AMI25',
                 'AMI30',
                 'AMI35',
                 'AMI40',
                 'AMI45',
                 'AMI50',
                 'AMI60',
                 'AMI65',
                 'AMI70',
                 'AMI75',
                 'AMI80',
                 'AMI85',
                 'AMI90',
                 'AMI100',
                 'MarketRate',
                 'ManagerUnit',
                 'Bedroom_0',
                 'Bedroom_1',
                 'Bedroom_2',
                 'Bedroom_3',
                 'Bedroom_4',
                 'Bedroom_5',
                 'Bedroom_Unknown',
                 'BedCount',
                 'Site_Type',
                 'HOMEcity',
                 'HOMEcounty',
                 'HOMEstate',
                 'Confidentiality',
                 'Policy',
                 'Senior',
                 'Disabled',
                 'Homeless',
                 'Transitional',
                 'Veterans',
                 'FundingSources',
                 'Tenure'),
               names_to='variable_class',
               values_to='variable_value',
               values_transform = list(variable_value=as.character))

# Remove some fields that we don't need here
long_WSHFC <- long_WSHFC[c(2,4,5)]

# Compare the two data sets in long form to identify values that have been changed
long_compare <- long_IRHD %>%
  inner_join(long_WSHFC, by=c('PropertyID', 'variable_class')) %>%
  mutate("match" = ifelse(mapply(identical, variable_value.x, variable_value.y), "YES", "NO")) %>%
  filter(match == "NO") %>%
  drop_na(variable_value.y)

## 6) Identify which fields will be updated with new WSHFC data, which keep existing data --------------------------------------------------------------------

## This section under development - is another approach better??

# Blanks in the IRHD will are to be filled with WSHFC data
long_compare <- long_compare %>%
  mutate("select" = ifelse(is.na(variable_value.x), "y", ""))