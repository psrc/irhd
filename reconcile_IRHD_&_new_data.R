#################################################################################
# Title: Reconcile IRHD and new data
# Author: Eric Clute (with assistance from Jesse Warren, King County)
# Date created: 2022-12-07
# Last Updated: 2023-05-19
#################################################################################

`%not_in%` <- Negate(`%in%`)

## load packages-----------------------------------------------------------------

library(tidyverse)
library(tidyr)
library(readxl)
library(data.table)
library(magrittr)

IRHD_path <- "J:/Projects/IncomeRestrictedHsgDB/2021 vintage/Data/1 Working Files/2021 IRHD v3 - ready4reconcilescript.csv"
WSHFC_path <- "J:/Projects/IncomeRestrictedHsgDB/2021 vintage/WSHFC/Cleaned Data/WSHFC_2021_cleaned.csv"

## 1) load data ---------------------------------------------------------------------

#load cleaned 2021 IRHD that has portfolios as of end of 2021
IRHD_raw <- fread(IRHD_path)

# borrow datatype characterization from IRHD to apply to identical columns in WSHFC data
irhd_colClasses = sapply(IRHD_raw, class)
names(irhd_colClasses) <- colnames(IRHD_raw)
WSHFC_cols = colnames(read.csv(WSHFC_path, nrows=1))
wshfc_colClasses <- irhd_colClasses %>% .[names(.) %in% WSHFC_cols]

#load cleaned WSHFC data that has portfolios as of end of 2021; apply datatypes to match
WSHFC_raw <- fread(WSHFC_path, colClasses=wshfc_colClasses)

#load cleaned KC data that has portfolios as of end of 2021
# KC21raw <- read_csv("J:/Projects/IncomeRestrictedHsgDB/2021 vintage/Review Files - Received/")


## 2) clean up fields in IRHD, limit to 3 counties, add/remove fields --------------------------------------------------------------------

IRHD_raw %<>% .[County %in% c("Pierce", "Snohomish", "Kitsap")]                                    # King county handled separately

IRHD_raw %<>% .[, grep("\\d+-\\d+%", colnames(.)):=NULL]                                           # Remove summary AMI fields

# Create three new HOME fields
IRHD_raw %<>% mutate(HOMEcity = NA_character_,                                                     # Add fields to match WSHFC
                     HOMEcounty = NA_character_,
                     HOMEstate = NA_character_, 
                     .after = HOME)

# Manage duplicate records in IRHD
IRHD_raw %<>%  filter(!(UniqueID == "SH_7002")) %>% # Remove this record, keep SH_6053
               filter(!(UniqueID == "SH_6516")) # Remove this record, keep SH_6517

## 3) clean up some variables in WSHFC before joining --------------------------------------------------------------------

IRHD_raw$Manager[IRHD_raw$Manager == 'HASCO'] <- 'Snohomish County Housing Authority'
IRHD_raw$Owner[IRHD_raw$Owner == 'HASCO'] <- 'Snohomish County Housing Authority'

IRHD_raw$Manager[IRHD_raw$Manager == 'Low Income Housing Institute'] <- 'Low Income Housing Institute (LIHI)'
IRHD_raw$Owner[IRHD_raw$Owner == 'Low Income Housing Institute'] <- 'Low Income Housing Institute (LIHI)'

WSHFC_raw$Address[WSHFC_raw$Address == '1724 E. 44th'] <- '1724 E 44th Street'
WSHFC_raw$Address[WSHFC_raw$Address == '9225 Bayshore Drive NW'] <- '9225 Bay Shore Dr NW'
WSHFC_raw$Address[WSHFC_raw$Address == '9239 Bayshore Dr NW'] <- '9239 Bay Shore Dr NW'

## 4) Locate records in WSHFC not in IRHD (likely new records/properties) --------------------------------------------------------------------

newWSHFC <- anti_join(WSHFC_raw, IRHD_raw, by = "PropertyID")

## 5) Locate records in IRHD not in WSHFC (No longer in WSHFC data, but once were?) --------------------------------------------------------------------

nomatchIRHD <- anti_join(IRHD_raw, WSHFC_raw, by = "PropertyID")
nomatchIRHD <- nomatchIRHD %>% drop_na(PropertyID)

# setwd("J:/Projects/IncomeRestrictedHsgDB/2021 vintage/WSHFC/Raw Data")
# write.csv(nomatchIRHD, "nomatchIRHD.csv", row.names=FALSE)

## 6) Identify matched records in IRHD and WSHFC --------------------------------------------------------------------

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
                 'AMI20','AMI25','AMI30','AMI35','AMI40','AMI45','AMI50','AMI60','AMI65','AMI70','AMI75','AMI80','AMI85','AMI90','AMI100',
                 'MarketRate',
                 'ManagerUnit',
                 'Bedroom_0','Bedroom_1','Bedroom_2','Bedroom_3','Bedroom_4','Bedroom_5','Bedroom_Unknown',
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
                 'AMI20','AMI25','AMI30','AMI35','AMI40','AMI45','AMI50','AMI60','AMI65','AMI70','AMI75','AMI80','AMI85','AMI90','AMI100',
                 'MarketRate',
                 'ManagerUnit',
                 'Bedroom_0','Bedroom_1','Bedroom_2','Bedroom_3','Bedroom_4','Bedroom_5','Bedroom_Unknown',
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

## 7) Identify which rows will be updated with new WSHFC data, or keep existing data --------------------------------------------------------------------

# Create field to indicate which variable to use
long_compare$select <- ""
long_compare <- tibble::rowid_to_column(long_compare, "ID")

# Subset 1) select records with no data in the IRHD - we will take new data from WSHFC
subset1 <- long_compare %>% subset((is.na(variable_value.x)| variable_value.x == ""), select = c(ID, PropertyID, variable_class,variable_value.x,variable_value.y,match, select))
subset1$select <- subset1$variable_value.y
long_compare <- anti_join(long_compare, subset1, by=c("ID"="ID")) # remove from long_compare
selected <- subset1
rm(subset1)

# Subset 2) Below fields - select WHSFC data
subset2 <- long_compare %>% subset((variable_class == "InServiceDate" |
                                    variable_class == "Manager"|
                                    variable_class == "Owner"|
                                    variable_class == "ProjectID"|
                                    variable_class == "Disabled"|
                                    variable_class == "Homeless"|
                                    variable_class == "Senior"|
                                    variable_class == "BedCount"|  
                                    variable_class == "PropertyName"|
                                    variable_class == "Site_Type"|
                                    variable_class == "FundingSources"|
                                    variable_class == "HOMEcity"|
                                    variable_class == "HOMEcounty"|
                                    variable_class == "HOMEstate"|
                                    variable_class == "ProjectName"), select = c(ID, PropertyID, variable_class,variable_value.x,variable_value.y,match, select))
subset2$select <- subset2$variable_value.y
long_compare <- anti_join(long_compare, subset2, by=c("ID"="ID")) # remove from long_compare
selected <- rbind(selected, subset2)
rm(subset2)

# Subset 3) select addresses that have "multiple" in the field - use IRHD address
subset3 <- long_compare %>% subset(str_detect(long_compare$variable_value.y, str_c("Mu")), select = c(ID, PropertyID, variable_class,variable_value.x,variable_value.y,match, select))
subset3$select <- subset3$variable_value.x
long_compare <- anti_join(long_compare, subset3, by=c("ID"="ID"))# remove from long_compare
selected <- rbind(selected, subset3)
rm(subset3)

# Subset 4) select all AMI/Unit count/Bedroom size data, identify small numeric changes
subset4 <- long_compare %>% subset((variable_class == "TotalUnits" |
                                    variable_class == "TotalRestrictedUnits"|
                                    variable_class == "AMI20"|
                                    variable_class == "AMI25"|
                                    variable_class == "AMI30"|
                                    variable_class == "AMI35"|
                                    variable_class == "AMI40"|
                                    variable_class == "AMI45"|
                                    variable_class == "AMI50"|
                                    variable_class == "AMI60"|
                                    variable_class == "AMI65"|
                                    variable_class == "AMI70"|
                                    variable_class == "AMI75"|
                                    variable_class == "AMI80"|
                                    variable_class == "AMI85"|
                                    variable_class == "AMI90"|
                                    variable_class == "AMI100"|
                                    variable_class == "MarketRate"|
                                    variable_class == "ManagerUnit"|
                                    variable_class == "Bedroom_0"|
                                    variable_class == "Bedroom_1"|
                                    variable_class == "Bedroom_2"|
                                    variable_class == "Bedroom_3"|
                                    variable_class == "Bedroom_4"|
                                    variable_class == "Bedroom_5"|
                                    variable_class == "Bedroom_Unknown"|
                                    variable_class == "BedCount"), select = c(ID, PropertyID, variable_class,variable_value.x,variable_value.y,match, select))

# Create formula for calculating difference between numeric values
subset4_sum <- subset4 %>% group_by(PropertyID) %>%
  summarize(sum.x=sum(as.numeric(variable_value.x)),
            sum.y=sum(as.numeric(variable_value.y)))

# abs function - absolute value of the percentage difference
subset4_sum$diff <- abs((subset4_sum$sum.x-subset4_sum$sum.y)/subset4_sum$sum.x)

# join back to subset4 table, so each row of data now has the percentage difference
subset4 <- merge(subset4, subset4_sum, by = "PropertyID")
rm(subset4_sum)

# Rows with "diff" of 12% or less will be selected - we want the WSHFC data
subset4$select <- ifelse(subset4$diff <= "0.12", subset4$variable_value.y, "")

# Rows where the sum.y is 0, we keep the sum.x data (if WSHFC data is 0, we keep IRHD data)
subset4$select <- ifelse(subset4$sum.y == "0", subset4$variable_value.x, subset4$select)

# Remove "diff" of greater than 12% from subset4
subset4 <- subset4 %>% subset(!(select == ""), select = c(ID, PropertyID, variable_class,variable_value.x,variable_value.y,match, select, sum.x, sum.y, diff))
long_compare <- anti_join(long_compare, subset4, by=c("ID"="ID")) # remove from long_compare

subset4 <- subset4[, -c(8,9,10)]
selected <- rbind(selected, subset4)
rm(subset4)

# Subset 5) Address matching


# Subset 6) Various manual selections of the remaining rows



## 8) Take "selected" data and update IRHD records, create IRHD_clean table --------------------------------------------------------------------

# Transform "selected" for updating existing IRHD
selected <- selected %>% pivot_wider(id_cols = c('PropertyID'), names_from = 'variable_class', values_from = 'select') %>%
  setDT()

class(selected$ProjectID) = "numeric"
class(selected$TotalUnits) = "numeric"
class(selected$TotalRestrictedUnits) = "numeric"
class(selected$ZIP) = "numeric"
class(selected$AMI30) = "numeric"
class(selected$AMI35) = "numeric"
class(selected$AMI40) = "numeric"
class(selected$AMI45) = "numeric"
class(selected$AMI50) = "numeric"
class(selected$AMI60) = "numeric"
class(selected$AMI65) = "numeric"
class(selected$AMI80) = "numeric"
class(selected$Bedroom_1) = "numeric"
class(selected$Bedroom_2) = "numeric"
class(selected$Bedroom_3) = "numeric"
class(selected$Bedroom_4) = "numeric"
class(selected$Bedroom_Unknown) = "numeric"
class(selected$BedCount) = "numeric"
class(selected$Senior) = "numeric"
class(selected$Homeless) = "numeric"
class(selected$Disabled) = "numeric"

# Create new clean IRHD file
IRHD_clean <- copy(IRHD_raw)

# Update records as determined by the "selected" dataframe
shared_fields <- intersect(names(selected), names(IRHD_clean))                                     # fields in common                                                   
dupes <- IRHD_clean[duplicated(PropertyID), cbind(.SD[1], number=.N), by=PropertyID] %>%           # duplicates (to exclude)
  pull(UniqueID) 
blankfill <- IRHD_clean %>%                                                                        # create IRHD data that matches fields from selected
  .[!is.na(PropertyID) & UniqueID %not_in% (dupes), (colnames(.) %in% shared_fields), with=FALSE]  # include only common records, no duplicate keys
selected %<>% rows_patch(blankfill, by="PropertyID", unmatched="ignore")                           # replace NA in `selected` with values from `IRHD_clean`
IRHD_clean %<>% .[selected, (shared_fields):=mget(paste0("i.", shared_fields)), on=.(PropertyID)]  # carry over all matching variables from selected
rm(dupes, blankfill, shared_fields, long_IRHD, long_WSHFC, wshfc_colClasses, WSHFC_cols, irhd_colClasses, long_compare) # Clean up
