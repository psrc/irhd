#################################################################################
# Title: Comparing WSHFC 2021 file to 2020 file (and exploring the WSHFC 2020 raw file)
# Author: Eric Clute
# Date created: 2023-04-06
# Last Updated: 2023-04-11
#################################################################################


## load packages-----------------------------------------------------------------

library(tidyverse)
library(readxl)

## 1) Run the "reconcile_IRHD_&_new_data.R" ---------------------------------------------------------------------

rm(newWSHFC)
rm(WSHFC_raw)

# Pull cleaned data
wshfc20 <- read_csv("J:/Projects/IncomeRestrictedHsgDB/2021 vintage/WSHFC/Cleaned Data/WSHFC_2020_cleaned.csv")
wshfc21 <- read_csv("J:/Projects/IncomeRestrictedHsgDB/2021 vintage/WSHFC/Cleaned Data/WSHFC_2021_cleaned.csv")

# Join nomatchIRHD to wshfc20 - were these records included in the 2020 data? They are currently missing from 2021 wshfc
nomatchIRHD_wshfc20_join <- left_join(nomatchIRHD,wshfc20, by = "PropertyID")

select_propid <- grep("PropertyID", names(nomatchIRHD_wshfc20_join))
select_pname <- grep("ProjectName", names(nomatchIRHD_wshfc20_join))
select_unit <- grep("TotalUnits", names(nomatchIRHD_wshfc20_join))

comparison <- nomatchIRHD_wshfc20_join %>% select(all_of(c(select_propid,select_pname,select_unit)))

## It appears that all 100 records were included in the 2020 WSHFC file, but not the 2021 file

## 2) Flag these 100 records in the WSHFC 2020 raw file for export to Melissa ---------------------------------------------------------------------

# Pull raw WBARS data
wshfc20_raw <- read.csv("J:/Projects/IncomeRestrictedHsgDB/2020 vintage/Updates/PSRC WBARS Report_12-31-2020.csv")

# Join nomatchIRHD to wshfc20_raw
wshfc20_raw <- rename(wshfc20_raw, PropertyID = SiteKey)
nomatchIRHD_wshfc20raw_join <- left_join(nomatchIRHD,wshfc20_raw, by = "PropertyID", multiple = "first")

## Again, it appears that all 100 records were included in the 2020 WSHFC raw file. WSHFC is claiming otherwise. Will advise.