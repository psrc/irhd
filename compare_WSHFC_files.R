#################################################################################
# Title: Comparing WSHFC 2022 file to 2021 file
# Author: Eric Clute
# Date created: 2023-12-06
# Last Updated: 2023-12-06
#################################################################################

# Set parameters
library(tidyverse)
library(readxl)
library(fuzzyjoin)

setwd("C:/Users/eclute/GitHub/irhd")
reconcile2022 <- "./reconcile_2022_IRHD.R"
wshfc20 <- read_csv("J:/Projects/IncomeRestrictedHsgDB/2020 vintage/WSHFC/Cleaned Data/WSHFC_2020_cleaned.csv")
wshfc21 <- read_csv("J:/Projects/IncomeRestrictedHsgDB/2021 vintage/WSHFC/Cleaned Data/WSHFC_2021_cleaned.csv")
wshfc22 <- read_xlsx("J:/Projects/IncomeRestrictedHsgDB/2022 vintage/Data/WSHFC/PSRC report for 2022.xlsx")
wshfc22 <- wshfc22 %>%
  filter(County == "Snohomish" | County == "Pierce" | County == "Kitsap")

source(reconcile2022)
rm(list=ls()[! ls() %in% c("no_match_irhd", "wshfc20", "wshfc21", "wshfc22")])
no_match_irhd$PropertyID <- as.numeric(no_match_irhd$property_id)

##################################################################
##### Select which WSHFC vintage to compare to no_match_irhd #####
selectedvintage <- wshfc20
##################################################################

# Were these no_match_irhd properties in the 2020 dataset?

stringr::str_detect('Multiple locations', 'Multiple')

selectedvintage %>%
  filter(!stringr::str_detect(Address, 'Multiple')) %>%
  filter(!stringr::str_detect(Address, 'Mutiple')) %>%
  select('ProjectID', 'Owner', 'Address')

df <- selectedvintage %>%
  filter(!stringr::str_detect(Address, 'Multiple')) %>%
  filter(!stringr::str_detect(Address, 'Mutiple')) %>%
  stringdist_join(wshfc21, by=c('ProjectName','PropertyName','Address'), method="jw", max_dist = .1) %>%
  select('ProjectID.x', 'ProjectID.y', 'ProjectName.x', 'ProjectName.y', 'Address.x', 'Address.y')


# Join nomatchIRHD to wshfcXX
no_match_irhd_wshfc_join <- left_join(no_match_irhd,selectedvintage, by = "PropertyID")

select_propid <- grep("PropertyID", names(no_match_irhd_wshfc_join))
select_pname <- grep("ProjectName", names(no_match_irhd_wshfc_join))
select_unit <- grep("TotalUnits", names(no_match_irhd_wshfc_join))

comparison <- no_match_irhd_wshfc_join %>% select(all_of(c(select_propid,select_pname,select_unit)))