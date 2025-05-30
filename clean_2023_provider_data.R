# TITLE: Clean 2023 Data Received from Data Providers
# DATA SOURCE: HASCO, THA, EHA
# DATE CREATED: 05.16.2025
# AUTHOR: Eric Clute

# Assumptions
library(readxl)
library(janitor)
library(data.table)
library(dplyr)
library(magrittr)

vintage_year <- 2023

setwd("C:/Users/eclute/GitHub/irhd")
bha_raw <- "J:/Projects/IncomeRestrictedHsgDB/2023 vintage/Review Files Recieved/IRHD_review_kitsap_bremertonhousingauthority.xlsx"
#hasco_raw <- "J:/Projects/IncomeRestrictedHsgDB/2023 vintage/Review Files Recieved/final_review_SNOHOMISH_hasco.xlsx"

# Pull in data
bha <- read_excel(bha_raw, sheet = 3)
#hasco <- read_excel(hasco_raw, sheet = 3)

# Clean data
bha %<>% mutate(in_service_date = na_if(in_service_date, "Not Applicable")) %>%
         filter(`Reviewer Comments` != 'New Property' | reported_address != '4860 Driftwood Street') %>% # Removed, appears to be a duplicate of existing WSHFC record
         filter(`Reviewer Comments` != 'New Property' | reported_address != '265 Oyster Bay Ave') # Removed, appears to be a duplicate of existing WSHFC record

# Combine
updates_received <- bind_rows(bha)#,hasco)
updates_received %<>% mutate(full_address = str_replace(full_address, ",\\s*(?=\\d{5}$)", " ")) # Remove extra comma before zip code

#Search and remove properties in the wrong vintage year
incorrect_inservicedate <- updates_received %>% filter(updates_received$in_service_date > vintage_year)
updates_received %<>% filter(updates_received$in_service_date <= vintage_year | is.na(updates_received$in_service_date))

# Clean script
rm(bha, incorrect_inservicedate,bha_raw,vintage_year,hasco_raw,hasco)
