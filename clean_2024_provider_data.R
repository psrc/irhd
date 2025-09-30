# TITLE: Clean 2024 Data Received from Data Providers
# DATA SOURCE: HASCO, THA, EHA
# DATE CREATED: 05.16.2025
# AUTHOR: Eric Clute

# NOTES: In mid 2025, PSRC received data from providers for the 2023/2024 vintages at once, hence why this script references 2023 files on the J drive. 

# Assumptions
library(readxl)
library(janitor)
library(data.table)
library(dplyr)
library(magrittr)
library(stringr)

vintage_year <- 2024

setwd("C:/Users/eclute/GitHub/irhd")
bha_raw <- "J:/Projects/IncomeRestrictedHsgDB/2023 vintage/Review Files Recieved/IRHD_review_kitsap_bremertonhousingauthority.xlsx"
hasco_raw <- "J:/Projects/IncomeRestrictedHsgDB/2023 vintage/Review Files Recieved/IRHD_review_snohomish.xlsx"

# Pull in data
bha <- read_excel(bha_raw, sheet = 3)
hasco <- read_excel(hasco_raw, sheet = 3)

## Clean data and Prep for Join ----------------------
# BHA data
bha %<>% mutate(in_service_date = na_if(in_service_date, "Not Applicable")) %>%
         filter(`Reviewer Comments` != 'New Property' | reported_address != '4860 Driftwood Street') %>% # Removed, appears to be a duplicate of existing WSHFC record
         filter(`Reviewer Comments` != 'New Property' | reported_address != '265 Oyster Bay Ave') # Removed, appears to be a duplicate of existing WSHFC record

# HASCO data
hasco$homeless <- as.character(hasco$homeless)
#hasco %<>% select(-c(dup_key, general_notes, farmworker))

## Join together ----------------------
# Compare datasets, which columns match, which do not

cols_bha <- colnames(bha)
cols_hasco <- colnames(hasco)
matching_cols <- intersect(cols_bha, cols_hasco) # Matching column names

unique_bha <- setdiff(cols_bha, cols_hasco)
unique_hasco <- setdiff(cols_hasco, cols_bha)
list(matching = matching_cols, only_in_bha = unique_bha, only_in_hasco = unique_hasco)

# Clean up fields
bha %<>% select(-c(irhd_property_id))
hasco %<>% select(-c(dup_key, general_notes, farmworker))

# Join tables
updates_received <- bind_rows(bha,hasco)
updates_received %<>% mutate(full_address = str_replace(full_address, ",\\s*(?=\\d{5}$)", " ")) # Remove extra comma before zip code

## Final Cleaning ----------------------
#Search and remove properties in the wrong vintage year
incorrect_inservicedate <- updates_received %>% filter(updates_received$in_service_date > vintage_year)
updates_received %<>% filter(updates_received$in_service_date <= vintage_year | is.na(updates_received$in_service_date))

# Clean script
rm(bha, incorrect_inservicedate,bha_raw,hasco_raw,hasco, cols_bha, cols_hasco, matching_cols, unique_bha, unique_hasco)