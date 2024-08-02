# TITLE: Clean 2022 King County data for inclusion in IRHD
# GEOGRAPHIES: King
# DATA SOURCE: King County
# DATE MODIFIED: 08.02.2024
# AUTHOR: Eric Clute

## assumptions -------------------------
library(tidyverse)
library(readxl)
library(magrittr)
library(vroom)

KC_path <- "J:/Projects/IncomeRestrictedHsgDB/2022 vintage/Data/King County/"
KC_vintage_year = "2022"
address_scrpt <- "./address_match.R"
source(address_scrpt)

## Import ----------------------
KC_raw <- read_csv(paste0(KC_path, "King County Income-restricted Housing Database 2023.csv")) # file name says 2023, mistake. Data is 2022 vintage

## Data cleaning ------------------------

# Adjust fields to match IRHD
KC <- KC_raw %>%
 rename("working_id" = "UniqueID",
        "data_source" = "DataSourceName",
        "project_name" = "ProjectName",
        "property_name" = "PropertyName",
        "city" = "GeoCode_City",
        "total_units" = "TotalUnits",
        "total_restricted_units" = "TotalRestrictedUnits",
        "ami_20" = "AMI20",
        "ami_25" = "AMI25",
        "ami_30" = "AMI30",
        "ami_35" = "AMI35",
        "ami_40" = "AMI40",
        "ami_45" = "AMI45",
        "ami_50" = "AMI50",
        "ami_60" = "AMI60",
        "ami_65" = "AMI65",
        "ami_70" = "AMI70",
        "ami_75" = "AMI75",
        "ami_80" = "AMI80",
        "ami_85" = "AMI85",
        "ami_90" = "AMI90",
        "ami_100" = "AMI100",
        "ami_120" = "AMI120",
        "market_rate" = "MarketRate",
        "manager_unit" = "ManagerUnit",
        "bedroom_0" = "Bedroom_0",
        "bedroom_1" = "Bedroom_1",
        "bedroom_2" = "Bedroom_2",
        "bedroom_3" = "Bedroom_3",
        "bedroom_4" = "Bedroom_4",
        "bedroom_5" = "Bedroom_5",
        "bedroom_unknown" = "Bedroom_Unknown",
        "bed_count" = "GroupHomeOrBed",
        "manager" = "ContactName",
        "reported_address" = "Address_standardized",
        "zip" = "GeoCode_Zip",
        "full_address" = "GeoCode_Street",
        "site_type" = "PopulationServed",
        "home" = "HOMEUnits",
        "hits_survey" = "HITS_survey",
        "in_service_date" = "InServiceDate",
        "expiration_date" = "ExpirationYear",
        "tenure" = "Tenure",
        "funding_sources" = "Funder",
        "policy" = "FundingSource", # FundingSource data added to the Policy field. KC may eventually build out these separately
        "policy_detailed" = "DetailedHousingCovenant",
        "confidentiality" = "Confidentiality",
        "property_owner" = "ProjectSponsor")

# Clean
KC$county <- "King"
incorrect_inservicedate <- KC %>% filter(KC$in_service_date > KC_vintage_year)
KC %<>% filter(KC$in_service_date <= KC_vintage_year | is.na(KC$in_service_date)) %>%
        slice(-1678) # Removing duplicate record (Hsh Apartments)

class(KC$in_service_date) = "character"

# Remove fields we don't need (Reconsider each year! Could be worth adding in the future)
KC %<>% select(-c(unique_linking_ID, hits_survey, Address, City, ProjectType, policy_detailed, Policy))

# Create 
KC$full_address <- str_c(KC$full_address,', ',KC$city,', WA ',KC$zip)
KC_cleaned <- KC
KC_cleaned <- add_cleaned_addresses(KC_cleaned)

# Identify and remove duplicated working_id value

duplicates <- KC_cleaned[!is.na(KC_cleaned$working_id) & KC_cleaned$working_id != "", ]
duplicates <- duplicates[duplicated(duplicates$working_id) | duplicated(duplicates$working_id, fromLast = TRUE), ]

## Clean up --------------------------
rm(KC_raw, KC, KC_path, KC_vintage_year, incorrect_inservicedate, duplicates)