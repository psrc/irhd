# TITLE: Clean 2023 King County data for inclusion in IRHD
# GEOGRAPHIES: King
# DATA SOURCE: King County
# DATE CREATED: 08.17.2025
# AUTHOR: Eric Clute

## assumptions -------------------------
library(tidyverse)
library(readxl)
library(magrittr)
library(vroom)

KC_path <- "J:/Projects/IncomeRestrictedHsgDB/2023 vintage/Data/"
KC_vintage_year = "2023"
address_scrpt <- "./address_match.R"
source(address_scrpt)

## Import ----------------------
KC_raw <- read_csv(paste0(KC_path, "PSRC_King County Income Restricted Housing Database 2024.csv")) # file name says 2024, but this is a mistake. Data is 2023 vintage

## Data cleaning ------------------------

# Adjust field names to match IRHD
KC <- KC_raw %>%
 rename("kc_id" = "uniqueID_final",
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
        "ami_110" = "AMI110",
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
        "reported_address" = "Address",
        "zip" = "GeoCode_Zip",
        "full_address" = "GeoCode_Street",
        "site_type" = "PopulationServed",
        "home" = "HOMEUnits",
        "hits_survey" = "HITS_survey",
        "in_service_date" = "InServiceDate",
        "expiration_date" = "ExpirationYear",
        "tenure" = "Tenure",
        "funding_sources" = "Funder",
        "policy" = "FundingSource", # FundingSource data added to Policy field. KC may eventually build out separately
        "policy_detailed" = "DetailedHousingCovenant",
        "confidentiality" = "Confidentiality",
        "property_owner" = "ProjectSponsor",
        "contractexpired_flag" = "ExpiredProperty_flag")

class(KC$in_service_date) = "character"

# Create new fields & clean address field
KC_cleaned <- KC %>% mutate(county = "King",
                            contractnew_flag = 0,
                            working_id = ifelse(str_starts(kc_id, "SH_"), str_sub(kc_id, 1, 7), NA),
                            full_address = str_c(full_address,', ',city,', WA ',zip)) %>%
                     select(-c(hits_survey, City, ProjectType, policy_detailed, Policy)) %>%
                     add_cleaned_addresses()

# Identify incorrect service year - remove from data
incorrect_inservicedate <- KC %>% filter(KC$in_service_date > KC_vintage_year)
KC %<>% filter(KC$in_service_date <= KC_vintage_year | is.na(KC$in_service_date))

# Identify duplicate working_id or kc_id value
duplicates <- KC_cleaned[!is.na(KC_cleaned$kc_id) & KC_cleaned$kc_id != "", ]
duplicates <- duplicates[duplicated(duplicates$kc_id) | duplicated(duplicates$kc_id, fromLast = TRUE), ]

duplicates <- KC_cleaned[!is.na(KC_cleaned$working_id) & KC_cleaned$working_id != "", ]
duplicates <- duplicates[duplicated(duplicates$working_id) | duplicated(duplicates$working_id, fromLast = TRUE), ]

## Identify properties with expired contracts and those that resigned --------------------------
kc_ended_contract <- KC_cleaned %>% filter(contractexpired_flag == "1")
kc_signed_new_contract <- KC_cleaned %>%
  filter((duplicated(project_name) | duplicated(project_name, fromLast = TRUE)) &
          project_name %in% kc_ended_contract$project_name)

# For properties that re-signed, update data
kc_signed_new_contract %<>%
  group_by(project_name) %>%
  fill(working_id, .direction = "downup") %>%
  ungroup() %<>%
  mutate(contractnew_flag = if_else(contractexpired_flag == 0, 1, contractnew_flag))

KC_cleaned <- KC_cleaned %>% # Remove any kc_id that appears in kc_signed_new_contract
  filter(!(kc_id %in% kc_signed_new_contract$kc_id)) %>% # Add back the replacements where contractexpired_flag == 0
  bind_rows(kc_signed_new_contract %>%
            filter(contractexpired_flag == 0) %>%
            mutate(contractnew_flag = 1)
  )

## Clean up --------------------------
rm(KC_raw, KC, KC_path, KC_vintage_year, incorrect_inservicedate, duplicates)
