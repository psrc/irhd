# TITLE: Clean 2024 King County data for inclusion in IRHD
# GEOGRAPHIES: King
# DATA SOURCE: King County
# DATE CREATED: 03.18.2026
# AUTHOR: Eric Clute

## assumptions -------------------------
library(tidyverse)
library(readxl)
library(magrittr)
library(vroom)

KC_path <- "J:/Projects/IncomeRestrictedHsgDB/2024 vintage/Data/"
KC_vintage_year = "2024"
address_scrpt <- "./address_match.R"
source(address_scrpt)

## Import ----------------------
KC_raw <- read_csv(paste0(KC_path, "King_County_Income_Restricted_Housing_Database_2024.csv"))

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
        "manager" = "ProjectContact",
        "reported_address" = "Address",
        "zip" = "GeoCode_Zip",
        "full_address" = "GeoCode_Street",
        "site_type" = "PopulationServed",
#        "home" = "HOMEUnits",
#        "hits_survey" = "HITS_survey",
        "in_service_date" = "InServiceDate",
        "tenure" = "Tenure",
        "funding_sources" = "Funder",
#        "confidentiality" = "Confidentiality",
        "property_owner" = "ProjectSponsor",
        "contractexpired_flag" = "ExpiredProperty_flag")

class(KC$in_service_date) = "character"
class(KC$HousingCovenant_A) = "character"
class(KC$HousingCovenant_B) = "character"
class(KC$HousingCovenant_C) = "character"

# Create new fields & clean address field
KC_cleaned <- KC %>%
  mutate(
    county = "King",
    contractnew_flag = 0,
    working_id = ifelse(str_starts(kc_id, "SH_"), str_sub(kc_id, 1, 7), NA),
    full_address = str_c(full_address, ', ', city, ', WA ', zip)
  ) %>%
  # Combine all sources/housing covenant data together, separate by ";"
  mutate(across(
    c(FundingSource, HousingCovenant_A, HousingCovenant_B, HousingCovenant_C),
    ~ na_if(str_trim(.), ""))) %>%
  unite(col = "policy", FundingSource, HousingCovenant_A, HousingCovenant_B, HousingCovenant_C,
    sep = "; ", na.rm = TRUE, remove = TRUE) %>%
  
  # Clean up
  mutate(policy = na_if(policy, "")) %>%
  select(-c(City, ProjectType)) %>%
  add_cleaned_addresses()

# -------- This section not working quite yet. Need help combining all Expiration data together

# TEST <- KC_cleaned |>
#   mutate(across(starts_with("Expiration_"), as.character)) |>
#   
#   rowwise() |>
#   mutate(
#     expiration_date = {
#       vals <- c_across(starts_with("Expiration_"))
#       
#       # remove NAs and blanks
#       vals <- vals[!is.na(vals) & vals != ""]
#       
#       if(length(vals) == 0) {
#         NA_character_  # empty row
#       } else if(any(str_detect(vals, regex("life|indefinite", ignore_case = TRUE)))) {
#         "Life/Indefinite"
#       } else {
#         # extract 4-digit years
#         years <- as.integer(str_extract(vals, "\\b\\d{4}\\b"))
#         years <- years[!is.na(years)]
#         if(length(years) == 0) NA_character_ else as.character(max(years))
#       }
#     }
#   ) |>
#   ungroup()

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
