# TITLE: Clean 2022 King County data for inclusion in IRHD
# GEOGRAPHIES: King
# DATA SOURCE: King County
# DATE MODIFIED: 11.16.2023
# AUTHOR: Eric Clute

## assumptions -------------------------
library(tidyverse)
library(readxl)

KC_path <- "J:/Projects/IncomeRestrictedHsgDB/2022 vintage/Data/King County/"
KC_raw <- read_xlsx(paste0(KC_path, "PSRC report for 2022.xlsx"))
KC_vintage_year = "2022"
address_scrpt <- "./address_match.R"

remotes::install_github("slu-openGIS/postmastr")
source(address_scrpt)

## Data cleaning ------------------------
KC <- KC_raw
KC$county <- "King"
KC %<>% filter(KC$in_service_date <= KC_vintage_year | is.na(KC$in_service_date))

# Remove fields we don't need (Policy field is blank, data currently stored in "FundingSource" - This may change!! Watch next year)
KC %<>% select(-c(unique_linking_ID,HITS_survey,GeoCode_Street,GeoCode_City,ProjectType,Policy))

# Adjust fields to match IRHD
KC <- KC %>%
 rename("data_source" = "DataSourceName",
        "bed_count" = "GroupHomeOrBed",
        "zip" = "GeoCode_Zip",
        "full_address" = "address_standardized",
        "expiration_date" = "ExpirationYear",
        "property_owner" = "ProjectSponsor",
        "manager" = "ContactName",
        "site_type" = "PopulationServed",
        "funding_sources" = "Funder",
        "HOME" = "HOMEUnits",
        "policy" = "FundingSource")
KC$cleaned_address <- str_c(KC$full_address,', ',KC$city,', WA, ',KC$zip)

KC_cleaned <- KC
KC_cleaned <- add_cleaned_addresses(KC_cleaned)

# Identify and remove duplicated working_id value
anyDuplicated(KC_cleaned, by="working_id") #check for any duplicates - hopefully 0!
# dups <- filter(KC_cleaned, working_id == "SH_5215")
# KC_cleaned[1222,1]<-"SH_7234"
# rm(dups)

## Clean up --------------------------
rm(KC_raw, KC_path, KC_vintage_year)