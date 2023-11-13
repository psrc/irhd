#################################################################################
# Title: Cleaning 2022 WSHFC data
# Author: Eric Clute (with assistance from Jesse Warren, King County)
# Date created: 2022-11-30
# Last Updated: 2023-11-13
#################################################################################

## load packages-----------------------------------------------------------------
library(tidyverse)
library(readxl)
library(janitor)

## 1) Set variables ---------------------------------------------------------------------

WSHFC_path <- "J:/Projects/IncomeRestrictedHsgDB/2022 vintage/Data/WSHFC/"
WSHFC_raw <- read_xlsx(paste0(WSHFC_path, "PSRC report for 2022.xlsx"))
WSHFC_clean_file <- "WSHFC_2022_cleaned.csv"
vintage_year = "2022"
address_scrpt <- "./address_match.R"

remotes::install_github("slu-openGIS/postmastr")
source(address_scrpt)

## 2) function --------------------------------------------------------------------

#create function to select and arrange columns needed for joining
select_and_arrange_columns_function <- function(df){
  df <- df %>%
    select(any_of(c("data_source",
                    "project_id",
                    "project_name",
                    "property_id",
                    "property_name",
                    "property_owner",
                    "manager",
                    "in_service_date",
                    "expiration_date",
                    "reported_address",
                    "city",
                    "zip",
                    "county",
                    "total_units",
                    "total_restricted_units",
                    "ami_20",
                    "ami_25",
                    "ami_30",
                    "ami_35",
                    "ami_40",
                    "ami_45",
                    "ami_50",
                    "ami_60",
                    "ami_65",
                    "ami_70",
                    "ami_75",
                    "ami_80",
                    "ami_85",
                    "ami_90",
                    "ami_100",
                    "ami_120",
                    "market_rate",
                    "manager_unit",
                    "bedroom_0",
                    "bedroom_1",
                    "bedroom_2",
                    "bedroom_3",
                    "bedroom_4",
                    "bedroom_5",
                    "bedroom_unknown",
                    "bed_count",
                    "site_type",
                    "home",
                    "HOMEcity",
                    "HOMEcounty",
                    "HOMEstate",
                    "confidentiality",
                    "policy",
                    "senior",
                    "disabled",
                    "farmworker",
                    "homeless",
                    "large_household",
                    "transitional",
                    "veterans",
                    "funding_sources",
                    "tenure")))
}

## 3) clean WSHFC data --------------------------------------------------------------------

# ------- DATA FILTER #1 ------- filter by county, create/modify fields
WSHFC_cleaned <- WSHFC_raw %>%
  filter(County == "Snohomish" | County == "Pierce" | County == "Kitsap")

#create grouped funder column
WSHFC_cleaned %<>%
  group_by(`Site Name`, Address) %>%
  mutate(Funder = paste(sort(unique(Funder)), collapse = ","))

# ------- DATA FILTER #2 ------- select entry with the largest total restricted unit count
WSHFC_cleaned <- WSHFC_cleaned %>%
  group_by(`Site Name`, Address) %>%
  slice_max(`Income & Rent Restricted Units`,
            n = 1,
            with_ties = TRUE) %>%
  distinct()

#check for duplicates
WSHFC_cleaned %>%
  unique() %>%
  group_by(`Site Name`, Address) %>%
  mutate(n = n()) %>%
  filter(n > 1) %>%
  arrange(`Project Name`, `Site Name`, Address) # %>%
#  view()

# ------- DATA FILTER #3 ------- select only entry with latest expiration date
WSHFC_cleaned <- WSHFC_cleaned %>%
  group_by(`Site Name`, Address) %>%
  slice_max(`Project Expiration Date`,
            n = 1,
            with_ties = TRUE) %>%
  distinct()

#check for duplicates
WSHFC_cleaned %>%
  unique() %>%
  group_by(`Site Name`, Address) %>%
  mutate(n = n()) %>%
  filter(n > 1) %>%
  arrange(`Project Name`, `Site Name`, Address)# %>%
#  view()

# ------- DATA FILTER #4 ------- select entry with earliest in service date
WSHFC_cleaned <- WSHFC_cleaned %>%
  group_by(`Site Name`, Address) %>%
  slice_min(`First Credit Year or C of O's`,
            n = 1,
            with_ties = TRUE) %>%
  distinct()

#check for duplicates
WSHFC_cleaned %>%
  distinct() %>%
  group_by(`Site Name`, Address) %>%
  mutate(n = n()) %>%
  filter(n > 1) %>%
  arrange(`Project Name`, `Site Name`, Address)# %>%
 # view()

# ------- DATA FILTER #4 ------- for entries where there are multiple properties with the same total restricted unit count but different other data, select record that seems correct
WSHFC_cleaned <- WSHFC_cleaned %>%
  distinct() %>%
  filter(!(`Project Name` == "Annobee Apartments, The" & `Site Name` == "Annobee Apartments, The" & `80%` == 43)) %>% #remove this record, keep record with pop served & deeper affordability
  filter(!(`Project Name` == "Catalina Apartments" & `Site Name` == "Catalina Apartments" & `40%` == 32)) %>% #remove this record, keep record with pop served & deeper affordability
  filter(!(`Project Name` == "Maternity Shelter (Youth Emergency Shelter (YES) North)" & `Site Name` == "Youth Emergency Shelter (YES) North" & `50%` == 8)) #remove this record, keep record with deeper affordability

#check to see if any duplicates remaining - should be 0
WSHFC_cleaned %>%
  distinct() %>%
  group_by(`Site Name`, Address) %>%
  mutate(n = n()) %>%
  filter(n > 1) %>%
  arrange(`Project Name`, `Site Name`, Address) %>%
  view()

# ------- DATA FILTER #5 ------- Small edits/checks

#Filter by InServiceDate - select only records in this vintage year or earlier
WSHFC_cleaned <- WSHFC_cleaned %>%
  filter(`First Credit Year or C of O's` <= vintage_year)

#Consolidate SRO and STUDIO into one column
WSHFC_cleaned$STUDIO = WSHFC_cleaned$SRO + WSHFC_cleaned$STUDIO

## 4) clean up field names --------------------------------------------------------------------

#rename columns and add empty columns for data we dont have
WSHFC_cleaned <- WSHFC_cleaned %>%
  mutate(DataSource = as.character(NA),
         ami_25 = as.numeric("0"),
         ami_75 = as.numeric("0"),
         ami_85 = as.numeric("0"),
         ami_90 = as.numeric("0"),
         ami_100 = as.numeric("0"),
         ami_120 = as.numeric("0"),
         market_rate = as.numeric("0"),
         manager_unit = as.numeric("0"),
         confidentiality = as.character(NA),
         policy = as.character(NA),
         tenure = as.character(NA)) %>%
  rename(project_id = `ProjectKey`,
         project_name = `Project Name`,
         property_id = `SiteKey`,
         property_name = `Site Name`,
         property_owner = `Contractor/Owner Org`,
         manager = `Property Management Org`,
         city = `City`,
         total_units = `Total Project Units`,
         total_restricted_units = `Income & Rent Restricted Units`,
         in_service_date = `First Credit Year or C of O's`,
         ami_20 = `20%`,
         ami_30 = `30%`,
         ami_35 = `35%`,
         ami_40 = `40%`,
         ami_45 = `45%`,
         ami_50 = `50%`,
         ami_60 = `60%`,
         ami_65 = `65%`,
         ami_70 = `70%`,
         ami_80 = `80%`,
         bedroom_0 = `STUDIO`,
         bedroom_1 = `1 BR`,
         bedroom_2 = `2 BR`,
         bedroom_3 = `3 BR`,
         bedroom_4 = `4 BR`,
         bedroom_5 = `5 BR`,
         bedroom_unknown = `Unknown`,
         bed_count = `GROUP HOME/BED`,
         home = `Number of HOME Units`,
         HOMEcity = `HOME City`,
         HOMEcounty = `HOME County`,
         HOMEstate = `HOME State`,
         funding_sources = `Funder`,
         expiration_date = `Project Expiration Date`,
         large_household = `Large Household (4+ pp)`,
         site_type = `Site Type`,
         senior = `Elderly`,
         disabled = `Persons with Disabilities`,
         reported_address = `Address`,
         county = `County`,
         farmworker = `Farmworker`,
         homeless = `Homeless`,
         transitional = `Transitional`,
         data_source = `DataSource`,
         veterans = `Veterans`,
         zip = `Zip`)

#select only necessary columns and arrange columns
WSHFC_cleaned <- select_and_arrange_columns_function(WSHFC_cleaned)

#set DataSource field
WSHFC_cleaned$data_source = "WSHFC"

WSHFC_cleaned$reported_address[WSHFC_cleaned$reported_address == '1724 E. 44th'] <- '1724 E 44th Street'
WSHFC_cleaned$reported_address[WSHFC_cleaned$reported_address == '9225 Bayshore Drive NW'] <- '9225 Bay Shore Dr NW'
WSHFC_cleaned$reported_address[WSHFC_cleaned$reported_address == '9239 Bayshore Dr NW'] <- '9239 Bay Shore Dr NW'

# Clean address field for matching
WSHFC_cleaned$full_address <- str_c(WSHFC_cleaned$reported_address,', ',WSHFC_cleaned$city,', WA, ',WSHFC_cleaned$zip)
WSHFC_cleaned_test <- add_cleaned_addresses(WSHFC_cleaned)

## 5) save file --------------------------------------------------------------------

#save cleaned file
write_csv(WSHFC_cleaned, paste0(WSHFC_path, WSHFC_clean_file))
