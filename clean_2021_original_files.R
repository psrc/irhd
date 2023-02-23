#################################################################################
# Title: 2022 update to IRHD, Cleaning WSHFC data and incorporating into existing database
# Author: Eric Clute (with assistance from Jesse Warren, King County)
# Date created: 2022-11-30
# Last Updated: 2023-02-22
#################################################################################

## load packages-----------------------------------------------------------------
library(tidyverse)
library(readxl)
library(janitor)

## 1) load data ---------------------------------------------------------------------

J_drive_raw_files_filepath <- "J:/Projects/IncomeRestrictedHsgDB/2022_update/WSHFC/Raw Data/"

original_WSHFC_raw <- read_xlsx(paste0(J_drive_raw_files_filepath, "PSRC Report_WSHFC_12-2021.xlsx"))

## 2) create functions --------------------------------------------------------------------

#create function to select and arrange columns needed for joining
select_and_arrange_columns_function <- function(df){
  df <- df %>%
    select(any_of(c("DataSourceName",
                    "ProjectKey",
                    "ProjectName",
                    "SiteKey",
                    "PropertyName",
                    "Address",
                    "City",
                    "Zip",
                    "County",
                    "Funder",
                    "TotalUnits",
                    "TotalRestrictedUnits",
                    "AMI20",
                    "AMI25",
                    "AMI30",
                    "AMI35",
                    "AMI40",
                    "AMI45",
                    "AMI50",
                    "AMI60",
                    "AMI65",
                    "AMI70",
                    "AMI75",
                    "AMI80",
                    "AMI85",
                    "AMI90",
                    "AMI100",
                    "MarketRate",
                    "ManagerUnit",
                    "Bedroom_0",
                    "Bedroom_1",
                    "Bedroom_2",
                    "Bedroom_3",
                    "Bedroom_4",
                    "Bedroom_5",
                    "Bedroom_Unknown",
                    "GroupHomeOrBed",
                    "HOMEcity",
                    "HOMEcounty",
                    "HOMEstate",
                    "PropertyFunderProgramName",
                    "InServiceDate",
                    "ExpirationYear",
                    "Confidentiality",
                    "ContactName",
                    "ProjectSponsor",
                    "Policy",
                    "Elderly",
                    "Persons with Disabilities",
                    "Farmworker",
                    "Homeless",
                    "Large Household (+4 pp)",
                    "Transitional",
                    "Veterans",
                    "ProjectType",
                    "Tenure",
                    "FundingSource")))
}

#create function to add unique linking ID that can be used for linking back to original, unedited data before we make manual changes to property names, project names, and addresses

create_unique_linking_ID_function <- function(df, funder_name){
  df <- df %>%
    ungroup() %>%
    mutate(unique_linking_ID = paste0(funder_name, "_", row_number())) %>%
    select(unique_linking_ID,
           everything())
}

## 3) clean WSHFC data --------------------------------------------------------------------

# ------- DATA FILTER #1 ------- filter by county, create/modify fields
WSHFC_cleaned <- original_WSHFC_raw %>%
  filter(County == "Snohomish" | County == "Pierce" | County == "Kitsap")

#group all 0 bedroom columns
WSHFC_cleaned <- WSHFC_cleaned %>% 
  mutate(Bedroom_0 = SRO + STUDIO) %>% 
  select(-SRO,
         -STUDIO)

#create grouped funder column
WSHFC_cleaned <- WSHFC_cleaned %>% 
  group_by(`Site Name`, Address) %>% 
  mutate(Funder = paste(sort(unique(Funder)), collapse = ", "))

###############
#Home units included in AMI categories - Discuss with Carol - Ask WSHFC about duplications in HOME columns

#remove Number of HOME Units category, which isnt needed and is duplicative of AMI count categories
# WSHFC_cleaned <- WSHFC_cleaned %>% 
#   select(-`Number of HOME Units`)


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

# ------- DATA FILTER #4 ------- select only entry with earliest in service date
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
  arrange(`Project Name`, `Site Name`, Address) %>%
  view()

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
  arrange(`Project Name`, `Site Name`, Address)# %>%
 # view()

#rename columns and add empty columns for data we dont have
WSHFC_cleaned <- WSHFC_cleaned %>% 
  mutate(AMI25 = as.numeric(NA),
         AMI75 = as.numeric(NA),
         AMI85 = as.numeric(NA),
         AMI90 = as.numeric(NA),
         AMI100 = as.numeric(NA),
         MarketRate = as.numeric(NA),
         ManagerUnit = as.numeric(NA),
         PropertyFunderProgramName = as.character(NA),
         Confidentiality = as.character(NA),
         Policy = as.character(NA),
         Tenure = as.character(NA),
         FundingSource = as.character(NA)) %>% 
  rename(ProjectName = `Project Name`,
         PropertyName = `Site Name`,
         City = City,
         TotalUnits = `Total Project Units`,
         TotalRestrictedUnits = `Income & Rent Restricted Units`,
         InServiceDate = `First Credit Year or C of O's`,
         ExpirationYear = `Project Expiration Date`,
         AMI20 = `20%`,
         AMI30 = `30%`,
         AMI35 = `35%`,
         AMI40 = `40%`,
         AMI45 = `45%`,
         AMI50 = `50%`,
         AMI60 = `60%`,
         AMI65 = `65%`,
         AMI70 = `70%`,
         AMI80 = `80%`,
         Bedroom_1 = `1 BR`,
         Bedroom_2 = `2 BR`,
         Bedroom_3 = `3 BR`,
         Bedroom_4 = `4 BR`,
         Bedroom_5 = `5 BR`,
         Bedroom_Unknown = `Unknown`,
         GroupHomeOrBed = `GROUP HOME/BED`,
         HOMEcity = `HOME City`,
         HOMEcounty = `HOME County`,
         HOMEstate = `HOME State`,
         ContactName = `Property Management Org`,
         ProjectSponsor = `Contractor/Owner Org`,
         ProjectType = `Site Type`)

#select only necessary columns and arrange columns
WSHFC_cleaned <- select_and_arrange_columns_function(WSHFC_cleaned) 

#add unique ID that can be used for linking back to original, unedited data before we make manual changes to property names, project names, and addresses
WSHFC_cleaned <- create_unique_linking_ID_function(WSHFC_cleaned,
                                                   "WSHFC")

## 4) save files --------------------------------------------------------------------

#save N: drive cleaned files location filepath
J_drive_cleaned_files_filepath <- "J:/Projects/IncomeRestrictedHsgDB/2022_update/WSHFC/Cleaned Data/"

#save cleaned files
write_csv(WSHFC_cleaned, paste0(J_drive_cleaned_files_filepath, "WSHFC_2021_cleaned.csv"))