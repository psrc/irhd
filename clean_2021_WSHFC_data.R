#################################################################################
# Title: 2021 IRHD, Cleaning WSHFC data and incorporating into existing database
# Author: Eric Clute (with assistance from Jesse Warren, King County)
# Date created: 2022-11-30
# Last Updated: 2023-04-06
#################################################################################

## load packages-----------------------------------------------------------------
library(tidyverse)
library(readxl)
library(janitor)

## 1) load data ---------------------------------------------------------------------

J_drive_raw_files_filepath <- "J:/Projects/IncomeRestrictedHsgDB/2021 vintage/WSHFC/Raw Data/"

original_WSHFC_raw <- read_xlsx(paste0(J_drive_raw_files_filepath, "PSRC Report_WSHFC_12-2021.xlsx"))

## 2) create functions --------------------------------------------------------------------

#create function to select and arrange columns needed for joining
select_and_arrange_columns_function <- function(df){
  df <- df %>%
    select(any_of(c("DataSource",
                    "ProjectID",
                    "ProjectName",
                    "PropertyID",
                    "PropertyName",
                    "Owner",
                    "Manager",
                    "InServiceDate",
                    "ExpirationDate",
                    "Address",
                    "City",
                    "ZIP",
                    "County",
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
                    "BedCount",
                    "Site_Type",
                    "HOMEcity",
                    "HOMEcounty",
                    "HOMEstate",
                    "Confidentiality",
                    "ContactName",
                    "ProjectSponsor",
                    "Policy",
                    "Senior",
                    "Disabled",
                    "Farmworker",
                    "Homeless",
                    "Large Household (+4 pp)",
                    "Transitional",
                    "Veterans",
                    "FundingSources",
                    "Tenure")))
}

## 3) clean WSHFC data --------------------------------------------------------------------

# ------- DATA FILTER #1 ------- filter by county, create/modify fields
WSHFC_cleaned <- original_WSHFC_raw %>%
  filter(County == "Snohomish" | County == "Pierce" | County == "Kitsap")

#create grouped funder column
WSHFC_cleaned <- WSHFC_cleaned %>% 
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

# ------- DATA FILTER #5 ------- Remove any records with an InServiceDate of 2022, this update is through 2021

WSHFC_cleaned <- WSHFC_cleaned %>%
  filter(`First Credit Year or C of O's` < "2022")

#rename columns and add empty columns for data we dont have
WSHFC_cleaned <- WSHFC_cleaned %>% 
  mutate(DataSource = as.character(NA),
         AMI25 = as.numeric(NA),
         AMI75 = as.numeric(NA),
         AMI85 = as.numeric(NA),
         AMI90 = as.numeric(NA),
         AMI100 = as.numeric(NA),
         MarketRate = as.numeric(NA),
         ManagerUnit = as.numeric(NA),
         Confidentiality = as.character(NA),
         Policy = as.character(NA),
         Tenure = as.character(NA)) %>% 
  rename(ProjectID = `ProjectKey`,
         ProjectName = `Project Name`,
         PropertyID = `SiteKey`,
         PropertyName = `Site Name`,
         Owner = `Contractor/Owner Org`,
         Manager = `Property Management Org`,
         City = `City`,
         TotalUnits = `Total Project Units`,
         TotalRestrictedUnits = `Income & Rent Restricted Units`,
         InServiceDate = `First Credit Year or C of O's`,
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
         Bedroom_0 = `STUDIO`,
         Bedroom_1 = `1 BR`,
         Bedroom_2 = `2 BR`,
         Bedroom_3 = `3 BR`,
         Bedroom_4 = `4 BR`,
         Bedroom_5 = `5 BR`,
         Bedroom_Unknown = `Unknown`,
         BedCount = `GROUP HOME/BED`,
         HOMEcity = `HOME City`,
         HOMEcounty = `HOME County`,
         HOMEstate = `HOME State`,
         FundingSources = `Funder`,
         ExpirationDate = `Project Expiration Date`,
         LargeHousehold4plus = `Large Household (4+ pp)`,
         Site_Type = `Site Type`,
         Senior = `Elderly`,
         Disabled = `Persons with Disabilities`,
         ZIP = `Zip`)

#select only necessary columns and arrange columns
WSHFC_cleaned <- select_and_arrange_columns_function(WSHFC_cleaned) 

#set DataSource field
WSHFC_cleaned$DataSource = "WSHFC"

## 4) save files --------------------------------------------------------------------

#save N: drive cleaned files location filepath
J_drive_cleaned_files_filepath <- "J:/Projects/IncomeRestrictedHsgDB/2021 vintage/WSHFC/Cleaned Data/"

#save cleaned files
write_csv(WSHFC_cleaned, paste0(J_drive_cleaned_files_filepath, "WSHFC_2021_cleaned.csv"))
