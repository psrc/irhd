#################################################################################
# Title: 2022 update to IRHD, Cleaning WSHFC data and incorporating into existing database
# Author: Eric Clute (with assistance from Jesse Warren, King County)
# Date created: 2022-11-07
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
                    "ProjectName",
                    "PropertyName",
                    "Address",
                    "city",
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
                    "PropertyFunderProgramName",
                    "InServiceDate",
                    "ExpirationYear",
                    "Confidentiality",
                    "ContactName",
                    "ProjectSponsor",
                    "Policy",
                    "PopulationServed",
                    "ProjectType",
                    "Tenure",
                    "Funder",
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

## 6) clean WSHFC data --------------------------------------------------------------------

#filter for just King County properties
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

#################################################################################
#Question for Jesse
#   Removal of HOME units - are we sure they're in the AMI count data?

#remove Number of HOME Units category, which isnt needed and is duplicative of AMI count categories
# WSHFC_cleaned <- WSHFC_cleaned %>% 
#   select(-`Number of HOME Units`)


#select only the entry with the largest total restricted unit count, as there are often multiple entries with different unit counts
WSHFC_cleaned <- WSHFC_cleaned %>% 
  group_by(`Site Name`, Address) %>% 
  slice_max(`Income & Rent Restricted Units`,
            n = 1,
            with_ties = TRUE) %>% 
  distinct()

#################################################################################
#Question for Jesse
#   How to view below output?

#view cases where there are still multiple properties in the dataset
WSHFC_cleaned %>% 
  unique() %>% 
  group_by(`Site Name`, Address) %>% 
  mutate(n = n()) %>% 
  filter(n > 1) %>% 
  arrange(`Project Name`, `Site Name`, Address)

#select only entry with latest expiration date, as there are still some properties with multiple entries and different expiration dates
WSHFC_cleaned <- WSHFC_cleaned %>% 
  group_by(`Site Name`, Address) %>% 
  slice_max(`Project Expiration Date`,
            n = 1,
            with_ties = TRUE) %>% 
  distinct()

#select only entry with earliest in service date, as there are still some properties with multiple entries and different expiration dates
WSHFC_cleaned <- WSHFC_cleaned %>% 
  group_by(`Site Name`, Address) %>% 
  slice_min(`First Credit Year or C of O's`,
            n = 1,
            with_ties = TRUE) %>% 
  distinct()

#check to see if any duplicates
WSHFC_Dups <- WSHFC_cleaned %>% 
  distinct() %>% 
  group_by(`Site Name`, Address) %>% 
  mutate(n = n()) %>% 
  filter(n > 1) %>% 
  arrange(`Project Name`, `Site Name`, Address)

#for entries where there are multiple properties with the same total restricted unit count but different other data, select record that seems correct
WSHFC_cleaned <- WSHFC_cleaned %>% 
  distinct() %>% 
  filter(!(`Project Name` == "Hirabayashi Place" & `Site Name` == "Hirabayashi Place" & `50%` == 57)) %>% #select SOH record, remove Commerce record
  filter(!(`Project Name` == "Passage Point" & `Site Name` == "Passage Point" & `50%` == 0)) #select KC record, remove WSHFC record

#check to see if any duplicates remaining - should be 0
WSHFC_Dups <- WSHFC_cleaned %>% 
  distinct() %>% 
  group_by(`Site Name`, Address) %>% 
  mutate(n = n()) %>% 
  filter(n > 1) %>% 
  arrange(`Project Name`, `Site Name`, Address)

#rename columns and add empty columns for data we dont have
WSHFC_cleaned <- WSHFC_cleaned %>% 
  mutate(DataSourceName = "WSHFC",
         AMI25 = as.numeric(NA),
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
         city = City,
         TotalUnits = `Total Project Units`,
         TotalRestrictedUnits = `Income & Rent Restricted Units`,
         InServiceDate = `First Credit Year or C of O's`,
         ExpirationYear = `Project Expiration Date`,
         AMI20 = `0.2`,
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
         ContactName = `Property Management Org`,
         ProjectSponsor = `Contractor/Owner Org`,
         ProjectType = `Site Type`)

#select only necessary columns and arrange columns
WSHFC_cleaned <- select_and_arrange_columns_function(WSHFC_cleaned) 

#add unique ID that can be used for linking back to original, unedited data before we make manual changes to property names, project names, and addresses
WSHFC_cleaned <- create_unique_linking_ID_function(WSHFC_cleaned,
                                                   "WSHFC")

## 9) make sure files are all ready for joining --------------------------------------------------------------------

#check that all column names are the same
colnames(ARCH_cleaned) == colnames(RHA_cleaned)
colnames(RHA_cleaned) == colnames(WSHFC_cleaned)
colnames(WSHFC_cleaned) == colnames(SHA_cleaned)
colnames(SHA_cleaned) == colnames(SOH_cleaned)
colnames(SOH_cleaned) == colnames(KCHA_cleaned)



#join all dfs together
joined_IRD <- bind_rows(ARCH_cleaned,
                        RHA_cleaned,
                        WSHFC_cleaned,
                        SHA_cleaned,
                        SOH_cleaned,
                        KCHA_cleaned)

## 10) save files --------------------------------------------------------------------

#save N: drive cleaned files location filepath
N_drive_cleaned_files_filepath <- "N:/PME/Homeless Housing and Services/Data Sets/Income Restricted Database/original_data_provider_files/2021/cleaned_files/"

#save cleaned files
write_csv(WSHFC_cleaned, paste0(N_drive_cleaned_files_filepath, "WSHFC_2020_cleaned.csv"))