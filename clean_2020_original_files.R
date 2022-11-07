#################################################################################
# Title: Clean 2020 original files from funders
# Author: Jesse Warren
# Date created: 2021-12-02
# ----------------------------------------------
# Overview: Clean 2020 original files we received from funders to get them prepared for joining
# Steps: 
#    1.
#    2.
#    3.
#    4.
#################################################################################

#outstanding questions:
#-categorize SROs as group homes or 0 bedrooms?


## load packages-----------------------------------------------------------------
library(tidyverse)
library(readxl)
library(janitor)



## 1) load data ---------------------------------------------------------------------


#save N: drive raw files location filepath
N_drive_raw_files_filepath <- "N:/PME/Homeless Housing and Services/Data Sets/Income Restricted Database/original_data_provider_files/2021/raw_files/"

#load original files from reporting agencies
original_ARCH_raw <- read_xlsx(paste0(N_drive_raw_files_filepath, "ARCH_Housing_Units-Recorded_Regulatory_Agreements-KC_dashboard.xlsx"))
original_RHA_raw <- read_xlsx(paste0(N_drive_raw_files_filepath, "RHA Subsidized Housing 2021.xlsx"),
                              skip = 1)
original_SHA_raw <- read_xlsx(paste0(N_drive_raw_files_filepath, "KC Income-Restricted Housing db 2020 Update.xlsx"))
original_WSHFC_raw <- read_xlsx(paste0(N_drive_raw_files_filepath, "PSRC WBARS Report_12-31-2020.xlsx"))
original_SOH_raw <- read_xlsx(paste0(N_drive_raw_files_filepath, "City of Seattle Rent and Income Restricted Housing as of 2020-12-31 for KING COUNTY 2021-10-22.xlsx"))
original_KCHA_raw_2020 <- read_xlsx(paste0(N_drive_raw_files_filepath, "KCHAPSRCDatabase-2020 Update.xlsx"),
                                    skip = 4,
                                    col_types = c("text",
                                                  "text",
                                                  "text",
                                                  "text",
                                                  "text",
                                                  "numeric",
                                                  "numeric",
                                                  "text",
                                                  "text",
                                                  "numeric",
                                                  "text",
                                                  "numeric",
                                                  "numeric",
                                                  "numeric",
                                                  "numeric",
                                                  "numeric",
                                                  "numeric",
                                                  "numeric",
                                                  "numeric",
                                                  "text",
                                                  "numeric",
                                                  "text",
                                                  "text",
                                                  "text",
                                                  "text",
                                                  "text",
                                                  "text",
                                                  "text",
                                                  "text",
                                                  "numeric",
                                                  "numeric",
                                                  "numeric",
                                                  "numeric",
                                                  "numeric",
                                                  "numeric",
                                                  "logical",
                                                  "numeric",
                                                  "numeric",
                                                  "numeric",
                                                  "numeric",
                                                  "text",
                                                  "text",
                                                  "text",
                                                  "text",
                                                  "text",
                                                  "text",
                                                  "text",
                                                  "text",
                                                  "text",
                                                  "text",
                                                  "text",
                                                  "text",
                                                  "numeric",
                                                  "numeric",
                                                  "numeric",
                                                  "numeric",
                                                  "numeric"))
original_KCHA_raw_2019 <- read_csv("N:/PME/Homeless Housing and Services/Data Sets/Income Restricted Database/original_data_provider_files/2020/Subsidized Housing Files Renamed/KingCountyHousingAuthority_KCHA.csv")


## 2) create functions to be used on all original files --------------------------------------------------------------------

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





## 3) clean ARCH data --------------------------------------------------------------------

#create wide df of AMI unit counts
ARCH_AMIs <- original_ARCH_raw %>% 
  group_by(`ARCH Project Name`,
           `Current Name`,
           `Current Jurisdiction`,
           `Current Tenure`,
           `Placed In Service Year`,
           `Qualified Project Period`,
           `Target Demographics`,
           Affordability) %>% 
  summarise(units = sum(Units)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = Affordability,
              values_from = units) %>% 
  rename_at(vars(ends_with("AMI")), funs(str_replace(., " ", ""))) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  rename(MarketRate = Market)

#replace Bed with NA's, as its unclear what size bedroom these units are
ARCH_bedroom_sizes <- original_ARCH_raw %>% 
  mutate(`Unit Type [BRs]` = if_else(`Unit Type [BRs]` == "Bed", as.character(NA), `Unit Type [BRs]`)) 
  
#create wide df of bedroom size unit counts
ARCH_bedroom_sizes <- ARCH_bedroom_sizes %>% 
  group_by(`ARCH Project Name`,
           `Current Name`,
           `Current Jurisdiction`,
           `Current Tenure`,
           `Placed In Service Year`,
           `Qualified Project Period`,
           `Target Demographics`,
           `Unit Type [BRs]`) %>% 
  summarise(units = sum(Units)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = `Unit Type [BRs]`,
              values_from = units) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0))

#rename columns
ARCH_bedroom_sizes <- ARCH_bedroom_sizes %>% 
  rename(Bedroom_1 = `1-BR`,
         Bedroom_2 = `2-BR`,
         Bedroom_3 = `3-BR`,
         Bedroom_4 = `4-BR`,
         Bedroom_5 = `5-BR`,
         Bedroom_Unknown = `NA`)
  
#combine studio, open 1br, and micro bedroom columns into one 0_bedroom column
ARCH_bedroom_sizes <- ARCH_bedroom_sizes %>% 
  mutate(Bedroom_0 = Studio + Micro + `Open 1-BR`) %>% 
  select(-Studio,
         -Micro,
         -`Open 1-BR`) 
  
#add blank columns not currently in data
ARCH_bedroom_sizes <- ARCH_bedroom_sizes %>% 
  mutate(ManagerUnit = 0,
         GroupHomeOrBed = 0)

#join AMI and bedroom size unit count df's together
ARCH_cleaned <- full_join(ARCH_AMIs, ARCH_bedroom_sizes)

#check for duplicates - none remaining
ARCH_cleaned %>% 
  count(`ARCH Project Name`,
        `Current Name`,
        `Qualified Project Period`) %>% 
  filter(n > 1)

#check to make sure AMI totals align with original df
ARCH_cleaned %>% 
  summarise_at(vars(ends_with(c("AMI", "MarketRate"))), sum) %>% 
  select(sort(names(.)))

original_ARCH_raw %>% 
  group_by(Affordability) %>% 
  summarise(units = sum(Units)) %>% 
  pivot_wider(names_from = Affordability,
              values_from = units) %>% 
  select(sort(names(.))) %>% 
  rename_at(vars(ends_with("AMI")), funs(str_replace(., " ", "")))

#check to make sure bedroom size totals align with original df
#1BR is 2 higher than original
ARCH_cleaned %>% 
  summarise_at(vars(starts_with("Bedroom")), sum) %>% 
  select(sort(names(.)))

original_ARCH_raw %>% 
  group_by(`Unit Type [BRs]`) %>% 
  summarise(units = sum(Units)) %>% 
  pivot_wider(names_from = `Unit Type [BRs]`,
              values_from = units) %>% 
  select(sort(names(.)))

#calculate total units columns
ARCH_cleaned <- ARCH_cleaned %>% 
  mutate(TotalUnits = MarketRate + `50AMI` + `60AMI` + `70AMI` + `80AMI` + `85AMI` + `90AMI` + `95AMI` + `100AMI` + `105AMI` + `110AMI` + `115AMI` + `120AMI`,
         TotalRestrictedUnits = `50AMI` + `60AMI` + `70AMI` + `80AMI` + `85AMI` + `90AMI` + `95AMI` + `100AMI` + `105AMI` + `110AMI` + `115AMI` + `120AMI`)

#rename AMI columns to match final database
ARCH_cleaned <- ARCH_cleaned %>% 
  rename(AMI50 = `50AMI`,
         AMI60 = `60AMI`,
         AMI70 = `70AMI`,
         AMI80 = `80AMI`,
         AMI85 = `85AMI`,
         AMI90 = `90AMI`,
         AMI95 = `95AMI`,
         AMI100 = `100AMI`,
         AMI105 = `105AMI`,
         AMI110 = `110AMI`,
         AMI115 = `115AMI`,
         AMI120 = `120AMI`)

#combine All AMI's above 100 into one single 100AMI column
ARCH_cleaned <- ARCH_cleaned %>% 
  mutate(AMI100 = AMI100 + AMI105 + AMI110 + AMI115 + AMI120)


#rename columns and add empty columns for data we dont have
ARCH_cleaned <- ARCH_cleaned %>% 
  mutate(DataSourceName = "ARCH",
         Address = as.character(NA),
         Funder = as.character(NA),
         AMI20 = as.numeric(NA),
         AMI25 = as.numeric(NA),
         AMI30 = as.numeric(NA), 
         AMI35 = as.numeric(NA),
         AMI40 = as.numeric(NA),
         AMI45 = as.numeric(NA),
         AMI65 = as.numeric(NA),
         AMI75 = as.numeric(NA),
         PropertyFunderProgramName = as.character(NA),
         Confidentiality = as.character(NA),
         ContactName = as.character(NA),
         ProjectSponsor = as.character(NA),
         Policy = as.character(NA),
         ProjectType = as.character(NA),
         FundingSource = as.character(NA)) %>% 
  rename(ProjectName = `ARCH Project Name`,
         PropertyName = `Current Name`,
         city = `Current Jurisdiction`,
         InServiceDate = `Placed In Service Year`,
         ExpirationYear = `Qualified Project Period`,
         PopulationServed = `Target Demographics`,
         Tenure = `Current Tenure`)

#select only necessary columns and arrange columns
ARCH_cleaned <- select_and_arrange_columns_function(ARCH_cleaned) 
  
#add unique ID that can be used for linking back to original, unedited data before we make manual changes to property names, project names, and addresses
ARCH_cleaned <- create_unique_linking_ID_function(ARCH_cleaned,
                                                  "ARCH")








## 4) clean RHA data --------------------------------------------------------------------

#see how many duplicates exist - none
original_RHA_raw %>% 
  count(`Property Name`,
        `Project Number`) %>% 
  filter(n > 1)

#standardize AMI values, using highest value listed
RHA_cleaned <- original_RHA_raw %>% 
  rename(AMI = `Income Restriction by Income Threshold`) %>% 
  mutate(AMI = case_when(AMI == "Up to 60% AMI" ~ "AMI60",
                         AMI == "3 Units up to 30% AMI and 5 Units up to 50% AMI" ~ "3 Units up to 30% AMI and 5 Units up to 50% AMI",
                         AMI == "Up to 50% AMI" ~ "AMI50",
                         AMI == "50% of Ami or below" ~ "AMI50",
                         AMI == "50%, 60%" ~ "AMI60",
                         AMI == "30%, 50%" ~ "AMI50",
                         TRUE ~ as.character(NA)))

#create AMI category
RHA_cleaned <- RHA_cleaned %>% 
  pivot_wider(names_from = AMI,
              values_from = `Total Housing Units`)

#fix issue where one property has units at multiple AMI categories
RHA_cleaned <- RHA_cleaned %>% 
  mutate(`AMI30` = if_else(`Property Name` == "Glennwood Avenue Townhomes", 3, as.numeric(NA)),
         `AMI50` = if_else(`Property Name` == "Glennwood Avenue Townhomes", 5, `AMI50`)) %>% 
  select(-`3 Units up to 30% AMI and 5 Units up to 50% AMI`)

#replace NA's with 0's
RHA_cleaned <- RHA_cleaned %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0))

#add total restricted units column
RHA_cleaned <- RHA_cleaned %>% 
  mutate(TotalRestrictedUnits = rowSums(across(starts_with("AMI"))))

#manually add bedroom size column by looking at individual text entries for unit size
RHA_cleaned <- RHA_cleaned %>% 
  mutate(Bedroom_0 = case_when(`Number of Units by Size` == "11 - 2 bedrooms, 5 - Studios,  44- 1 bedrooms" ~ 5,
                               `Number of Units by Size` == "Sixteen studios, Thirty-eight 1 bdrm, Six 2 bdrm" ~ 16,
                               `Number of Units by Size` == "Sixteen studios, Thirty-three 1 bdrm, One 2  bdrm" ~ 16,
                               `Number of Units by Size` == "27 - 2 bedrooms, 168 - 1 bedrooms, 49 - studios" ~ 49,
                               TRUE ~ 0),
         Bedroom_1 = case_when(`Number of Units by Size` == "Sixteen studios, Thirty-eight 1 bdrm, Six 2 bdrm" ~ 38,
                               `Number of Units by Size` == "Sixteen studios, Thirty-three 1 bdrm, One 2  bdrm" ~ 33,
                               `Number of Units by Size` == "103 - 1 bedrooms , 1 - 2 bedroom" ~ 103,
                               `Number of Units by Size` == "Fifty-two 1 bdrm, one 2 bdrm" ~ 52,
                               `Number of Units by Size` == "6 - 2 bedrooms, 9 1 bedrooms" ~ 9,
                               `Number of Units by Size` == "Seventy-two - 1 bdrm" ~ 72,
                               `Number of Units by Size` == "27 - 2 bedrooms, 168 - 1 bedrooms, 49 - studios" ~ 168,
                               `Number of Units by Size` == "18 - 3 bedroom, 20 - 2 bedroom, and 12 - 1 bedroom" ~ 12,
                               `Number of Units by Size` == "11 - 2 bedrooms, 5 - Studios,  44- 1 bedrooms" ~ 44,
                               TRUE ~ 0),
         Bedroom_2 = case_when(`Number of Units by Size` == "Twenty 2 bdrm, Six 3 bdrm, Two 4 bdrm" ~ 20,
                               `Number of Units by Size` == "Sixteen studios, Thirty-eight 1 bdrm, Six 2 bdrm" ~ 6,
                               `Number of Units by Size` == "Sixteen studios, Thirty-three 1 bdrm, One 2  bdrm" ~ 1,
                               `Number of Units by Size` == "12 - 3 bedrooms, 6 - 2 bedrooms" ~ 6,
                               `Number of Units by Size` == "6 - 2 bedrooms, 9 1 bedrooms" ~ 6,
                               `Number of Units by Size` == "103 - 1 bedrooms , 1 - 2 bedroom" ~ 1,
                               `Number of Units by Size` == "Fourteen 2 bdrm, Four 3 bdrm" ~ 14,
                               `Number of Units by Size` == "Fifty-two 1 bdrm, one 2 bdrm" ~ 1,
                               `Number of Units by Size` == "27 - 2 bedrooms, 168 - 1 bedrooms, 49 - studios" ~ 27,
                               `Number of Units by Size` == "18 - 3 bedroom, 20 - 2 bedroom, and 12 - 1 bedroom" ~ 20,
                               `Number of Units by Size` == "11 - 2 bedrooms, 5 - Studios,  44- 1 bedrooms" ~ 11,
                               TRUE ~ 0),
         Bedroom_3 = case_when(`Number of Units by Size` == "Twenty 2 bdrm, Six 3 bdrm, Two 4 bdrm" ~ 6,
                               `Number of Units by Size` == "12 - 3 bedrooms, 6 - 2 bedrooms" ~ 12,
                               `Number of Units by Size` == "Fourteen 2 bdrm, Four 3 bdrm" ~ 4,
                               `Number of Units by Size` == "18 - 3 bedroom, 20 - 2 bedroom, and 12 - 1 bedroom" ~ 18,
                               TRUE ~ 0),
         Bedroom_4 = case_when(`Number of Units by Size` == "Twenty 2 bdrm, Six 3 bdrm, Two 4 bdrm" ~ 2,
                               `Number of Units by Size` == "Eight 4 bdrm" ~ 8,
                               TRUE ~ 0),
         Bedroom_5 = 0) %>% 
  select(-`Number of Units by Size`)

#create tenure category
RHA_cleaned <- RHA_cleaned %>% 
  mutate(Tenure = case_when(Rental == "Y" & Ownership == "N" ~ "Rental",
                            Rental == "Y" & Ownership == "Y" ~ "Rental and Homeownership",
                            Rental == "N" & Ownership == "Y" ~ "Homeownership",
                            TRUE ~ as.character(NA)))

#rename columns and add empty columns for data we dont have
RHA_cleaned <- RHA_cleaned %>% 
  mutate(DataSourceName = "RHA",
         Funder = as.character(NA),
         TotalUnits = TotalRestrictedUnits,
         AMI20 = as.numeric(NA),
         AMI25 = as.numeric(NA),
         AMI35 = as.numeric(NA),
         AMI40 = as.numeric(NA),
         AMI45 = as.numeric(NA),
         AMI65 = as.numeric(NA),
         AMI70 = as.numeric(NA),
         AMI75 = as.numeric(NA),
         AMI80 = as.numeric(NA),
         AMI85 = as.numeric(NA),
         AMI90 = as.numeric(NA),
         AMI100 = as.numeric(NA),
         MarketRate = as.numeric(NA),
         Bedroom_Unknown = as.numeric(NA),
         GroupHomeOrBed = as.numeric(NA),
         PropertyFunderProgramName = as.character(NA),
         Confidentiality = as.character(NA),
         ContactName = as.character(NA),
         ProjectSponsor = as.character(NA),
         Policy = as.character(NA),
         PopulationServed = as.character(NA),
         ProjectType = as.character(NA),
         ExpirationYear = as.character(NA)) %>% 
  rename(Address = `Street Address`,
         ProjectName = `Property Name`,
         PropertyName = `Project Number`,
         FundingSource = `Funding Sources/Acquisition Contribution`,
         ManagerUnit = `Number of Manager Units`,
         InServiceDate = `In-Service Year`,
         city = City)


#select only necessary columns and arrange columns
RHA_cleaned <- select_and_arrange_columns_function(RHA_cleaned) 

#add unique ID that can be used for linking back to original, unedited data before we make manual changes to property names, project names, and addresses
RHA_cleaned <- create_unique_linking_ID_function(RHA_cleaned,
                                                 "RHA")








## 5) clean SHA data --------------------------------------------------------------------

#see how many duplicates exist - none
original_SHA_raw %>% 
  count(Project,
        Property) %>% 
  filter(n > 1)

#rename columns to match other data
SHA_cleaned <- original_SHA_raw %>% 
  rename(Bedroom_0 = `0BR`,
         Bedroom_1 = `1BR`,
         Bedroom_2 = `2BR`,
         Bedroom_3 = `3BR`,
         Bedroom_4 = `4BR`,
         Bedroom_5 = `5BR`,
         AMI30 = `30% AMI`,
         AMI40 = `40% AMI`,
         AMI45 = `45% AMI`,
         AMI50 = `50% AMI`,
         AMI60 = `60% AMI`,
         AMI80 = `80% AMI`,
         ExpirationYear = `Affordability Covenant(s) Expiration Year(s)`)

#add 6 BRs to 5+ BR category and remove 6 BR category
SHA_cleaned <- SHA_cleaned %>% 
  mutate(Bedroom_5 = Bedroom_5 + `6BR`) %>% 
  select(-`6BR`)

#rename columns and add empty columns for data we dont have
SHA_cleaned <- SHA_cleaned %>% 
  mutate(DataSourceName = "SHA",
         city = "Seattle",
         Funder = as.character(NA),
         AMI20 = as.numeric(NA),
         AMI25 = as.numeric(NA),
         AMI35 = as.numeric(NA),
         AMI65 = as.numeric(NA),
         AMI70 = as.numeric(NA),
         AMI75 = as.numeric(NA),
         AMI85 = as.numeric(NA),
         AMI90 = as.numeric(NA),
         AMI100 = as.numeric(NA),
         MarketRate = as.numeric(NA),
         ManagerUnit = as.numeric(NA),
         Bedroom_Unknown = as.numeric(NA),
         GroupHomeOrBed = as.numeric(NA),
         PropertyFunderProgramName = as.character(NA),
         Policy = as.character(NA),
         Funder = as.character(NA),
         FundingSource = as.character(NA)) %>% 
  rename(ProjectName = Project,
         PropertyName = Property,
         TotalUnits = `Total Units`,
         TotalRestrictedUnits = `Total Restricted Units`,
         ContactName = `Property Management Organization`,
         InServiceDate = `Year in Service`,
         ProjectSponsor = `Project Sponsor`,
         Confidentiality = `Confidentiality Status`,
         PopulationServed = `Population Served`,
         ProjectType = `Project Type`)

#select only necessary columns and arrange columns
SHA_cleaned <- select_and_arrange_columns_function(SHA_cleaned) 

#add unique ID that can be used for linking back to original, unedited data before we make manual changes to property names, project names, and addresses
SHA_cleaned <- create_unique_linking_ID_function(SHA_cleaned,
                                                 "SHA")










## 6) clean WSHFC data --------------------------------------------------------------------

#filter for just King County properties
WSHFC_cleaned <- original_WSHFC_raw %>%
  filter(County == "King")

#group all 0 bedroom columns
WSHFC_cleaned <- WSHFC_cleaned %>% 
  mutate(Bedroom_0 = SRO + STUDIO) %>% 
  select(-SRO,
         -STUDIO)

#group all population served categories together so they are in a single text variable
WSHFC_cleaned <- WSHFC_cleaned %>% 
  mutate(Elderly = if_else(!is.na(Elderly), "Elderly", as.character(NA)),
         `Persons with Disabilities` = if_else(!is.na(`Persons with Disabilities`), "Persons with Disabilities", as.character(NA)),
         Farmworker = if_else(!is.na(Farmworker), "Farmworker", as.character(NA)),
         Homeless = if_else(!is.na(Homeless), "Homeless", as.character(NA)),
         `Large Household (4+ pp)` = if_else(!is.na(`Large Household (4+ pp)`), "Large Household (4+ pp)", as.character(NA)),
         Transitional = if_else(!is.na(Transitional), "Transitional", as.character(NA)),
         Veterans = if_else(!is.na(Veterans), "Veterans", as.character(NA))) %>% 
  unite(PopulationServed, c(Elderly,
                            `Persons with Disabilities`,
                            Farmworker,
                            Homeless,
                            `Large Household (4+ pp)`,
                            Transitional,
                            Veterans),
        sep = ", ",
        remove = TRUE,
        na.rm = TRUE)

#create grouped funder column
WSHFC_cleaned <- WSHFC_cleaned %>% 
  group_by(`Site Name`, Address) %>% 
  mutate(Funder = paste(sort(unique(Funder)), collapse = ", "))

#create grouped population served column
WSHFC_cleaned <- WSHFC_cleaned %>% 
  group_by(`Site Name`, Address) %>% 
  mutate(PopulationServed2 = paste(sort(unique(PopulationServed)), collapse = ", "))

WSHFC_cleaned <- WSHFC_cleaned %>% 
  mutate(PopulationServed3 = PopulationServed2) %>% 
  separate_rows(PopulationServed3) %>% 
  group_by(`Site Name`, Address) %>% 
  mutate(PopulationServed4 = paste(unique(PopulationServed3), collapse = ", ")) %>% 
  mutate(PopulationServed4 = gsub(",", "", PopulationServed4)) %>% 
  mutate(PopulationServed4 = gsub("Disabilities", "Disabilities,", PopulationServed4)) %>% 
  mutate(PopulationServed4 = gsub("Homeless", "Homeless,", PopulationServed4)) %>% 
  mutate(PopulationServed4 = gsub("(4+ pp)", "(4+ pp),", PopulationServed4)) %>% 
  mutate(PopulationServed4 = gsub("Elderly", "Elderly,", PopulationServed4)) %>% 
  mutate(PopulationServed4 = gsub("Veterans", "Veterans,", PopulationServed4)) %>% 
  mutate(PopulationServed4 = gsub("Farmworker", "Farmworker,", PopulationServed4)) %>% 
  mutate(PopulationServed4 = gsub("Transitional", "Transitional,", PopulationServed4)) %>% 
  mutate(PopulationServed4 = trimws(PopulationServed4, which = "both")) %>% 
  mutate(PopulationServed4 = trimws(PopulationServed4, whitespace = ",")) %>% 
  mutate(PopulationServed = PopulationServed4) %>% 
  select(-PopulationServed2,
         -PopulationServed3,
         -PopulationServed4)


#remove Number of HOME Units category, which isnt needed and is duplicative of AMI count categories
WSHFC_cleaned <- WSHFC_cleaned %>% 
  select(-`Number of HOME Units`)


#select only the entry with the largest total restricted unit count, as there are often multiple entries with different unit counts
WSHFC_cleaned <- WSHFC_cleaned %>% 
  group_by(`Site Name`, Address) %>% 
  slice_max(`Income & Rent Restricted Units`,
            n = 1,
            with_ties = TRUE) %>% 
  distinct()

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

#for entries where there are multiple properties with the same total restricted unit count but different other data, select record that seems correct
WSHFC_cleaned <- WSHFC_cleaned %>% 
  distinct() %>% 
  filter(!(`Project Name` == "Hirabayashi Place" & `Site Name` == "Hirabayashi Place" & `50%` == 57)) %>% #select SOH record, remove Commerce record
  filter(!(`Project Name` == "Passage Point" & `Site Name` == "Passage Point" & `50%` == 0)) #select KC record, remove WSHFC record

#check to see if any duplicates remaining - should be 0
WSHFC_cleaned %>% 
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







## 7) clean SOH data --------------------------------------------------------------------

#filter out properties that shouldnt be included in the count, as identified by SOH
SOH_cleaned <- original_SOH_raw %>% 
  filter(`DUPLICATE RECORD - Yes/No` %in% c("NO - INCLUDE IN COUNT",
                                            "YES - INCLUDE IN COUNT",
                                            "YES - INCLUDE IN COUNT (manual entry)"))

#filter out units still in pipeline
SOH_cleaned <- SOH_cleaned %>% 
  filter(`HOUSING STATUS` != "PIPELINE - APPLICATION APPROVED")

#rename columns and add empty columns for data we dont have
SOH_cleaned <- SOH_cleaned %>% 
  mutate(DataSourceName = "SOH",
         ProjectName = as.character(NA),
         city = "Seattle",
         Funder = as.character(NA),
         AMI20 = as.numeric(NA),
         AMI25 = as.numeric(NA),
         AMI35 = as.numeric(NA),
         AMI45 = as.numeric(NA),
         AMI100 = as.numeric(NA),
         ManagerUnit = as.numeric(NA),
         Bedroom_4 = as.numeric(NA),
         Bedroom_5 = as.numeric(NA),
         Bedroom_Unknown = as.numeric(NA),
         PropertyFunderProgramName = as.character(NA),
         ContactName = as.character(NA),
         ProjectSponsor = as.character(NA),
         Policy = as.character(NA),
         PopulationServed = as.character(NA),
         ProjectType = as.character(NA),
         Tenure = as.character(NA),
         Funder = as.character(NA)) %>% 
  rename(PropertyName = `PROPERTY NAME`,
         Address = `PROPERTY ADDRESS`,
         TotalUnits = `TOTAL UNITS`,
         TotalRestrictedUnits = `RENT/INCOME RESTRICTED UNITS`,
         AMI30 = "<= 30% AMI",
         AMI40 = "<= 40% AMI",
         AMI50 = "<= 50% AMI",
         AMI60 = "< = 60% AMI",
         AMI65 = "<= 65% AMI",
         AMI70 = "<= 70% AMI",
         AMI75 = "<= 75% AMI",
         AMI80 = "<= 80% AMI",                                  
         AMI85 = "<= 85% AMI",
         AMI90 = "<=90% AMI",
         MarketRate = `UNRESTRICTED UNITS`,
         Bedroom_0 = `SEDU, STUDIO`,
         Bedroom_1 = `1 BR`,
         Bedroom_2 = `2 BR`,
         Bedroom_3 = `3 BR and larger`,
         GroupHomeOrBed = `CONGREGATE RESIDENCE SLEEPING ROOM, SRO, BED`,
         InServiceDate = `INITIAL YEAR "IN SERVICE"`,
         ExpirationYear = `ACTION DATE (YEAR)`,
         Confidentiality = `INCLUDE RECORD IN OPEN DATA`,
         FundingSource = `HOUSING COVENANTS`)

#select only necessary columns and arrange columns
SOH_cleaned <- select_and_arrange_columns_function(SOH_cleaned) 

#add unique ID that can be used for linking back to original, unedited data before we make manual changes to property names, project names, and addresses
SOH_cleaned <- create_unique_linking_ID_function(SOH_cleaned,
                                                 "SOH")






## 8) clean KCHA data --------------------------------------------------------------------

#remove unnecessary lines from KCHA 2020 data
KCHA_cleaned <- original_KCHA_raw_2020 %>% 
  filter(!is.na(`KCHA Prop #`))

#check to make sure column names and types are the same for 2020 and 2019 data
colnames(original_KCHA_raw_2019) == colnames(KCHA_cleaned)
sapply(KCHA_cleaned, class) == sapply(original_KCHA_raw_2019, class)

#join 2020 and 2019 data together, as KCHA only submitted new properties for 2020, not their entire portfolio
KCHA_cleaned <- bind_rows(original_KCHA_raw_2019, KCHA_cleaned)

#convert market category and bedroom categories to numeric
KCHA_cleaned <- KCHA_cleaned %>% 
  mutate(Market = if_else(Market == "-", as.character(NA), Market),
         `0 BR` = if_else(`0 BR` == "-", as.character(NA), `0 BR`),
         `1 BR` = if_else(`1 BR` == "-", as.character(NA), `1 BR`),
         `2 BR` = if_else(`2 BR` == "-", as.character(NA), `2 BR`),
         `3 BR` = if_else(`3 BR` == "-", as.character(NA), `3 BR`),
         `4 BR` = if_else(`4 BR` == "-", as.character(NA), `4 BR`),
         `5 BR` = if_else(`5 BR` == "-", as.character(NA), `5 BR`)) %>% 
  mutate(Market = as.numeric(Market),
         `0 BR` = as.numeric(`0 BR`),
         `1 BR` = as.numeric(`1 BR`),
         `2 BR` = as.numeric(`2 BR`),
         `3 BR` = as.numeric(`3 BR`),
         `4 BR` = as.numeric(`4 BR`),
         `5 BR` = as.numeric(`5 BR`))

#replace all numeric NA's with 0's
KCHA_cleaned <- KCHA_cleaned %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0))

#create confidentiality category
KCHA_cleaned <- KCHA_cleaned %>% 
  mutate(Confidentiality = case_when(!is.na(`Domestic Violence`) ~ "Domestic Violence",
                                     `Home Ownership Units` > 0 ~ "Homeownership",
                                     TRUE ~ as.character(NA)))

#create population served category
KCHA_cleaned <- KCHA_cleaned %>% 
  mutate(Seniors = if_else(Seniors > 0, "Seniors", as.character(NA)),
         `Elderly/ Disabled` = if_else(`Elderly/ Disabled` > 0, "Elderly/ Disabled", as.character(NA)),
         Disabled = if_else(Disabled > 0, "Disabled", as.character(NA)),
         Veterans = if_else(Veterans > 0, "Veterans", as.character(NA)),
         Families = if_else(Families > 0, "Families", as.character(NA)),
         `Domestic Violence` = if_else(!is.na(`Domestic Violence`), as.character("Domestic Violence"), as.character(NA)),
         Homeless = if_else(Homeless > 0, "Homeless", as.character(NA))) %>% 
  unite(col = PopulationServed, 
               c(Seniors,
                 `Elderly/ Disabled`,
                 Disabled,
                 Veterans,
                 Families,
                 `Domestic Violence`,
                 Homeless),
        sep = ",",
        na.rm = TRUE,
        remove = FALSE)

#create project type category
KCHA_cleaned <- KCHA_cleaned %>% 
  mutate(`Emergency Shelter Units` = if_else(`Emergency Shelter Units` > 0, "Emergency Shelter Units", as.character(NA)),
         `Transitional Units` = if_else(`Transitional Units` > 0, "Transitional Units", as.character(NA)),
         `Permanent Supportive Housing` = if_else(`Permanent Supportive Housing` > 0, "Permanent Supportive Housing", as.character(NA))) %>% 
  unite(col = ProjectType, 
        c(`Emergency Shelter Units`,
          `Transitional Units`,
          `Permanent Supportive Housing`),
        sep = ",",
        na.rm = TRUE,
        remove = FALSE)

#create tenure category
KCHA_cleaned <- KCHA_cleaned %>% 
  mutate(Tenure = case_when(`Rental Units` > 0 & `Home Ownership Units` == 0 ~ "Rental",
                            `Rental Units` == 0 & `Home Ownership Units` > 0 ~ "Homeownership",
                            `Rental Units` > 0 & `Home Ownership Units` > 0 ~ "Rental and Homeownership",
                            TRUE ~ as.character(NA)))

#create funder category
KCHA_cleaned <- KCHA_cleaned %>% 
  mutate(`Commerce` = if_else(`Commerce` %in% c("Y", "TRUE"), "Commerce", as.character(NA)),
         `King County` = if_else(`King County` %in% c("Y", "TRUE"), "King County", as.character(NA)),
         `ARCH` = if_else(`ARCH` %in% c("Y", "TRUE"), "ARCH", as.character(NA)),
         `City of Bellevue` = if_else(`City of Bellevue` %in% c("Y", "TRUE"), "City of Bellevue", as.character(NA))) %>% 
  unite(col = Funder, 
        c(`Commerce`,
          `King County`,
          `ARCH`,
          `City of Bellevue`),
        sep = ",",
        na.rm = TRUE,
        remove = FALSE)
  
#create funding source category
KCHA_cleaned <- KCHA_cleaned %>% 
  mutate(`4% LIHTC` = if_else(`4% LIHTC` %in% c("Y", "TRUE"), "4% LIHTC", as.character(NA)),
         `9% LIHTC` = if_else(`9% LIHTC` %in% c("Y", "TRUE"), "9% LIHTC", as.character(NA)),
         `New Markets Tax Credits` = if_else(`New Markets Tax Credits` %in% c("Y", "TRUE"), "New Markets Tax Credits", as.character(NA)),
         `Tax Exempt Bonds` = if_else(`Tax Exempt Bonds` %in% c("Y", "TRUE"), "Tax Exempt Bonds", as.character(NA)),
         `HUD Financing` = if_else(`HUD Financing` %in% c("Y", "TRUE"), "HUD Financing", as.character(NA)),
         `USDA Loan` = if_else(`USDA Loan` %in% c("Y", "TRUE"), "USDA Loan", as.character(NA)),
         `King County CLA Credit Enhancement` = if_else(`King County CLA Credit Enhancement` %in% c("Y", "TRUE"), "King County CLA Credit Enhancement", as.character(NA)),
         `King County Direct Credit Enhancement` = if_else(`King County Direct Credit Enhancement` %in% c("Y", "TRUE"), "King County Direct Credit Enhancement", as.character(NA))) %>% 
  unite(col = FundingSource, 
        c(`Commerce`,
          `King County`,
          `ARCH`,
          `City of Bellevue`,
          `4% LIHTC`,
          `9% LIHTC`,
          `New Markets Tax Credits`,
          `Tax Exempt Bonds`,
          `HUD Financing`,
          `USDA Loan`,
          `King County CLA Credit Enhancement`,
          `King County Direct Credit Enhancement`),
        sep = ",",
        na.rm = TRUE,
        remove = FALSE)

#rename columns and add empty columns for data we dont have
KCHA_cleaned <- KCHA_cleaned %>% 
  mutate(DataSourceName = "KCHA",
         ProjectName = as.character(NA),
         Funder = as.character(NA), #CHECK ON THIS
         TotalRestrictedUnits = (`Total Housing Units` - Market),
         AMI20 = 0,
         AMI25 = 0,                    
         AMI35 = 0,
         AMI65 = 0,
         AMI70 = 0,
         AMI75 = 0,
         AMI85 = 0,
         AMI90 = 0,
         AMI100 = 0,
         Bedroom_Unknown = 0, #Switch to NA?
         GroupHomeOrBed = 0, #Switch to NA?
         PropertyFunderProgramName = as.character(NA),
         ExpirationYear = as.character(NA),
         Policy = as.character(NA)) %>%  
  rename(PropertyName = Name,
         city = City,
         TotalUnits = `Total Housing Units`,
         AMI30 = `30% AMI`,
         AMI40 = `40% AMI`,
         AMI45 = `45% AMI`,
         AMI50 = `50% AMI`,
         AMI60 = `60% AMI`,
         AMI80 = `80% AMI`,
         MarketRate = Market,
         ManagerUnit = `Manager's Unit`,
         Bedroom_0 = `0 BR`,
         Bedroom_1 = `1 BR`,
         Bedroom_2 = `2 BR`,
         Bedroom_3 = `3 BR`,
         Bedroom_4 = `4 BR`,
         Bedroom_5 = `5 BR`,
         InServiceDate = `In Service Date`,
         ContactName = `Managed By`,
         ProjectSponsor = `Sponsor/Developer`)

#select only necessary columns and arrange columns
KCHA_cleaned <- select_and_arrange_columns_function(KCHA_cleaned) 

#add unique ID that can be used for linking back to original, unedited data before we make manual changes to property names, project names, and addresses
KCHA_cleaned <- create_unique_linking_ID_function(KCHA_cleaned,
                                                  "KCHA")









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
write_csv(ARCH_cleaned, paste0(N_drive_cleaned_files_filepath, "ARCH_2020_cleaned.csv"))
write_csv(RHA_cleaned, paste0(N_drive_cleaned_files_filepath, "RHA_2020_cleaned.csv"))
write_csv(WSHFC_cleaned, paste0(N_drive_cleaned_files_filepath, "WSHFC_2020_cleaned.csv"))
write_csv(SOH_cleaned, paste0(N_drive_cleaned_files_filepath, "SOH_2020_cleaned.csv"))
write_csv(SHA_cleaned, paste0(N_drive_cleaned_files_filepath, "SHA_2020_cleaned.csv"))
write_csv(KCHA_cleaned, paste0(N_drive_cleaned_files_filepath, "KCHA_2020_cleaned.csv"))










## 11)  --------------------------------------------------------------------

## 12)  --------------------------------------------------------------------

## 13)  --------------------------------------------------------------------

## 14)  --------------------------------------------------------------------