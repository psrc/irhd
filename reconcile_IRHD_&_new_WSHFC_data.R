#################################################################################
# Title: Reconcile IRHD and new WSHFC data
# Author: Eric Clute (with assistance from Jesse Warren, King County)
# Date created: 2022-12-07
# Last Updated: 2023-02-22
#################################################################################


## load packages-----------------------------------------------------------------

library(tidyverse)
library(readxl)
library(janitor)


## 1) load data ---------------------------------------------------------------------

#load cleaned 2022 IRHD that has portfolios as of end of 2021
IRHD_2022_raw <- read_csv("J:/Projects/IncomeRestrictedHsgDB/2022_update/Data/1 Working Files/2022 IRHD v3 - ready4reconcilescript.csv")

#load cleaned WSHFC data that has portfolios as of end of 2021
WSHFC_2022_raw <- read_csv("J:/Projects/IncomeRestrictedHsgDB/2022_update/WSHFC/Cleaned Data/WSHFC_2021_cleaned.csv")

#load cleaned KCdata that has portfolios as of end of 2021
KC_2022_raw <- read_csv("J:/Projects/IncomeRestrictedHsgDB/2022_update/WSHFC/")

# #save N: drive raw files location filepaths
# N_drive_2021_submissions_raw_files_filepath <- "N:/PME/Homeless Housing and Services/Data Sets/Income Restricted Database/original_data_provider_files/2021/raw_files/"
# N_drive_2022_submissions_raw_files_filepath <- "N:/PME/Homeless Housing and Services/Data Sets/Income Restricted Database/original_data_provider_files/2022/raw_files/"
# 
# #load original files from reporting agencies we received in 2021, that has data on portfolios as of the end of 2020
# ARCH_2021_submission_raw <- read_xlsx(paste0(N_drive_2021_submissions_raw_files_filepath, "ARCH_Housing_Units-Recorded_Regulatory_Agreements-KC_dashboard.xlsx"))
# RHA_2021_submission_raw <- read_xlsx(paste0(N_drive_2021_submissions_raw_files_filepath, "RHA Subsidized Housing 2021.xlsx"),
#                                      skip = 2)
# SHA_2021_submission_raw <- read_xlsx(paste0(N_drive_2021_submissions_raw_files_filepath, "KC Income-Restricted Housing db 2020 Update.xlsx"))
# WSHFC_2021_submission_raw <- read_xlsx(paste0(N_drive_2021_submissions_raw_files_filepath, "PSRC WBARS Report_12-31-2020.xlsx"))
# SOH_2021_submission_raw <- read_xlsx(paste0(N_drive_2021_submissions_raw_files_filepath, "City of Seattle Rent and Income Restricted Housing as of 2020-12-31 for KING COUNTY 2021-10-22.xlsx"))
# KCHA_2022_submission_raw <- read_xlsx(paste0(N_drive_2021_submissions_raw_files_filepath, "KCHAPSRCDatabase-2020 Update.xlsx"),
#                                     skip = 4,
#                                     col_types = c("text",
#                                                   "text",
#                                                   "text",
#                                                   "text",
#                                                   "text",
#                                                   "numeric",
#                                                   "numeric",
#                                                   "text",
#                                                   "text",
#                                                   "numeric",
#                                                   "text",
#                                                   "numeric",
#                                                   "numeric",
#                                                   "numeric",
#                                                   "numeric",
#                                                   "numeric",
#                                                   "numeric",
#                                                   "numeric",
#                                                   "numeric",
#                                                   "text",
#                                                   "numeric",
#                                                   "text",
#                                                   "text",
#                                                   "text",
#                                                   "text",
#                                                   "text",
#                                                   "text",
#                                                   "text",
#                                                   "text",
#                                                   "numeric",
#                                                   "numeric",
#                                                   "numeric",
#                                                   "numeric",
#                                                   "numeric",
#                                                   "numeric",
#                                                   "logical",
#                                                   "numeric",
#                                                   "numeric",
#                                                   "numeric",
#                                                   "numeric",
#                                                   "text",
#                                                   "text",
#                                                   "text",
#                                                   "text",
#                                                   "text",
#                                                   "text",
#                                                   "text",
#                                                   "text",
#                                                   "text",
#                                                   "text",
#                                                   "text",
#                                                   "text",
#                                                   "numeric",
#                                                   "numeric",
#                                                   "numeric",
#                                                   "numeric",
#                                                   "numeric"))
# 
# #load original files from reporting agencies we received in 2022
# ARCH_2022_submission_raw <- read_xlsx(paste0(N_drive_2022_submissions_raw_files_filepath, "ARCH_Housing_Units-Recorded_Regulatory_Agreements-KC_dashboard-20221202.xlsx"))
# RHA_2022_submission_raw <- read_xlsx(paste0(N_drive_2022_submissions_raw_files_filepath, "RHA Subsidized Housing 2021.xlsx"),
#                      skip = 2)
# SHA_2022_submission_raw <- read_xlsx(paste0(N_drive_2022_submissions_raw_files_filepath, "KC Income-Restricted Housing db 2021 Update.xlsx"))
# WSHFC_2022_submission_raw <- read_xlsx(paste0(N_drive_2022_submissions_raw_files_filepath, "PSRC Report_WSHFC_12-2021.xlsx"))
# SOH_2022_submission_raw <- read_xlsx(paste0(N_drive_2022_submissions_raw_files_filepath, "City of Seattle LIH - 2021 Activity for King County.xlsx"))
# KCHA_2022_submission_raw <- read_xlsx(paste0(N_drive_2022_submissions_raw_files_filepath, "KCHAPSRCDatabase-2022Update.xlsx"),
#                                       skip = 4)



## 2) add HOME units column so column names align --------------------------------------------------------------------

add_HOME_units_column <- function(df){
  
  df <- df %>% 
    mutate(HOMEUnits = as.numeric(NA))
  
}

ARCH_2020_raw <- add_HOME_units_column(ARCH_2020_raw)
RHA_2020_raw <- add_HOME_units_column(RHA_2020_raw)
WSHFC_2020_raw <- add_HOME_units_column(WSHFC_2020_raw)
SOH_2020_raw <- add_HOME_units_column(SOH_2020_raw)
SHA_2020_raw <- add_HOME_units_column(SHA_2020_raw)
KCHA_2020_raw <- add_HOME_units_column(KCHA_2020_raw)
  




## 3) compare ARCH --------------------------------------------------------------------

#ensure column names are the same
all.equal(sort(colnames(ARCH_2020_raw)), sort(colnames(ARCH_2021_raw)))

#see if any fields are different
all.equal(ARCH_2020_raw, ARCH_2021_raw)

#join 2021 and 2022 data together
ARCH_joined <- full_join(ARCH_2020_raw,
                         ARCH_2021_raw,
                         by = "PropertyName",
                         suffix = c(" 2020",
                                    " 2021"),
                         keep = FALSE) %>% 
  select(PropertyName,
         everything())

#create join status column using a value that is non-NA for every field in both the 2021 and 2022 individual dfs
ARCH_joined <- ARCH_joined %>% 
  mutate(join_status = case_when(!is.na(`city 2020`) & !is.na(`city 2021`) ~ "Joined",
                                 !is.na(`city 2020`) & is.na(`city 2021`) ~ "Only in 2020 Data",
                                 is.na(`city 2020`) & !is.na(`city 2021`) ~ "Only in 2021 Data",
                                 TRUE ~ as.character(NA)))


#172 joined, 14 only in 2020 data, 7 only in 2021 data
ARCH_joined$join_status %>% 
  table()

#see which properties werent joined
#looked at these manually, no obvious ones that should have been joined. some with similar names but very different unit counts, so probably not the same records
#somewhat unsure on this though
ARCH_joined %>% 
  filter(join_status == "Only in 2020 Data") %>% 
  select(PropertyName,
         ends_with("2020"))
ARCH_joined %>% 
  filter(join_status == "Only in 2021 Data") %>% 
  select(PropertyName,
         ends_with("2021"))


#create function to compare differences
compare_2020_2021_differences <- function(df,
                                          grouping_variable_1,
                                          grouping_variable_2 = "",
                                          grouping_variable_3 = "",
                                          comparison_variable){
  
  comparison_variable_2020 <- sym(paste0(comparison_variable, " 2020"))
  comparison_variable_2021 <- sym(paste0(comparison_variable, " 2021"))

  df %>% 
    select(any_of(c(!!grouping_variable_1,
                    !!grouping_variable_2,
                    !!grouping_variable_3)),
           !!comparison_variable_2020,
           !!comparison_variable_2021) %>% 
    mutate(diff = if_else(!!comparison_variable_2020 == !!comparison_variable_2021, "Same", "Different")) %>% 
    tabyl(diff) %>% 
    print()
  
  df %>% 
    select(any_of(c(!!grouping_variable_1,
                    !!grouping_variable_2,
                    !!grouping_variable_3)),
           !!comparison_variable_2020,
           !!comparison_variable_2021) %>% 
    mutate(diff = if_else(!!comparison_variable_2020 == !!comparison_variable_2021, "Same", "Different")) %>% 
    filter(diff == "Different") %>% 
    print(n = 1000)

}


#compare current jurisdiction field - all same or NA's
compare_2020_2021_differences(ARCH_joined,
                              "PropertyName",
                              comparison_variable = "city")

#compare tenure field - all same or NA's
compare_2020_2021_differences(ARCH_joined,
                              "PropertyName",
                              comparison_variable = "Tenure")

#compare placed in service year field
compare_2020_2021_differences(ARCH_joined,
                              "PropertyName",
                              comparison_variable = "InServiceDate") 

#compare total restricted units field
compare_2020_2021_differences(ARCH_joined,
                              "PropertyName",
                              comparison_variable = "TotalRestrictedUnits")

#compare total units field
compare_2020_2021_differences(ARCH_joined,
                              "PropertyName",
                              comparison_variable = "TotalUnits")

#compare population served field
compare_2020_2021_differences(ARCH_joined,
                              "PropertyName",
                              comparison_variable = "PopulationServed")










## 3) compare RHA --------------------------------------------------------------------

#ensure column names are the same
all.equal(sort(colnames(RHA_2020_raw)), sort(colnames(RHA_2021_raw)))

#see if any fields are different
all.equal(RHA_2020_raw, RHA_2021_raw)

#join 2021 and 2022 data together
RHA_joined <- full_join(RHA_2020_raw,
                        RHA_2021_raw,
                         by = c("ProjectName",
                                "PropertyName"),
                         suffix = c(" 2020",
                                    " 2021"),
                         keep = FALSE) %>% 
  select(ProjectName,
         PropertyName,
         everything())

#compare total units field
compare_2020_2021_differences(RHA_joined,
                              "ProjectName",
                              "PropertyName",
                              comparison_variable = "city")

#compare in service year field
compare_2020_2021_differences(RHA_joined,
                              "Property Name",
                              "Project Number",
                              comparison_variable = "Address")

#compare address field
compare_2020_2021_differences(RHA_joined,
                              "Property Name",
                              "Project Number",
                              comparison_variable = "TotalUnits")

#compare rental field
compare_2020_2021_differences(RHA_joined,
                              "Property Name",
                              "Project Number",
                              comparison_variable = "TotalRestrictedUnits")

#compare ownership field
compare_2020_2021_differences(RHA_joined,
                              "Property Name",
                              "Project Number",
                              comparison_variable = "InServiceDate")

#compare income restriction field
compare_2020_2021_differences(RHA_joined,
                              "Property Name",
                              "Project Number",
                              comparison_variable = "ExpirationYear")








