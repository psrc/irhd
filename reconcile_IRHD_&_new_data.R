#################################################################################
# Title: Reconcile IRHD and new data
# Author: Eric Clute (with assistance from Jesse Warren, King County)
# Date created: 2022-12-07
# Last Updated: 2023-03-02
#################################################################################


## load packages-----------------------------------------------------------------

library(tidyverse)
library(readxl)
library(janitor)


## 1) load data ---------------------------------------------------------------------

#load cleaned 2022 IRHD that has portfolios as of end of 2021
IRHD22raw <- read_csv("J:/Projects/IncomeRestrictedHsgDB/2022_update/Data/1 Working Files/2022 IRHD v3 - ready4reconcilescript.csv")

#load cleaned WSHFC data that has portfolios as of end of 2021
WSHFC22raw <- read_csv("J:/Projects/IncomeRestrictedHsgDB/2022_update/WSHFC/Cleaned Data/WSHFC_2022_cleaned.csv")

#load cleaned KC data that has portfolios as of end of 2021
# KC22raw <- read_csv("J:/Projects/IncomeRestrictedHsgDB/2022_update/Review Files - Recieved/")


## 2) clean up HOME data in IRHD. Rename existing HOME field, add new fields --------------------------------------------------------------------

# Create three new HOME fields. Rename existing HOME field
IRHD22raw <- IRHD22raw %>%
  mutate(HOMEcity = as.character(NA),
         HOMEcounty = as.character(NA),
         HOMEstate = as.character(NA)) %>% 
  rename(HOMEtotal = `HOME`)

#reorder fields
IRHD22raw <- IRHD22raw[, c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
                           21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
                           41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,
                           61,62,63,64,65,66,67,68,69,78,79,80,70,71,72,73,74,75,76,77)]


## 3) Locate records in WSHFC not in IRHD (matched on PropertyID) --------------------------------------------------------------------






## 4) Locate records in IRHD not in WSHFC (matched on PropertyID) --------------------------------------------------------------------






## 5) Locate records found in both WSHFC and IRHD (matched on PropertyID) --------------------------------------------------------------------






## 6) Update fields in IRHD for records found in both WSHFC and IRHD (matched on PropertyID) --------------------------------------------------------------------










# ## 3) compare ARCH --------------------------------------------------------------------
# 
# #ensure column names are the same
# all.equal(sort(colnames(ARCH_2020_raw)), sort(colnames(ARCH_2021_raw)))
# 
# #see if any fields are different
# all.equal(ARCH_2020_raw, ARCH_2021_raw)
# 
# #join 2021 and 2022 data together
# ARCH_joined <- full_join(ARCH_2020_raw,
#                          ARCH_2021_raw,
#                          by = "PropertyName",
#                          suffix = c(" 2020",
#                                     " 2021"),
#                          keep = FALSE) %>% 
#   select(PropertyName,
#          everything())
# 
# #create join status column using a value that is non-NA for every field in both the 2021 and 2022 individual dfs
# ARCH_joined <- ARCH_joined %>% 
#   mutate(join_status = case_when(!is.na(`city 2020`) & !is.na(`city 2021`) ~ "Joined",
#                                  !is.na(`city 2020`) & is.na(`city 2021`) ~ "Only in 2020 Data",
#                                  is.na(`city 2020`) & !is.na(`city 2021`) ~ "Only in 2021 Data",
#                                  TRUE ~ as.character(NA)))
# 
# 
# #172 joined, 14 only in 2020 data, 7 only in 2021 data
# ARCH_joined$join_status %>% 
#   table()
# 
# #see which properties werent joined
# #looked at these manually, no obvious ones that should have been joined. some with similar names but very different unit counts, so probably not the same records
# #somewhat unsure on this though
# ARCH_joined %>% 
#   filter(join_status == "Only in 2020 Data") %>% 
#   select(PropertyName,
#          ends_with("2020"))
# ARCH_joined %>% 
#   filter(join_status == "Only in 2021 Data") %>% 
#   select(PropertyName,
#          ends_with("2021"))
# 
# 
# #create function to compare differences
# compare_2020_2021_differences <- function(df,
#                                           grouping_variable_1,
#                                           grouping_variable_2 = "",
#                                           grouping_variable_3 = "",
#                                           comparison_variable){
#   
#   comparison_variable_2020 <- sym(paste0(comparison_variable, " 2020"))
#   comparison_variable_2021 <- sym(paste0(comparison_variable, " 2021"))
# 
#   df %>% 
#     select(any_of(c(!!grouping_variable_1,
#                     !!grouping_variable_2,
#                     !!grouping_variable_3)),
#            !!comparison_variable_2020,
#            !!comparison_variable_2021) %>% 
#     mutate(diff = if_else(!!comparison_variable_2020 == !!comparison_variable_2021, "Same", "Different")) %>% 
#     tabyl(diff) %>% 
#     print()
#   
#   df %>% 
#     select(any_of(c(!!grouping_variable_1,
#                     !!grouping_variable_2,
#                     !!grouping_variable_3)),
#            !!comparison_variable_2020,
#            !!comparison_variable_2021) %>% 
#     mutate(diff = if_else(!!comparison_variable_2020 == !!comparison_variable_2021, "Same", "Different")) %>% 
#     filter(diff == "Different") %>% 
#     print(n = 1000)
# 
# }
# 
# 
# #compare current jurisdiction field - all same or NA's
# compare_2020_2021_differences(ARCH_joined,
#                               "PropertyName",
#                               comparison_variable = "city")
# 
# #compare tenure field - all same or NA's
# compare_2020_2021_differences(ARCH_joined,
#                               "PropertyName",
#                               comparison_variable = "Tenure")
# 
# #compare placed in service year field
# compare_2020_2021_differences(ARCH_joined,
#                               "PropertyName",
#                               comparison_variable = "InServiceDate") 
# 
# #compare total restricted units field
# compare_2020_2021_differences(ARCH_joined,
#                               "PropertyName",
#                               comparison_variable = "TotalRestrictedUnits")
# 
# #compare total units field
# compare_2020_2021_differences(ARCH_joined,
#                               "PropertyName",
#                               comparison_variable = "TotalUnits")
# 
# #compare population served field
# compare_2020_2021_differences(ARCH_joined,
#                               "PropertyName",
#                               comparison_variable = "PopulationServed")
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ## 3) compare RHA --------------------------------------------------------------------
# 
# #ensure column names are the same
# all.equal(sort(colnames(RHA_2020_raw)), sort(colnames(RHA_2021_raw)))
# 
# #see if any fields are different
# all.equal(RHA_2020_raw, RHA_2021_raw)
# 
# #join 2021 and 2022 data together
# RHA_joined <- full_join(RHA_2020_raw,
#                         RHA_2021_raw,
#                          by = c("ProjectName",
#                                 "PropertyName"),
#                          suffix = c(" 2020",
#                                     " 2021"),
#                          keep = FALSE) %>% 
#   select(ProjectName,
#          PropertyName,
#          everything())
# 
# #compare total units field
# compare_2020_2021_differences(RHA_joined,
#                               "ProjectName",
#                               "PropertyName",
#                               comparison_variable = "city")
# 
# #compare in service year field
# compare_2020_2021_differences(RHA_joined,
#                               "Property Name",
#                               "Project Number",
#                               comparison_variable = "Address")
# 
# #compare address field
# compare_2020_2021_differences(RHA_joined,
#                               "Property Name",
#                               "Project Number",
#                               comparison_variable = "TotalUnits")
# 
# #compare rental field
# compare_2020_2021_differences(RHA_joined,
#                               "Property Name",
#                               "Project Number",
#                               comparison_variable = "TotalRestrictedUnits")
# 
# #compare ownership field
# compare_2020_2021_differences(RHA_joined,
#                               "Property Name",
#                               "Project Number",
#                               comparison_variable = "InServiceDate")
# 
# #compare income restriction field
# compare_2020_2021_differences(RHA_joined,
#                               "Property Name",
#                               "Project Number",
#                               comparison_variable = "ExpirationYear")