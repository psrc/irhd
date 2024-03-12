# TITLE: Clean 2022 Data Recieved from Data Providers
# DATA SOURCE: HASCO, THA, EHA
# DATE MODIFIED: 03.11.2024
# AUTHOR: Eric Clute

# Assumptions
library(readxl)
library(janitor)
library(data.table)

setwd("C:/Users/eclute/GitHub/irhd")
tha_raw <- "J:/Projects/IncomeRestrictedHsgDB/2022 vintage/Review - Files Recieved/final_review_PIERCE_THA_Update.xlsx"
hasco_raw <- "J:/Projects/IncomeRestrictedHsgDB/2022 vintage/Review - Files Recieved/final_review_SNOHOMISH_hasco.xlsx"
eva_raw <-"J:/Projects/IncomeRestrictedHsgDB/2022 vintage/Review - Files Recieved/final_review_SNOHOMISH_EHA.xlsx"

# Pull in data
tha <- read_excel(tha_raw, sheet = 3)
hasco <- read_excel(hasco_raw, sheet = 3)
eva <- read_excel(eva_raw, sheet = 3)

# Clean data
tha <- tha %>% filter(tha$data_source == "THA")
hasco <- hasco %>% filter(!hasco$Reviewer_Comments == "")
eva <- eva %>% filter(eva$manager == "Everett Housing Authority")


updates_received <- rbind(tha,hasco)
updates_received <- rbind(updates_received, eva)
