# TITLE: Reconcile IRHD and new data
# GEOGRAPHIES: King, Snohomish, Pierce, Kitsap
# DATA SOURCE: King County, WSHFC, HASCO, THA, EHA, PCHA, BHA, HK
# DATE CREATED: 05.01.2025
# AUTHOR: Eric Clute

## INSTRUCTIONS
## STEP 1: Set Assumptions, connect to Elmer
## STEP 2: Clean new data received from WSHFC (Washington State Housing Finance Commission)
## STEP 3: Identify changes to WSHFC data compared to last vintage of IRHD (select which datapoints to go with)
## STEP 4: Send prelim IRHD to data providers for review
## STEP 5: Incorporate any changes from data providers
## STEP 6: Final clean up and push to Elmer


## STEP 1: Assumptions -------------------------
library(tidyverse)
library(tidyr)
library(readxl)
library(data.table)
library(magrittr)
library(stringr)
library(dplyr)
library(odbc)
library(DBI)
library(openxlsx)

setwd("C:/Users/eclute/GitHub/irhd")
review_after_join_housingauthorities <- "./Export4review-housingauthorities.csv" # Export for review after WSHFC-IRHD join. Help understanding why property data are changing, reach out to housing authorities or WSHFC
review_after_join_wshfc <- "./Export4review-wshfc.csv" # Export for review after WSHFC-IRHD join. Why property data are missing from new WSHFC data but included in IRHD
final_review_housingauthorities <- "./final_review_housingauthorities.xlsx" # Export final dataset for review by housing authorities

irhd_func <- "./irhd_cleaning_func.R"
wshfc_clean_script <- "./clean_2023_WSHFC_data.R"
#kc_clean_script <- "./clean_2023_KC_data.R"
updates_received_script <- "./clean_2023_provider_data.R"

source(irhd_func)

`%not_in%` <- Negate(`%in%`)
vintage_year <- 2023
last_vintage <- vintage_year - 1

elmer_connection <- dbConnect(odbc::odbc(),
                              driver = "SQL Server",
                              server = "SQLserver",
                              database = "Elmer",
                              trusted_connection = "yes")

table_id <- Id(schema = "stg", table = "irhd")
sql_bing_maps_key <- Sys.getenv("BING_MAPS_KEY")
sql_import <- paste('irhd.properties')
sql_export <- paste0('exec irhd.merge_irhd_properties ', vintage_year, ",'", sql_bing_maps_key, "'")

## STEP 2: Clean new data received from WSHFC (Washington State Housing Finance Commission) -------------------------
## a) load data
IRHD_raw <- dbReadTable(elmer_connection, SQL(sql_import)) # import last vintage of IRHD from Elmer
source(wshfc_clean_script) # cleaned WSHFC data
#source(kc_clean_script) # cleaned KC data
source(updates_received_script) #cleaned data from providers

## b) Final tweaks to incoming data
IRHD_raw <- IRHD_raw %>% filter(data_year == last_vintage)
IRHD <- IRHD_raw %>% filter(!(county == "King")) %>% # King county handled separately
                     select(-c(created_at,updated_at,shape,irhd_property_id)) %>% # Remove unneeded fields
                     mutate(full_address = str_replace(full_address, ",\\s*(?=\\d{5}$)", " ")) # Remove extra comma before zip code

# Clean KC data - Identify & carry over assigned working_ids from prior vintage
## This step may be clarified in future if KC decides to create a key field to help with matching/tracking changes over time

# IRHD_raw_kc <- IRHD_raw %>% filter(county == "King") %>%
#                             filter(working_id != "SH_7289") %>% # Removed duplicate from prior vintage (La Madera Apartment Homes)
#                             filter(working_id != "SH_7290") %>% # Removed duplicate from prior vintage (Larc @ Burien)
#                             filter(working_id != "SH_7293") %>% # Removed duplicate from prior vintage (Panorama Apartments)
#                             filter(working_id != "SH_7281") %>% # Removed duplicate from prior vintage (Crossroads Senior Living)
#                             mutate(kc_match = paste(project_name, property_name, total_units, zip, sep = " - "))
# 
# kc_cleaned_with_wid <- KC_cleaned %>% filter(!is.na(working_id))
# kc_cleaned_no_wid <- KC_cleaned %>% filter(is.na(working_id)) %>%
#                                     mutate(kc_match = paste(project_name, property_name, total_units, zip, sep = " - "))
# 
# # Join, matching on newly created kc_match
# kc_cleaned_no_wid <- left_join(kc_cleaned_no_wid,
#                                select(IRHD_raw_kc, kc_match, working_id),
#                                by = "kc_match")
# 
# kc_cleaned_no_wid <- kc_cleaned_no_wid %>%
#   select(-c(working_id.x, kc_match)) %>%  # Remove the column "working_id.x"
#   rename(working_id = working_id.y)  # Rename "working_id.y" to "working_id"
# 
# # Append kc_cleaned_no_wid to kc_cleaned_with_wid to create an updated KC_cleaned df
# KC_cleaned <- kc_cleaned_with_wid %>%
#   bind_rows(kc_cleaned_no_wid)
# 
# rm(IRHD_raw_kc,kc_cleaned_no_wid,kc_cleaned_with_wid)

## STEP 3: Identify changes to WSHFC data compared to last vintage of IRHD (select which datapoint to go with) -------------------------
## a) Locate records in WSHFC not in IRHD (likely new records/properties)

new_wshfc <- anti_join(WSHFC_cleaned, IRHD, by = "property_id")

## b) Locate records in IRHD not in WSHFC_cleaned (No longer in WSHFC data. Similar to last vintage. 2022 Nona @ WSHFC confirmed these were filtered out by "site type". Assisted living, DV, group home, manufactured housing)

no_match_irhd <- anti_join(IRHD, WSHFC_cleaned, by = "property_id")
no_match_irhd <- no_match_irhd %>% drop_na(property_id)
write.csv(no_match_irhd, review_after_join_wshfc, row.names=FALSE)

## c) Identify changes - IRHD and WSHFC_cleaned. Create long-form for easy comparison

rectify <- identify_changes_irhd(IRHD, WSHFC_cleaned, 'property_id')

## d) Identify which rows will be updated with new WSHFC data, or keep existing data

# Subset 1) select records with no data in the IRHD - we will take new data from WSHFC
subset1 <- rectify %>% subset((is.na(variable_value.x)| variable_value.x == ""), select = c(ID, property_id, variable_class,variable_value.x,variable_value.y,match, select))
subset1$select <- subset1$variable_value.y
rectify <- anti_join(rectify, subset1, by=c("ID"="ID")) # remove from rectify
updates <- subset1
rm(subset1)

# Subset 2) Below fields - select WHSFC data
subset2 <- rectify %>% subset((variable_class == "in_service_date" |
                                    variable_class == "manager"| variable_class == "property_owner"|
                                    variable_class == "project_id"| variable_class == "disabled"|
                                    variable_class == "homeless"| variable_class == "senior"|
                                    variable_class == "bed_count"| variable_class == "property_name"|
                                    variable_class == "site_type"| variable_class == "funding_sources"|
                                    variable_class == "HOMEcity"| variable_class == "HOMEcounty"| variable_class == "HOMEstate"|
                                    variable_class == "expiration_date"| variable_class == "large_household"|
                                    variable_class == "project_name"), select = c(ID, property_id, variable_class,variable_value.x,variable_value.y,match, select))
subset2$select <- subset2$variable_value.y
rectify <- anti_join(rectify, subset2, by=c("ID"="ID")) # remove from rectify
updates <- rbind(updates, subset2)
rm(subset2)

# Subset 3) select addresses that have "multiple" in the field - use IRHD address
subset3 <- rectify %>% subset(str_detect(rectify$variable_value.y, str_c("Mu")), select = c(ID, property_id, variable_class,variable_value.x,variable_value.y,match, select))
subset3$select <- subset3$variable_value.x
rectify <- anti_join(rectify, subset3, by=c("ID"="ID"))# remove from rectify
updates <- rbind(updates, subset3)
rm(subset3)

# Subset 4) select all AMI/Unit count/Bedroom size data, identify small numeric changes
subset4 <- rectify %>% subset((variable_class == "total_units" | variable_class == "total_restricted_units"|
                                    variable_class == "ami_20"| variable_class == "ami_25"|
                                    variable_class == "ami_30"| variable_class == "ami_35"|
                                    variable_class == "ami_40"| variable_class == "ami_45"|
                                    variable_class == "ami_50"|
                                    variable_class == "ami_60"| variable_class == "ami_65"|
                                    variable_class == "ami_70"| variable_class == "ami_75"|
                                    variable_class == "ami_80"| variable_class == "ami_85"|
                                    variable_class == "ami_90"| variable_class == "ami_100"| variable_class == "ami_120"|
                                    variable_class == "market_rate"| variable_class == "manager_unit"|
                                    variable_class == "bedroom_0"| variable_class == "bedroom_1"| variable_class == "bedroom_2"| variable_class == "bedroom_3"|
                                    variable_class == "bedroom_4"| variable_class == "bedroom_5"| variable_class == "bedroom_unknown"|
                                    variable_class == "bed_count"), select = c(ID, property_id, variable_class,variable_value.x,variable_value.y,match, select))

# Create formula for calculating difference between numeric values
subset4_sum <- subset4 %>% group_by(property_id) %>%
  summarize(sum.x=sum(as.numeric(variable_value.x)),
            sum.y=sum(as.numeric(variable_value.y)))

# abs function - absolute value of the percentage difference
subset4_sum$diff <- abs((subset4_sum$sum.x-subset4_sum$sum.y)/subset4_sum$sum.x)

# join back to subset4 table, so each row of data now has the percentage difference
subset4 <- merge(subset4, subset4_sum, by = "property_id")
rm(subset4_sum)

# Rows with "diff" of 12% or less will be selected - we want the WSHFC data
subset4$select <- ifelse(subset4$diff <= "0.12", subset4$variable_value.y, "")

# Rows where the sum.y is 0, we keep the sum.x data (if WSHFC data is 0, we keep IRHD data)
subset4$select <- ifelse(subset4$sum.y == "0", subset4$variable_value.x, subset4$select)

# Remove "diff" of greater than 12% from subset4
subset4 <- subset4 %>% subset(!(select == ""),
                              select = c(ID, property_id, variable_class,variable_value.x,variable_value.y,match, select, sum.x, sum.y, diff))
rectify <- anti_join(rectify, subset4, by=c("ID"="ID")) # remove from rectify

subset4 <- subset4[, -c(8,9,10)]
updates <- rbind(updates, subset4)
rm(subset4)

# Subset 5) If WSHFC field is blank, select IRHD data
subset5 <- rectify %>% subset((is.na(variable_value.y)| variable_value.y == ""),
                              select = c(ID, property_id, variable_class,variable_value.x,variable_value.y,match, select))
subset5$select <- subset5$variable_value.x
rectify <- anti_join(rectify, subset5, by=c("ID"="ID")) # remove from rectify
updates <- rbind(updates, subset5)
rm(subset5)


# Subset 6-10) Various manual selections
subset6 <- rectify %>% subset(str_detect(rectify$variable_value.y, str_c("303 Howell Way & 417 3rd Ave, Edmonds, WA 98020")),
                              select = c(ID, property_id, variable_class,variable_value.x,variable_value.y,match, select))
subset6$select <- subset6$variable_value.x
rectify <- anti_join(rectify, subset6, by=c("ID"="ID"))# remove from rectify
updates <- rbind(updates, subset6)
rm(subset6)

subset7 <- rectify %>% subset(str_detect(rectify$variable_value.y, " Rainier Ave, Everett, WA 98201"),
                              select = c(ID, property_id, variable_class,variable_value.x,variable_value.y,match, select))
subset7$select <- subset7$variable_value.x
rectify <- anti_join(rectify, subset7, by=c("ID"="ID"))# remove from rectify
updates <- rbind(updates, subset7)
rm(subset7)

subset8 <- rectify %>% subset(str_starts(rectify$variable_value.y, ("[:alpha:]")),
                              select = c(ID, property_id, variable_class,variable_value.x,variable_value.y,match, select))
subset8$select <- subset8$variable_value.x
rectify <- anti_join(rectify, subset8, by=c("ID"="ID"))# remove from rectify
updates <- rbind(updates, subset8)
rm(subset8)

# Subset 9 selects the existing IRHD data over the new WSHFC data - selected since the new data appears "weird" or I confirmed the data online, etc. Somewhat arbitrary
subset9 <- rectify %>% subset(str_detect(rectify$property_id, "18015|18016|16100|16101|16402|16002|18092|16002|17394|16408|17832|16445|16964|18086|17951|18181|16269|16794|18320|16707|18422|18379|18436"),
                              select = c(ID, property_id, variable_class,variable_value.x,variable_value.y,match, select))
subset9$select <- subset9$variable_value.x
rectify <- anti_join(rectify, subset9, by=c("ID"="ID"))# remove from rectify
updates <- rbind(updates, subset9)
rm(subset9)

# Subset 10 selects the new WSHFC data over the existing IRHD data - selected since the new data appears "legit". Pretty darn arbitrary
subset10 <- rectify %>% subset(str_detect(rectify$property_id, "18210|16044|16774|16725|16158|16905|17438"), select = c(ID, property_id, variable_class,variable_value.x,variable_value.y,match, select))
subset10$select <- subset10$variable_value.y
rectify <- anti_join(rectify, subset10, by=c("ID"="ID"))# remove from rectify
updates <- rbind(updates, subset10)
rm(subset10)

# Subset 11+) As directed by housing authorities 
#Everett Housing Authority
subset11 <- rectify %>% subset(str_detect(rectify$property_id, "15905|15932|15961|16024|16593|17818|17820|17821|18107|18108|18109|18110|17749|17748"),
                               select = c(ID, property_id, variable_class,variable_value.x,variable_value.y,match, select))
subset11$select <- subset11$variable_value.x
rectify <- anti_join(rectify, subset11, by=c("ID"="ID"))# remove from rectify
updates <- rbind(updates, subset11)
rm(subset11)

# If needed, export remaining WSHFC records and contact the corresponding housing authority
# rectify_export <- rectify %>%
#   inner_join(IRHD, by='property_id')
# 
# rectify_export = rectify_export[,c("ID","property_id","variable_class","variable_value.x","variable_value.y","data_source","project_name","property_owner","in_service_date", "county","cleaned_address")]
#write.csv(rectify_export, review_after_join_housingauthorities, row.names=FALSE)

#All remaining changes (select newer WSHFC data - assuming it is correct)
subset14 <- rectify %>% subset((rectify$select == ""),
                               select = c(ID, property_id, variable_class,variable_value.x,variable_value.y,match, select))
subset14$select <- subset14$variable_value.y
rectify <- anti_join(rectify, subset14, by=c("ID"="ID"))# remove from rectify
updates <- rbind(updates, subset14)
rm(subset14)

## e) Take "updates" data and update IRHD records, join cleaned KC data, create IRHD_clean table

IRHD_clean <- update_irhd(IRHD, updates, 'property_id')

# Add in new properties identified in new_wshfc as well as all cleaned KC data
IRHD_clean <- rbind(IRHD_clean, new_wshfc, KC_cleaned, fill=TRUE)

# Clean up before export to housing authorities
IRHD_clean <- ami_cleanup(IRHD_clean)
IRHD_clean <- unitsize_cleanup(IRHD_clean)
IRHD_clean <- datayear_cleanup(IRHD_clean)
IRHD_clean <- create_workingid(IRHD_clean)

## STEP 4: Send prelim IRHD to data providers for review  -------------------------
## a) Export for review by housing authorities, ask for new properties, remove out-of-service properties, etc

# # Export IRHD_clean for review
# county_kitsap_review <- IRHD_clean %>%
#   filter(IRHD_clean$county == "Kitsap")
# 
# county_pierce_review <- IRHD_clean %>%
#   filter(IRHD_clean$county == "Pierce")
# 
# county_snohomish_review <- IRHD_clean %>%
#   filter(IRHD_clean$county == "Snohomish")
# 
# # Create a blank workbook
# final_review_export <- createWorkbook()
# 
# # Add some sheets to the workbook
# addWorksheet(final_review_export, "Kitsap")
# addWorksheet(final_review_export, "Pierce")
# addWorksheet(final_review_export, "Snohomish")
# 
# # Write the data to the sheets
# writeData(final_review_export, sheet = "Kitsap", x = county_kitsap_review)
# writeData(final_review_export, sheet = "Pierce", x = county_pierce_review)
# writeData(final_review_export, sheet = "Snohomish", x = county_snohomish_review)
# 
# # Export the file
# saveWorkbook(final_review_export, final_review_housingauthorities, overwrite = TRUE)

## STEP 5: Incorporate any changes from data providers -------------------------
# a) Add new properties, remove out-of-service, update records as needed
new <- updates_received %>% filter(`Reviewer Comments` == "New Property") %>% select(-`Reviewer Comments`)
remove <- updates_received %>% filter(`Reviewer Comments` == "remove") %>% select(-`Reviewer Comments`)

IRHD_clean <- rbind(IRHD_clean, new, fill = TRUE)
IRHD_clean <- anti_join(IRHD_clean, remove, by=c("working_id" = "working_id"))

rectify <- identify_changes_irhd(IRHD_clean, updates_received, 'working_id')

# Select new data from provider
subset15 <- rectify %>% filter(rectify$select == "")
subset15$select <- subset15$variable_value.y
rectify <- anti_join(rectify, subset15, by=c("ID"="ID"))# remove from rectify
updates <- subset15
rm(subset15)

# Take "updates" data and update IRHD records
IRHD_clean <- update_irhd(IRHD_clean, updates, 'working_id')

## STEP 6: Final clean up and push to Elmer -------------------------
## a) Final Cleanup
IRHD_clean <- ami_cleanup(IRHD_clean)
IRHD_clean <- unitsize_cleanup(IRHD_clean)
IRHD_clean <- datayear_cleanup(IRHD_clean)
IRHD_clean <- create_workingid(IRHD_clean)

# check for any duplicates - hopefully 0!
dups <- IRHD_clean %>%
  unique() %>%
  group_by(`property_id`) %>%
  mutate(n = n()) %>%
  filter(n > 1)
dups <- filter(dups, !is.na(property_id))

dups <- IRHD_clean %>%
  unique() %>%
  group_by(`working_id`) %>%
  mutate(n = n()) %>%
  filter(n > 1)
dups <- filter(dups, !is.na(working_id))

# clean up environment
rm(updates_received, no_match_irhd, KC_cleaned, dups, updates, new_wshfc, IRHD_raw, IRHD, WSHFC_cleaned, rectify, new, remove)

## b) Summary table by County and AMI/Unit Size
IRHD_county_bedrooms <- summary_county_bedrooms(IRHD_clean)
IRHD_county_ami <- summary_county_ami(IRHD_clean)

## c) Explore new units
new_IRHD <- IRHD_clean %>%
  filter(IRHD_clean$in_service_date == vintage_year)

new_IRHD_county_bedrooms <- summary_county_bedrooms(new_IRHD)
new_IRHD_county_ami <- summary_county_ami(new_IRHD)
new_IRHD_county <- summary_county(new_IRHD)

## d) Export to Elmer IRHD_clean
# dbWriteTable(conn = elmer_connection, name = table_id, value = IRHD_clean, overwrite = TRUE)
# dbExecute(conn=elmer_connection, statement=sql_export)
# dbDisconnect(elmer_connection)
