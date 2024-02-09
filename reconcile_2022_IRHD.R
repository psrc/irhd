# TITLE: Reconcile IRHD and new data
# GEOGRAPHIES: King, Snohomish, Pierce, Kitsap
# DATA SOURCE: WSHFC, HASCO, THA, King County, EHA, PCHA, BHA
# DATE MODIFIED: 12.04.2023
# AUTHOR: Eric Clute

## assumptions -------------------------
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

remotes::install_github("slu-openGIS/postmastr")

elmer_connection <- dbConnect(odbc::odbc(),
                              driver = "SQL Server",
                              server = "AWS-PROD-SQL\\Sockeye",
                              database = "Elmer",
                              trusted_connection = "yes")

review_after_join_housingauthorities <- "./Export4review-housingauthorities.csv" # Export for review after WSHFC-IRHD join. Help understanding why property data are changing
review_after_join_wshfc <- "./Export4review-wshfc.csv" # Export for review after WSHFC-IRHD join. Help understanding why property data are missing, different, etc
final_review_housingauthorities <- "./final_review_housingauthorities.xlsx" # Export final dataset for review. Ask housing authorities to flag errors, add new properties, remove out-of-service properties, etc
#HASCO_updates_path <- ""
#THA_updates_path <- ""

address_func <- "./address_match.R"
irhd_func <- "./irhd_cleaning_func.R"
wshfc_clean_script <- "./clean_2022_WSHFC_data.R"
kc_clean_script <- "./clean_2022_KC_data.R"

source(address_func)
source(irhd_func)

`%not_in%` <- Negate(`%in%`)
vintage_year <- 2022
last_vintage <- vintage_year - 1

sql_import <- paste('irhd.properties')
sql_export <- paste('exec irhd.merge_irhd_properties', vintage_year)

## 1) load data -------------------------

# load all IRHD records from Elmer
IRHD_raw <- dbReadTable(elmer_connection, SQL(sql_import))

# load cleaned WSHFC data
source(wshfc_clean_script)

# load cleaned data from data partners
#source(kc_clean_script)

# Only keep fields where we have new data (in the "Corrected" column)
# HASCO_raw <- fread(HASCO_updates_path)
# HASCO <- HASCO_raw %>%
#   drop_na(Corrected)
# 
# THA_raw <- fread(THA_updates_path)
# THA <- THA_raw %>%
#   drop_na(Corrected)

## 2) Final tweaks -------------------------

IRHD_raw <- IRHD_raw %>% filter(data_year == last_vintage)
IRHD <- IRHD_raw %>% filter(!(county == "King")) # King county handled separately
IRHD %<>% select(-c(created_at,updated_at,sro,shape,irhd_property_id)) # Remove unneeded fields

## 3) Locate records in WSHFC not in IRHD (likely new records/properties) -------------------------

new_wshfc <- anti_join(WSHFC_cleaned, IRHD, by = "property_id")

## 4) Locate records in IRHD not in WSHFC_cleaned (No longer in WSHFC data. Will need to be verified (did they go offline, etc?)) -------------------------

no_match_irhd <- anti_join(IRHD, WSHFC_cleaned, by = "property_id")
no_match_irhd <- no_match_irhd %>% drop_na(property_id)
write.csv(no_match_irhd, review_after_join_wshfc, row.names=FALSE)

## 5) Identify matched records in IRHD and WSHFC_cleaned -------------------------

# Pivot the IRHD data to make it long and thin
long_IRHD <- IRHD %>%
  pivot_longer(c('project_id',
                 'project_name',
                 'property_name',
                 'property_owner',
                 'manager',
                 'in_service_date',
                 'expiration_date',
                 'cleaned_address',
                 'county',
                 'total_units',
                 'total_restricted_units',
                 'ami_20','ami_25','ami_30','ami_35','ami_40','ami_45','ami_50','ami_60','ami_65','ami_70','ami_75','ami_80','ami_85','ami_90','ami_100',
                 'market_rate',
                 'manager_unit',
                 'bedroom_0','bedroom_1','bedroom_2','bedroom_3','bedroom_4','bedroom_5','bedroom_unknown',
                 'bed_count',
                 'site_type',
                 'HOMEcity',
                 'HOMEcounty',
                 'HOMEstate',
                 'confidentiality',
                 'policy',
                 'senior',
                 'disabled',
                 'homeless',
                 'transitional',
                 'veterans',
                 'funding_sources',
                 'tenure'),
               names_to='variable_class',
               values_to='variable_value',
               values_transform = list(variable_value=as.character))

# Remove some fields that we don't need here
long_IRHD %<>% select(c(property_id,variable_class,variable_value))

# Pivot the mocked-up data to make it long and thin
long_WSHFC <- WSHFC_cleaned %>%
  pivot_longer(c('project_id',
                 'project_name',
                 'property_name',
                 'property_owner',
                 'manager',
                 'in_service_date',
                 'expiration_date',
                 'cleaned_address',
                 'county',
                 'total_units',
                 'total_restricted_units',
                 'ami_20','ami_25','ami_30','ami_35','ami_40','ami_45','ami_50','ami_60','ami_65','ami_70','ami_75','ami_80','ami_85','ami_90','ami_100',
                 'market_rate',
                 'manager_unit',
                 'bedroom_0','bedroom_1','bedroom_2','bedroom_3','bedroom_4','bedroom_5','bedroom_unknown',
                 'bed_count',
                 'site_type',
                 'HOMEcity',
                 'HOMEcounty',
                 'HOMEstate',
                 'confidentiality',
                 'policy',
                 'senior',
                 'disabled',
                 'homeless',
                 'transitional',
                 'veterans',
                 'funding_sources',
                 'tenure'),
               names_to='variable_class',
               values_to='variable_value',
               values_transform = list(variable_value=as.character))

# Remove some fields that we don't need here
long_WSHFC %<>% select(c(property_id,variable_class,variable_value))


# Compare the two data sets in long form to identify values that have been changed
rectify <- long_IRHD %>%
  inner_join(long_WSHFC, by=c('property_id', 'variable_class')) %>%
  mutate("match" = ifelse(mapply(identical, variable_value.x, variable_value.y), "YES", "NO")) %>%
  filter(match == "NO") %>%
  drop_na(variable_value.y)

## 6) Identify which rows will be updated with new WSHFC data, or keep existing data -------------------------

# Create field to indicate which variable to use
rectify$select <- ""
rectify <- tibble::rowid_to_column(rectify, "ID")

# Subset 1) select records with no data in the IRHD - we will take new data from WSHFC
subset1 <- rectify %>% subset((is.na(variable_value.x)| variable_value.x == ""), select = c(ID, property_id, variable_class,variable_value.x,variable_value.y,match, select))
subset1$select <- subset1$variable_value.y
rectify <- anti_join(rectify, subset1, by=c("ID"="ID")) # remove from rectify
updates <- subset1
rm(subset1)

# Subset 2) Below fields - select WHSFC data
subset2 <- rectify %>% subset((variable_class == "in_service_date" |
                                    variable_class == "manager"|
                                    variable_class == "property_owner"|
                                    variable_class == "project_id"|
                                    variable_class == "disabled"|
                                    variable_class == "homeless"|
                                    variable_class == "senior"|
                                    variable_class == "bed_count"|
                                    variable_class == "property_name"|
                                    variable_class == "site_type"|
                                    variable_class == "funding_sources"|
                                    variable_class == "HOMEcity"|
                                    variable_class == "HOMEcounty"|
                                    variable_class == "HOMEstate"|
                                    variable_class == "expiration_date"|
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
subset4 <- rectify %>% subset((variable_class == "total_units" |
                                    variable_class == "total_restricted_units"|
                                    variable_class == "ami_20"|
                                    variable_class == "ami_25"|
                                    variable_class == "ami_30"|
                                    variable_class == "ami_35"|
                                    variable_class == "ami_40"|
                                    variable_class == "ami_45"|
                                    variable_class == "ami_50"|
                                    variable_class == "ami_60"|
                                    variable_class == "ami_65"|
                                    variable_class == "ami_70"|
                                    variable_class == "ami_75"|
                                    variable_class == "ami_80"|
                                    variable_class == "ami_85"|
                                    variable_class == "ami_90"|
                                    variable_class == "ami_100"|
                                    variable_class == "market_rate"|
                                    variable_class == "manager_unit"|
                                    variable_class == "bedroom_0"|
                                    variable_class == "bedroom_1"|
                                    variable_class == "bedroom_2"|
                                    variable_class == "bedroom_3"|
                                    variable_class == "bedroom_4"|
                                    variable_class == "bedroom_5"|
                                    variable_class == "bedroom_unknown"|
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
subset4 <- subset4 %>% subset(!(select == ""), select = c(ID, property_id, variable_class,variable_value.x,variable_value.y,match, select, sum.x, sum.y, diff))
rectify <- anti_join(rectify, subset4, by=c("ID"="ID")) # remove from rectify

subset4 <- subset4[, -c(8,9,10)]
updates <- rbind(updates, subset4)
rm(subset4)

# Subset 5) If WSHFC field is blank, select IRHD data
subset5 <- rectify %>% subset((is.na(variable_value.y)| variable_value.y == ""), select = c(ID, property_id, variable_class,variable_value.x,variable_value.y,match, select))
subset5$select <- subset5$variable_value.x
rectify <- anti_join(rectify, subset5, by=c("ID"="ID")) # remove from rectify
updates <- rbind(updates, subset5)
rm(subset5)


# Subset 6-10) Various manual selections
subset6 <- rectify %>% subset(str_detect(rectify$variable_value.y, str_c("303 Howell Way & 417 3rd Ave, Edmonds, WA 98020")), select = c(ID, property_id, variable_class,variable_value.x,variable_value.y,match, select))
subset6$select <- subset6$variable_value.x
rectify <- anti_join(rectify, subset6, by=c("ID"="ID"))# remove from rectify
updates <- rbind(updates, subset6)
rm(subset6)

subset7 <- rectify %>% subset(str_detect(rectify$variable_value.y, " Rainier Ave, Everett, WA 98201"), select = c(ID, property_id, variable_class,variable_value.x,variable_value.y,match, select))
subset7$select <- subset7$variable_value.x
rectify <- anti_join(rectify, subset7, by=c("ID"="ID"))# remove from rectify
updates <- rbind(updates, subset7)
rm(subset7)

subset8 <- rectify %>% subset(str_starts(rectify$variable_value.y, ("[:alpha:]")), select = c(ID, property_id, variable_class,variable_value.x,variable_value.y,match, select))
subset8$select <- subset8$variable_value.x
rectify <- anti_join(rectify, subset8, by=c("ID"="ID"))# remove from rectify
updates <- rbind(updates, subset8)
rm(subset8)

# Subset 9 selects the existing IRHD data over the new WSHFC data - selected since the new data appears "weird" or I confirmed the data online, etc. Somewhat arbitrary
subset9 <- rectify %>% subset(str_detect(rectify$property_id, "18015|18016|16100|16101|16402|16002|18092|16002|17394|16408|17832|16445|16964|18086|17951|18181|16269|16794|18320|16707|18422|18379|18436"), select = c(ID, property_id, variable_class,variable_value.x,variable_value.y,match, select))
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

# Subset 11-14) As directed by housing authorities 
#Everett Housing Authority
subset11 <- rectify %>% subset(str_detect(rectify$property_id, "15905|15932|15961|16024|16593|17818|17820|17821|18107|18108|18109|18110|17749|17748"), select = c(ID, property_id, variable_class,variable_value.x,variable_value.y,match, select))
subset11$select <- subset11$variable_value.x
rectify <- anti_join(rectify, subset11, by=c("ID"="ID"))# remove from rectify
updates <- rbind(updates, subset11)
rm(subset11)

# If needed, export remaining WSHFC records and contact the corresponding housing authority
rectify_export <- rectify %>%
  inner_join(IRHD, by='property_id')

rectify_export = rectify_export[,c("ID","property_id","variable_class","variable_value.x","variable_value.y","data_source","project_name","property_owner","in_service_date", "county","cleaned_address")]
#write.csv(rectify_export, review_after_join_housingauthorities, row.names=FALSE)

#Snohomish County Housing Authority
# subset12 <- rectify %>%
#   inner_join(HASCO, join_by(property_id == property_id, variable_class == Variable))
# subset12$select <- subset12$Corrected
# subset12 <- subset12 %>% 
#   rename("ID" = "ID.x")
# subset12 %<>% select(c(ID,property_id,variable_class,variable_value.x,variable_value.y,match,select))
# rectify <- anti_join(rectify, subset12, by=c("ID"="ID"))# remove from rectify
# updates <- rbind(updates, subset12)
# rm(subset12)

#Tacoma Housing Authority
# subset13 <- rectify %>%
#   inner_join(THA, join_by(property_id == property_id, variable_class == Variable))
# subset13$select <- subset13$Corrected
# subset13 <- subset13 %>% 
#   rename("ID" = "ID.x")
# subset13 %<>% select(c(ID,property_id,variable_class,variable_value.x,variable_value.y,match,select))
# rectify <- anti_join(rectify, subset13, by=c("ID"="ID"))# remove from rectify
# updates <- rbind(updates, subset13)
# rm(subset13)

#All remaining changes (select newer WSHFC data - assuming it is correct)
subset14 <- rectify %>% subset((rectify$select == ""), select = c(ID, property_id, variable_class,variable_value.x,variable_value.y,match, select))
subset14$select <- subset14$variable_value.y
rectify <- anti_join(rectify, subset14, by=c("ID"="ID"))# remove from rectify
updates <- rbind(updates, subset14)
rm(subset14)


## 7) Take "updates" data and update IRHD records, create IRHD_clean table -------------------------
# Transform "updates" for updating existing IRHD
updates <- updates %>% pivot_wider(id_cols = c('property_id'), names_from = 'variable_class', values_from = 'select') %>%
  setDT()

class(updates$project_id) = "character"
class(updates$total_units) = "numeric"
class(updates$total_restricted_units) = "numeric"
class(updates$in_service_date) = "character"
class(updates$zip) = "character"
class(updates$ami_20) = "numeric"
class(updates$ami_30) = "numeric"
class(updates$ami_35) = "numeric"
class(updates$ami_40) = "numeric"
class(updates$ami_45) = "numeric"
class(updates$ami_50) = "numeric"
class(updates$ami_60) = "numeric"
class(updates$ami_65) = "numeric"
class(updates$ami_80) = "numeric"
class(updates$market_rate) = "numeric"
class(updates$manager_unit) = "numeric"
class(updates$bedroom_0) = "numeric"
class(updates$bedroom_1) = "numeric"
class(updates$bedroom_2) = "numeric"
class(updates$bedroom_3) = "numeric"
class(updates$bedroom_4) = "numeric"
class(updates$bedroom_unknown) = "numeric"
class(updates$bed_count) = "numeric"
class(updates$senior) = "numeric"
class(updates$HOMEcity) = "numeric"
class(updates$HOMEcounty) = "numeric"
class(updates$HOMEstate) = "numeric"
class(updates$homeless) = "numeric"
class(updates$disabled) = "numeric"

# Create new clean IRHD file
IRHD_clean <- copy(IRHD)
setDT(IRHD_clean)

# Update records as determined by the "updates" dataframe
shared_fields <- intersect(names(updates), names(IRHD_clean))                                        # fields in common
dupes <- IRHD_clean[duplicated(property_id), cbind(.SD[1], number=.N), by=property_id] %>%            # duplicates (to exclude)
  pull(working_id)
blankfill <- IRHD_clean %>%                                                                           # create IRHD data that matches fields from updates
  .[!is.na(property_id) & working_id %not_in% (dupes), (colnames(.) %in% shared_fields), with=FALSE]  # include only common records, no duplicate keys
updates %<>% rows_patch(blankfill, by="property_id", unmatched="ignore")                             # replace NA in `updates` with values from `IRHD_clean`
IRHD_clean %<>% .[updates, (shared_fields):=mget(paste0("i.", shared_fields)), on=.(property_id)]    # carry over all matching variables from updates
rm(dupes, blankfill, shared_fields, long_IRHD, long_WSHFC, rectify)                                   # Clean up

# Add in new properties identified in new_wshfc
IRHD_clean <- bind_rows(IRHD_clean, new_wshfc)

# Clean up before export to housing authorities
IRHD_clean <- ami_cleanup(IRHD_clean)
IRHD_clean <- unitsize_cleanup(IRHD_clean)
IRHD_clean <- datayear_cleanup(IRHD_clean)

## 8) Export for review by housing authorities, ask for new properties, remove out-of-service properties, etc -------------------------
# Export IRHD_clean for review

county_kitsap_review <- IRHD_clean %>%
  filter(IRHD_clean$county == "Kitsap")

county_pierce_review <- IRHD_clean %>%
  filter(IRHD_clean$county == "Pierce")

county_snohomish_review <- IRHD_clean %>%
  filter(IRHD_clean$county == "Snohomish")

# Create a blank workbook
final_review_export <- createWorkbook()

# Add some sheets to the workbook
addWorksheet(final_review_export, "Kitsap")
addWorksheet(final_review_export, "Pierce")
addWorksheet(final_review_export, "Snohomish")

# Write the data to the sheets
writeData(final_review_export, sheet = "Kitsap", x = county_kitsap_review)
writeData(final_review_export, sheet = "Pierce", x = county_pierce_review)
writeData(final_review_export, sheet = "Snohomish", x = county_snohomish_review)

# Export the file
saveWorkbook(final_review_export, final_review_housingauthorities)

# Add new properties, remove out-of-service, update records as needed




## 9) Join IRHD_clean table with cleaned data from King County -------------------------
# IRHD_clean <- rbind(IRHD_clean, KC_cleaned,fill=TRUE)

## 10) Final Cleanup ----------------------
IRHD_clean <- create_workingid(IRHD_clean)
IRHD_clean <- ami_cleanup(IRHD_clean)
IRHD_clean <- unitsize_cleanup(IRHD_clean)
IRHD_clean <- datayear_cleanup(IRHD_clean)

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

## 11) Summary table by County and AMI/Unit Size -------------------------
IRHD_county_bedrooms <- summary_county_bedrooms(IRHD_clean)
IRHD_county_ami <- summary_county_ami(IRHD_clean)

## 12) Explore new units -------------------------
new_IRHD <- IRHD_clean %>%
  filter(IRHD_clean$in_service_date == vintage_year)

new_IRHD_county_bedrooms <- summary_county_bedrooms(new_IRHD)
new_IRHD_county_ami <- summary_county_ami(new_IRHD)
new_IRHD_county <- summary_county(new_IRHD)

## 13) Export to Elmer IRHD_clean -------------------------
# table_id <- Id(schema = "stg", table = "irhd")
# dbWriteTable(conn = elmer_connection, name = table_id, value = IRHD_clean, overwrite = TRUE)
# dbExecute(conn=elmer_connection, statement=sql_export)
# dbDisconnect(elmer_connection)