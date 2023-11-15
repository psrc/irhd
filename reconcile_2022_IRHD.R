# TITLE: Reconcile IRHD and new data
# GEOGRAPHIES: King, Snohomish, Pierce, Kitsap
# DATA SOURCE: WSHFC, HASCO, THA, King County, EHA, PCHA, BHA
# DATE MODIFIED: 11.14.2023
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
remotes::install_github("slu-openGIS/postmastr")

elmer_connection <- dbConnect(odbc::odbc(),
                              driver = "SQL Server",
                              server = "AWS-PROD-SQL\\Sockeye",
                              database = "Elmer",
                              trusted_connection = "yes")

WSHFC_path <- "J:/Projects/IncomeRestrictedHsgDB/2022 vintage/Data/WSHFC/WSHFC_2022_cleaned.csv"
export_4review_housingauthorities <- "C:/Users/eclute/GitHub/irhd/Export4review-housingauthorities.csv"
export_4review_wshfc <- "C:/Users/eclute/GitHub/irhd/Export4review-wshfc.csv"
#HASCO_updates_path <- "J:/Projects/IncomeRestrictedHsgDB/2022 vintage/Review Files - Received/PSRC_2022_IRHD_Snohomish_minor updates.csv"
#THA_updates_path <- "J:/Projects/IncomeRestrictedHsgDB/2022 vintage/Review Files - Received/PSRC_2022_IRHD_Pierce_THA_minor updates.csv"
#KC_path <- "J:/Projects/IncomeRestrictedHsgDB/2022 vintage/Review Files - Received/King County Income-restricted Housing Database 2022.csv"
address_script <- "C:/Users/eclute/GitHub/irhd/address_match.R"
summary_func <- "C:/Users/eclute/GitHub/irhd/summary_func_irhd.R"

source(address_script)
source(summary_func)

`%not_in%` <- Negate(`%in%`)
vintage_year <- 2022
last_vintage <- vintage_year - 1

sql_import <- paste('irhd.properties')
sql_export <- paste('exec irhd.merge_irhd_properties', vintage_year)

## 1) load data -------------------------

# load all IRHD records from Elmer
IRHD_raw <- dbReadTable(elmer_connection, SQL(sql_import))

# borrow datatype characterization from IRHD to apply to identical columns in WSHFC data
# irhd_colClasses = sapply(IRHD_raw, class)
# names(irhd_colClasses) <- colnames(IRHD_raw)
# WSHFC_cols = colnames(read.csv(WSHFC_path, nrows=1))
# wshfc_colClasses <- irhd_colClasses %>% .[names(.) %in% WSHFC_cols]

# load cleaned WSHFC data that has portfolios as of end of 2022; apply datatypes to match
# WSHFC_raw <- fread(WSHFC_path, colClasses=wshfc_colClasses)
WSHFC_raw <- fread(WSHFC_path)

# load cleaned KC data that has portfolios as of end of 2022
# KC_raw <- fread(KC_path)

# load cleaned HASCO & THA data - only keep fields where we have new data (in the "Corrected" column)
# HASCO_raw <- fread(HASCO_updates_path)
# HASCO <- HASCO_raw %>%
#   drop_na(Corrected)
# 
# THA_raw <- fread(THA_updates_path)
# THA <- THA_raw %>%
#   drop_na(Corrected)

## 2) clean up data -------------------------

# IRHD ---
IRHD_raw <- IRHD_raw %>% filter(data_year == last_vintage)
IRHD <- IRHD_raw %>% filter(!(county == "King"))  # King county handled separately

# Remove unneeded fields
IRHD %<>% select(-c(created_at,updated_at,sro,shape))
IRHD$property_id <- as.integer(IRHD$property_id)

# King County finalized data ---
# KC <- KC_raw
# KC$county <- "King"
# KC %<>% filter(KC$in_service_date <= vintage_year | is.na(KC$in_service_date))

# Remove fields we don't need
##(Policy field is blank, data currently stored in "FundingSource" - This may change!! Watch next year)
# KC %<>% select(-c(unique_linking_ID,HITS_survey,GeoCode_Street,GeoCode_City,ProjectType,Policy))

# Rename fields to match IRHD
# KC <- KC %>% 
#   rename("data_source" = "DataSourceName",
#          "bed_count" = "GroupHomeOrBed",
#          "zip" = "GeoCode_Zip",
#          "full_address" = "address_standardized",
#          "expiration_date" = "ExpirationYear",
#          "property_owner" = "ProjectSponsor",
#          "manager" = "ContactName",
#          "site_type" = "PopulationServed",
#          "funding_sources" = "Funder",
#          "HOME" = "HOMEUnits",
#          "policy" = "FundingSource")
# 
# KC$cleaned_address <- str_c(KC$full_address,', ',KC$city,', WA, ',KC$zip)

# Identify and remove duplicated working_id value
# dups <- filter(KC, working_id == "SH_5215")
# KC[1222,1]<-"SH_7234"
# rm(dups)

## 2) Locate records in WSHFC_raw not in IRHD (likely new records/properties) -------------------------

newWSHFC <- anti_join(WSHFC_raw, IRHD, by = "property_id")

## 3) Locate records in IRHD not in WSHFC (No longer in WSHFC data. Will need to be verified (did they go offline, etc?)) -------------------------

nomatchIRHD <- anti_join(IRHD, WSHFC_raw, by = "property_id")
nomatchIRHD <- nomatchIRHD %>% drop_na(property_id)
write.csv(nomatchIRHD, export_4review_wshfc, row.names=FALSE)

## 4) Identify matched records in IRHD and WSHFC -------------------------

# Pivot the IRHD data to make it long and thin
long_IRHD <- IRHD %>%
  pivot_longer(c('project_id',
                 'project_name',
                 'property_name',
                 'property_owner',
                 'manager',
                 'in_service_date',
                 'expiration_date',
                # 'cleaned_address',
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
long_WSHFC <- WSHFC_raw %>%
  pivot_longer(c('project_id',
                 'project_name',
                 'property_name',
                 'property_owner',
                 'manager',
                 'in_service_date',
                 'expiration_date',
              #   'cleaned_address',
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
long_compare <- long_IRHD %>%
  inner_join(long_WSHFC, by=c('property_id', 'variable_class')) %>%
  mutate("match" = ifelse(mapply(identical, variable_value.x, variable_value.y), "YES", "NO")) %>%
  filter(match == "NO") %>%
  drop_na(variable_value.y)

## 5) Identify which rows will be updated with new WSHFC data, or keep existing data -------------------------

# Create field to indicate which variable to use
long_compare$select <- ""
long_compare <- tibble::rowid_to_column(long_compare, "ID")

# Subset 1) select records with no data in the IRHD - we will take new data from WSHFC
subset1 <- long_compare %>% subset((is.na(variable_value.x)| variable_value.x == ""), select = c(ID, property_id, variable_class,variable_value.x,variable_value.y,match, select))
subset1$select <- subset1$variable_value.y
long_compare <- anti_join(long_compare, subset1, by=c("ID"="ID")) # remove from long_compare
selected <- subset1
rm(subset1)

# Subset 2) Below fields - select WHSFC data
subset2 <- long_compare %>% subset((variable_class == "in_service_date" |
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
long_compare <- anti_join(long_compare, subset2, by=c("ID"="ID")) # remove from long_compare
selected <- rbind(selected, subset2)
rm(subset2)

# Subset 3) select addresses that have "multiple" in the field - use IRHD address
subset3 <- long_compare %>% subset(str_detect(long_compare$variable_value.y, str_c("Mu")), select = c(ID, property_id, variable_class,variable_value.x,variable_value.y,match, select))
subset3$select <- subset3$variable_value.x
long_compare <- anti_join(long_compare, subset3, by=c("ID"="ID"))# remove from long_compare
selected <- rbind(selected, subset3)
rm(subset3)

# Subset 4) select all AMI/Unit count/Bedroom size data, identify small numeric changes
subset4 <- long_compare %>% subset((variable_class == "total_units" |
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
long_compare <- anti_join(long_compare, subset4, by=c("ID"="ID")) # remove from long_compare

subset4 <- subset4[, -c(8,9,10)]
selected <- rbind(selected, subset4)
rm(subset4)

# Subset 5) If WSHFC field is blank, select IRHD data
subset5 <- long_compare %>% subset((is.na(variable_value.y)| variable_value.y == ""), select = c(ID, property_id, variable_class,variable_value.x,variable_value.y,match, select))
subset5$select <- subset5$variable_value.x
long_compare <- anti_join(long_compare, subset5, by=c("ID"="ID")) # remove from long_compare
selected <- rbind(selected, subset5)
rm(subset5)


# Subset 6-10) Various manual selections
subset6 <- long_compare %>% subset(str_detect(long_compare$variable_value.y, str_c("303 Howell Way & 417 3rd Ave, Edmonds, WA 98020")), select = c(ID, property_id, variable_class,variable_value.x,variable_value.y,match, select))
subset6$select <- subset6$variable_value.x
long_compare <- anti_join(long_compare, subset6, by=c("ID"="ID"))# remove from long_compare
selected <- rbind(selected, subset6)
rm(subset6)

subset7 <- long_compare %>% subset(str_detect(long_compare$variable_value.y, " Rainier Ave, Everett, WA 98201"), select = c(ID, property_id, variable_class,variable_value.x,variable_value.y,match, select))
subset7$select <- subset7$variable_value.x
long_compare <- anti_join(long_compare, subset7, by=c("ID"="ID"))# remove from long_compare
selected <- rbind(selected, subset7)
rm(subset7)

subset8 <- long_compare %>% subset(str_starts(long_compare$variable_value.y, ("[:alpha:]")), select = c(ID, property_id, variable_class,variable_value.x,variable_value.y,match, select))
subset8$select <- subset8$variable_value.x
long_compare <- anti_join(long_compare, subset8, by=c("ID"="ID"))# remove from long_compare
selected <- rbind(selected, subset8)
rm(subset8)

# Subset 9 selects the existing IRHD data over the new WSHFC data - selected since the new data appears "weird" or I confirmed the data online, etc. Somewhat arbitrary
subset9 <- long_compare %>% subset(str_detect(long_compare$property_id, "18015|18016|16100|16101|16402|16002|18092|16002|17394|16408|17832|16445|16964"), select = c(ID, property_id, variable_class,variable_value.x,variable_value.y,match, select))
subset9$select <- subset9$variable_value.x
long_compare <- anti_join(long_compare, subset9, by=c("ID"="ID"))# remove from long_compare
selected <- rbind(selected, subset9)
rm(subset9)

# Subset 10 selects the new WSHFC data over the existing IRHD data - selected since the new data appears "legit". Pretty darn arbitrary
subset10 <- long_compare %>% subset(str_detect(long_compare$property_id, "18210|16044|16774|16725"), select = c(ID, property_id, variable_class,variable_value.x,variable_value.y,match, select))
subset10$select <- subset10$variable_value.y
long_compare <- anti_join(long_compare, subset10, by=c("ID"="ID"))# remove from long_compare
selected <- rbind(selected, subset10)
rm(subset10)

# Subset 11-14) As directed by housing authorities 
#Everett Housing Authority
subset11 <- long_compare %>% subset(str_detect(long_compare$property_id, "15905|15932|15961|16024|16593|17818|17820|17821|18107|18108|18109|18110|17749|17748"), select = c(ID, property_id, variable_class,variable_value.x,variable_value.y,match, select))
subset11$select <- subset11$variable_value.x
long_compare <- anti_join(long_compare, subset11, by=c("ID"="ID"))# remove from long_compare
selected <- rbind(selected, subset11)
rm(subset11)

# Export remaining records and contact the corresponding housing authority
export_longcompare <- long_compare %>%
  inner_join(IRHD, by='property_id')

export_longcompare = export_longcompare[,c("ID","property_id","variable_class","variable_value.x","variable_value.y","data_source","project_name","property_owner","in_service_date", "county","cleaned_address")]
write.csv(export_longcompare, export_4review_housingauthorities, row.names=FALSE)

#Snohomish County Housing Authority
# subset12 <- long_compare %>%
#   inner_join(HASCO, join_by(property_id == property_id, variable_class == Variable))
# subset12$select <- subset12$Corrected
# subset12 <- subset12 %>% 
#   rename("ID" = "ID.x")
# subset12 %<>% select(c(ID,property_id,variable_class,variable_value.x,variable_value.y,match,select))
# long_compare <- anti_join(long_compare, subset12, by=c("ID"="ID"))# remove from long_compare
# selected <- rbind(selected, subset12)
# rm(subset12)

#Tacoma Housing Authority
# subset13 <- long_compare %>%
#   inner_join(THA, join_by(property_id == property_id, variable_class == Variable))
# subset13$select <- subset13$Corrected
# subset13 <- subset13 %>% 
#   rename("ID" = "ID.x")
# subset13 %<>% select(c(ID,property_id,variable_class,variable_value.x,variable_value.y,match,select))
# long_compare <- anti_join(long_compare, subset13, by=c("ID"="ID"))# remove from long_compare
# selected <- rbind(selected, subset13)
# rm(subset13)

#All remaining changes (select newer WSHFC data - assuming it is correct)
subset14 <- long_compare %>% subset((long_compare$select == ""), select = c(ID, property_id, variable_class,variable_value.x,variable_value.y,match, select))
subset14$select <- subset14$variable_value.y
long_compare <- anti_join(long_compare, subset14, by=c("ID"="ID"))# remove from long_compare
selected <- rbind(selected, subset14)
rm(subset14)


## 6) Take "selected" data and update IRHD records, create IRHD_clean table -------------------------
# Transform "selected" for updating existing IRHD
selected <- selected %>% pivot_wider(id_cols = c('property_id'), names_from = 'variable_class', values_from = 'select') %>%
  setDT()

class(selected$project_id) = "numeric"
class(selected$total_units) = "numeric"
class(selected$total_restricted_units) = "numeric"
class(selected$zip) = "numeric"
class(selected$ami_20) = "numeric"
class(selected$ami_30) = "numeric"
class(selected$ami_35) = "numeric"
class(selected$ami_40) = "numeric"
class(selected$ami_45) = "numeric"
class(selected$ami_50) = "numeric"
class(selected$ami_60) = "numeric"
class(selected$ami_65) = "numeric"
class(selected$ami_80) = "numeric"
class(selected$bedroom_0) = "numeric"
class(selected$bedroom_1) = "numeric"
class(selected$bedroom_2) = "numeric"
class(selected$bedroom_3) = "numeric"
class(selected$bedroom_4) = "numeric"
class(selected$bedroom_unknown) = "numeric"
class(selected$bed_count) = "numeric"
class(selected$senior) = "numeric"
class(selected$homeless) = "numeric"
class(selected$disabled) = "numeric"

# Create new clean IRHD file
IRHD_clean <- copy(IRHD)

# Update records as determined by the "selected" dataframe
shared_fields <- intersect(names(selected), names(IRHD_clean))                                     # fields in common
dupes <- IRHD_clean[duplicated(property_id), cbind(.SD[1], number=.N), by=property_id] %>%           # duplicates (to exclude)
  pull(working_id)
blankfill <- IRHD_clean %>%                                                                        # create IRHD data that matches fields from selected
  .[!is.na(property_id) & working_id %not_in% (dupes), (colnames(.) %in% shared_fields), with=FALSE]  # include only common records, no duplicate keys
selected %<>% rows_patch(blankfill, by="property_id", unmatched="ignore")                           # replace NA in `selected` with values from `IRHD_clean`
IRHD_clean %<>% .[selected, (shared_fields):=mget(paste0("i.", shared_fields)), on=.(property_id)]  # carry over all matching variables from selected
rm(dupes, blankfill, shared_fields, long_IRHD, long_WSHFC, wshfc_colClasses, WSHFC_cols, irhd_colClasses, long_compare) # Clean up

# Add in new properties identified in newWSHFC
IRHD_clean$property_id <- as.integer(IRHD_clean$property_id)
IRHD_clean <- bind_rows(IRHD_clean, newWSHFC)

## 7) Join IRHD_clean table with cleaned data from King County -------------------------
IRHD_clean <- rbind(IRHD_clean, KC,fill=TRUE)

IRHD_clean %<>%
  relocate(ami_120, .after = ami_100)

# Create new working_id value for each new record
IRHD_clean$tempID <- str_sub(IRHD_clean$working_id, start= -4)
first <- as.numeric(max(na.omit(IRHD_clean$tempID)))+1
last <- first + sum(is.na(IRHD_clean$tempID))-1
IRHD_clean$working_id[IRHD_clean$working_id == "" | is.na(IRHD_clean$working_id)] <- paste0('SH_', first:last)
IRHD_clean <- subset(IRHD_clean, select = -c(tempID))

anyDuplicated(IRHD_clean, by="working_id") #check for any duplicates - hopefully 0!

## 8) Update AMI_Unknown and Bedroom_Unknown field ----------------------
# This code cleans up the AMI_Unknown field, so it adequately represents how many units are truly "unknown" in their AMI limits
AMIcols<-as.character(quote(c(ami_20, ami_25, ami_30, ami_35, ami_40, ami_45, ami_50, ami_60, ami_65, ami_70, ami_75, ami_80, ami_85, ami_90,  ami_100, ami_120)))[-1]

IRHD_clean %<>%
  mutate(across(all_of(AMIcols), ~replace_na(.,0) )%>%
           mutate(ami_unknown = total_restricted_units - rowSums(across(AMIcols))))
IRHD_clean %<>% mutate(ami_unknown = if_else(ami_unknown < 0, 0, ami_unknown))

sum(IRHD_clean$ami_20, IRHD_clean$ami_25, IRHD_clean$ami_30, IRHD_clean$ami_35, IRHD_clean$ami_40, IRHD_clean$ami_45, IRHD_clean$ami_50, IRHD_clean$ami_60, IRHD_clean$ami_65, IRHD_clean$ami_70, IRHD_clean$ami_75, IRHD_clean$ami_80, IRHD_clean$ami_85, IRHD_clean$ami_90,  IRHD_clean$ami_100, IRHD_clean$ami_120, na.rm = T)

# This code cleans up the Bedroom_Unknown field, so it adequately represents how many units are truly "unknown" in their unit bedroom count
sizecols<-as.character(quote(c(bedroom_0,bedroom_1,bedroom_2,bedroom_3,bedroom_4,bedroom_5)))[-1]

IRHD_clean %<>%
  mutate(across(all_of(sizecols), ~replace_na(.,0) )%>%
           mutate(bedroom_unknown = total_units - rowSums(across(sizecols))))
IRHD_clean %<>% mutate(bedroom_unknown = if_else(bedroom_unknown < 0, 0, bedroom_unknown))

sum(IRHD_clean$bedroom_0,IRHD_clean$bedroom_1,IRHD_clean$bedroom_2,IRHD_clean$bedroom_3,IRHD_clean$bedroom_4,IRHD_clean$bedroom_5, na.rm = T)

## 9) Summary table by County and AMI/Unit Size -------------------------
IRHD_county_bedrooms <- summary_county_bedrooms(IRHD_clean)
IRHD_county_ami <- summary_county_ami(IRHD_clean)

## 10) Explore new units -------------------------
new_IRHD <- IRHD_clean %>%
  filter(IRHD_clean$in_service_date == vintage_year)

new_IRHD_county_bedrooms <- summary_county_bedrooms(new_IRHD)
new_IRHD_county_ami <- summary_county_ami(new_IRHD)
new_IRHD_county <- summary_county(new_IRHD)

## 11) Export to Elmer IRHD_clean -------------------------
# table_id <- Id(schema = "stg", table = "irhd")
# dbWriteTable(conn = elmer_connection, name = table_id, value = IRHD_clean, overwrite = TRUE)
# dbExecute(conn=elmer_connection, statement=sql_export)
# dbDisconnect(elmer_connection)