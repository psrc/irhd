# TITLE: Reconcile IRHD and new data
# GEOGRAPHIES: King, Snohomish, Pierce, Kitsap
# DATA SOURCE: WSHFC, HASCO, THA, King County, EHA, PCHA, BHA
# DATE MODIFIED: 10.20.2023
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

IRHD_path <- "J:/Projects/IncomeRestrictedHsgDB/2021 vintage/Data/Working Files/2021 IRHD v3 - ready4reconcilescript.csv"
WSHFC_path <- "J:/Projects/IncomeRestrictedHsgDB/2021 vintage/WSHFC/Cleaned Data/WSHFC_2021_cleaned.csv"
export_4review_path <- "C:/Users/eclute/GitHub/irhd/Export4review.csv"
HASCO_updates_path <- "J:/Projects/IncomeRestrictedHsgDB/2021 vintage/Review Files - Received/PSRC_2021_IRHD_Snohomish_minor updates.csv"
THA_updates_path <- "J:/Projects/IncomeRestrictedHsgDB/2021 vintage/Review Files - Received/PSRC_2021_IRHD_Pierce_THA_minor updates.csv"
KC_path <- "J:/Projects/IncomeRestrictedHsgDB/2021 vintage/Review Files - Received/King County Income-restricted Housing Database 2021.csv"
script_path <- "C:/Users/eclute/GitHub/irhd/address_match.R"
source(script_path)

`%not_in%` <- Negate(`%in%`)
vintage_year <- "2021"
sql <- paste('exec irhd.merge_irhd_properties', vintage_year)

elmer_connection <- dbConnect(odbc::odbc(),
                              driver = "SQL Server",
                              server = "SQLserver",
                              database = "Elmer",
                              trusted_connection = "yes")

table_id <- Id(schema = "stg", table = "irhd")
sql_bing_maps_key <- Sys.getenv("BING_MAPS_KEY")
sql_import <- paste('irhd.properties')
sql_export <- paste0('exec irhd.merge_irhd_properties ', vintage_year, ",'", sql_bing_maps_key, "'")

# functions ---
# BY COUNTY
summary_county <- function(df){
  new_IRHD_county <- df %>%
    group_by(County) %>%
    summarize("unit count" = sum(na.omit(TotalRestrictedUnits)))
  
  # add total column
  new_IRHD_county <- new_IRHD_county %>%
    bind_rows(summarise(., across(where(is.numeric), sum),
                        across(where(is.character), ~'Total')))
  
  #transpose
  new_IRHD_county <- transpose(new_IRHD_county, keep.names = 'County')
  
  #fix column names
  colnames(new_IRHD_county) <- new_IRHD_county[1,]
  new_IRHD_county <- new_IRHD_county[-1, ] 
  new_IRHD_county %<>% rename(!!paste(vintage_year, "new units") := "County")
  
}

# BY UNIT SIZE
summary_county_bedrooms <- function(df){
  IRHD_county_bedrooms <- df %>%
    group_by(County) %>%
    summarize(`studio and one bedrooms` = sum(na.omit(Bedroom_0 + Bedroom_1)),`two and three bedrooms` = sum(na.omit(Bedroom_2 + Bedroom_3)),`four bedrooms and more` = sum(na.omit(Bedroom_4 + Bedroom_5)),`Unknown Size` = sum(na.omit(Bedroom_Unknown)))
  
  # add total column
  IRHD_county_bedrooms <- IRHD_county_bedrooms %>%
    bind_rows(summarise(., across(where(is.numeric), sum),
                        across(where(is.character), ~'Total')))
  # add total row
  IRHD_county_bedrooms %<>% mutate(total=rowSums(select_if(., is.numeric)))
  
  #transpose
  IRHD_county_bedrooms <- transpose(IRHD_county_bedrooms, keep.names = 'County')
  
  #fix column names
  colnames(IRHD_county_bedrooms) <- IRHD_county_bedrooms[1,]
  IRHD_county_bedrooms <- IRHD_county_bedrooms[-1, ] 
  IRHD_county_bedrooms %<>% rename("unit_size" = "County")

}

# BY AMI LIMIT
summary_county_ami <- function(df){
  IRHD_county_ami <- df %>%
    group_by(County) %>%
    summarize(`less than 30` = sum(na.omit(AMI20 + AMI25 + AMI30)),`31 to 50` = sum(na.omit(AMI35 + AMI40 + AMI45 +AMI50)),`51 to 80` = sum(na.omit(AMI60 + AMI65 + AMI70 + AMI75 + AMI80)),`81 to 100` = sum(na.omit(AMI85 + AMI90 + AMI100)),`100 plus` = sum(na.omit(AMI120)),`unknown AMI` = sum(na.omit(AMI_Unknown)))

  # add total column
  IRHD_county_ami <- IRHD_county_ami %>%
    bind_rows(summarise(., across(where(is.numeric), sum),
                        across(where(is.character), ~'Total')))
  # add total row
  IRHD_county_ami %<>% mutate(total=rowSums(select_if(., is.numeric)))
  
  #transpose
  IRHD_county_ami <- transpose(IRHD_county_ami, keep.names = 'County')
  
  #fix column names
  colnames(IRHD_county_ami) <- IRHD_county_ami[1,]
  IRHD_county_ami <- IRHD_county_ami[-1, ] 
  IRHD_county_ami %<>% rename("ami_limits" = "County")
}

## 1) load data -------------------------

# load cleaned 2021 IRHD that has portfolios as of end of 2021
IRHD_raw <- fread(IRHD_path)

# borrow datatype characterization from IRHD to apply to identical columns in WSHFC data
irhd_colClasses = sapply(IRHD_raw, class)
names(irhd_colClasses) <- colnames(IRHD_raw)
WSHFC_cols = colnames(read.csv(WSHFC_path, nrows=1))
wshfc_colClasses <- irhd_colClasses %>% .[names(.) %in% WSHFC_cols]

# load cleaned WSHFC data that has portfolios as of end of 2021; apply datatypes to match
WSHFC_raw <- fread(WSHFC_path, colClasses=wshfc_colClasses)

# load cleaned KC data that has portfolios as of end of 2021
KC_raw <- fread(KC_path)

# load cleaned HASCO & THA data - only keep fields where we have new data (in the "Corrected" column)
HASCO_raw <- fread(HASCO_updates_path)
HASCO <- HASCO_raw %>%
  drop_na(Corrected)

THA_raw <- fread(THA_updates_path)
THA <- THA_raw %>%
  drop_na(Corrected)

## 2) clean up data -------------------------

# IRHD ---
IRHD <- IRHD_raw %>% .[County %in% c("Pierce", "Snohomish", "Kitsap")]                         # King county handled separately
IRHD %<>% .[, grep("\\d+-\\d+%", colnames(.)):=NULL]                                           # Remove summary AMI fields

# Create three new HOME fields
IRHD %<>% mutate(HOMEcity = NA_character_,                                                     # Add fields to match WSHFC
                     HOMEcounty = NA_character_,
                     HOMEstate = NA_character_,
                     .after = HOME)

# Manage duplicate records in IRHD
IRHD %<>%  filter(!(UniqueID == "SH_7002")) %>% # Remove this record, keep SH_6053
               filter(!(UniqueID == "SH_6516")) # Remove this record, keep SH_6517

# Remove Jurisdiction and cityFIPS fields, we will calculate these in Elmer going forward
IRHD %<>% select(-c(Jurisdiction,CityFIPS))

IRHD$full_address <- str_c(IRHD$Address,', ',IRHD$City,', WA, ',IRHD$ZIP)
IRHD <- add_cleaned_addresses(IRHD) %>% setDT()

str(IRHD)

# King County finalized 2021 data ---
KC <- KC_raw
KC$County <- "King"
KC %<>% filter(KC$InServiceDate <= vintage_year | is.na(KC$InServiceDate))

# Remove fields we don't need
##(Policy field is blank, data currently stored in "FundingSource" - This may change!! Watch next year)
KC %<>% select(-c(unique_linking_ID,HITS_survey,GeoCode_Street,GeoCode_City,ProjectType,Policy))

# Rename fields to match IRHD
KC <- KC %>% 
  rename("DataSource" = "DataSourceName",
         "BedCount" = "GroupHomeOrBed",
         "ZIP" = "GeoCode_Zip",
         "full_address" = "Address_standardized",
         "ExpirationDate" = "ExpirationYear",
         "Owner" = "ProjectSponsor",
         "Manager" = "ContactName",
         "Site_Type" = "PopulationServed",
         "FundingSources" = "Funder",
         "HOME" = "HOMEUnits",
         "Policy" = "FundingSource")

KC$cleaned_address <- str_c(KC$full_address,', ',KC$City,', WA, ',KC$ZIP)

# Identify and remove duplicated UniqueID value
dups <- filter(KC, UniqueID == "SH_5215")
KC[1222,1]<-"SH_7234"
rm(dups)

## 3) clean up some variables in WSHFC before joining -------------------------

IRHD$Manager[IRHD$Manager == 'HASCO'] <- 'Snohomish County Housing Authority'
IRHD$Owner[IRHD$Owner == 'HASCO'] <- 'Snohomish County Housing Authority'

IRHD$Manager[IRHD$Manager == 'Low Income Housing Institute'] <- 'Low Income Housing Institute (LIHI)'
IRHD$Owner[IRHD$Owner == 'Low Income Housing Institute'] <- 'Low Income Housing Institute (LIHI)'

WSHFC_raw$Address[WSHFC_raw$Address == '1724 E. 44th'] <- '1724 E 44th Street'
WSHFC_raw$Address[WSHFC_raw$Address == '9225 Bayshore Drive NW'] <- '9225 Bay Shore Dr NW'
WSHFC_raw$Address[WSHFC_raw$Address == '9239 Bayshore Dr NW'] <- '9239 Bay Shore Dr NW'

# Clean Address field for matching
remotes::install_github("slu-openGIS/postmastr")
library(stringr)

WSHFC_raw$full_address <- str_c(WSHFC_raw$Address,', ',WSHFC_raw$City,', WA, ',WSHFC_raw$ZIP)
WSHFC_raw <- add_cleaned_addresses(WSHFC_raw)

str(WSHFC_raw)

## 4) Locate records in WSHFC_raw not in IRHD (likely new records/properties) -------------------------

newWSHFC <- anti_join(WSHFC_raw, IRHD, by = "PropertyID")
newWSHFC <- newWSHFC[ , !names(newWSHFC) %in% c("Farmworker")]

## 5) Locate records in IRHD not in WSHFC (No longer in WSHFC data, but once were?) -------------------------

nomatchIRHD <- anti_join(IRHD, WSHFC_raw, by = "PropertyID")
nomatchIRHD <- nomatchIRHD %>% drop_na(PropertyID)

# 7/5/23 after confirmation from Commerce/WSHFC, these missing properties were accidentally excluded from the 2021 WSHFC dataset
# KEEP all these records in IRHD. 2022 WSHFC dataset should include these. Next time, properties in 'nomatchIRHD' will need to be verified (did they go offline, etc?)

## 6) Identify matched records in IRHD and WSHFC -------------------------

# Pivot the IRHD data to make it long and thin
long_IRHD <- IRHD %>%
  pivot_longer(c('ProjectID',
                 'ProjectName',
                 'PropertyName',
                 'Owner',
                 'Manager',
                 'InServiceDate',
                 'ExpirationDate',
                 'cleaned_address',
                 'County',
                 'TotalUnits',
                 'TotalRestrictedUnits',
                 'AMI20','AMI25','AMI30','AMI35','AMI40','AMI45','AMI50','AMI60','AMI65','AMI70','AMI75','AMI80','AMI85','AMI90','AMI100',
                 'MarketRate',
                 'ManagerUnit',
                 'Bedroom_0','Bedroom_1','Bedroom_2','Bedroom_3','Bedroom_4','Bedroom_5','Bedroom_Unknown',
                 'BedCount',
                 'Site_Type',
                 'HOMEcity',
                 'HOMEcounty',
                 'HOMEstate',
                 'Confidentiality',
                 'Policy',
                 'Senior',
                 'Disabled',
                 'Homeless',
                 'Transitional',
                 'Veterans',
                 'FundingSources',
                 'Tenure'),
               names_to='variable_class',
               values_to='variable_value',
               values_transform = list(variable_value=as.character))

# Remove some fields that we don't need here
long_IRHD %<>% select(c(PropertyID,variable_class,variable_value))

# Pivot the mocked-up data to make it long and thin
long_WSHFC <- WSHFC_raw %>%
  pivot_longer(c('ProjectID',
                 'ProjectName',
                 'PropertyName',
                 'Owner',
                 'Manager',
                 'InServiceDate',
                 'ExpirationDate',
                 'cleaned_address',
                 'County',
                 'TotalUnits',
                 'TotalRestrictedUnits',
                 'AMI20','AMI25','AMI30','AMI35','AMI40','AMI45','AMI50','AMI60','AMI65','AMI70','AMI75','AMI80','AMI85','AMI90','AMI100',
                 'MarketRate',
                 'ManagerUnit',
                 'Bedroom_0','Bedroom_1','Bedroom_2','Bedroom_3','Bedroom_4','Bedroom_5','Bedroom_Unknown',
                 'BedCount',
                 'Site_Type',
                 'HOMEcity',
                 'HOMEcounty',
                 'HOMEstate',
                 'Confidentiality',
                 'Policy',
                 'Senior',
                 'Disabled',
                 'Homeless',
                 'Transitional',
                 'Veterans',
                 'FundingSources',
                 'Tenure'),
               names_to='variable_class',
               values_to='variable_value',
               values_transform = list(variable_value=as.character))

# Remove some fields that we don't need here
long_WSHFC %<>% select(c(PropertyID,variable_class,variable_value))


# Compare the two data sets in long form to identify values that have been changed
long_compare <- long_IRHD %>%
  inner_join(long_WSHFC, by=c('PropertyID', 'variable_class')) %>%
  mutate("match" = ifelse(mapply(identical, variable_value.x, variable_value.y), "YES", "NO")) %>%
  filter(match == "NO") %>%
  drop_na(variable_value.y)

## 7) Identify which rows will be updated with new WSHFC data, or keep existing data -------------------------

# Create field to indicate which variable to use
long_compare$select <- ""
long_compare <- tibble::rowid_to_column(long_compare, "ID")

# Subset 1) select records with no data in the IRHD - we will take new data from WSHFC
subset1 <- long_compare %>% subset((is.na(variable_value.x)| variable_value.x == ""), select = c(ID, PropertyID, variable_class,variable_value.x,variable_value.y,match, select))
subset1$select <- subset1$variable_value.y
long_compare <- anti_join(long_compare, subset1, by=c("ID"="ID")) # remove from long_compare
selected <- subset1
rm(subset1)

# Subset 2) Below fields - select WHSFC data
subset2 <- long_compare %>% subset((variable_class == "InServiceDate" |
                                    variable_class == "Manager"|
                                    variable_class == "Owner"|
                                    variable_class == "ProjectID"|
                                    variable_class == "Disabled"|
                                    variable_class == "Homeless"|
                                    variable_class == "Senior"|
                                    variable_class == "BedCount"|
                                    variable_class == "PropertyName"|
                                    variable_class == "Site_Type"|
                                    variable_class == "FundingSources"|
                                    variable_class == "HOMEcity"|
                                    variable_class == "HOMEcounty"|
                                    variable_class == "HOMEstate"|
                                    variable_class == "ProjectName"), select = c(ID, PropertyID, variable_class,variable_value.x,variable_value.y,match, select))
subset2$select <- subset2$variable_value.y
long_compare <- anti_join(long_compare, subset2, by=c("ID"="ID")) # remove from long_compare
selected <- rbind(selected, subset2)
rm(subset2)

# Subset 3) select addresses that have "multiple" in the field - use IRHD address
subset3 <- long_compare %>% subset(str_detect(long_compare$variable_value.y, str_c("Mu")), select = c(ID, PropertyID, variable_class,variable_value.x,variable_value.y,match, select))
subset3$select <- subset3$variable_value.x
long_compare <- anti_join(long_compare, subset3, by=c("ID"="ID"))# remove from long_compare
selected <- rbind(selected, subset3)
rm(subset3)

# Subset 4) select all AMI/Unit count/Bedroom size data, identify small numeric changes
subset4 <- long_compare %>% subset((variable_class == "TotalUnits" |
                                    variable_class == "TotalRestrictedUnits"|
                                    variable_class == "AMI20"|
                                    variable_class == "AMI25"|
                                    variable_class == "AMI30"|
                                    variable_class == "AMI35"|
                                    variable_class == "AMI40"|
                                    variable_class == "AMI45"|
                                    variable_class == "AMI50"|
                                    variable_class == "AMI60"|
                                    variable_class == "AMI65"|
                                    variable_class == "AMI70"|
                                    variable_class == "AMI75"|
                                    variable_class == "AMI80"|
                                    variable_class == "AMI85"|
                                    variable_class == "AMI90"|
                                    variable_class == "AMI100"|
                                    variable_class == "MarketRate"|
                                    variable_class == "ManagerUnit"|
                                    variable_class == "Bedroom_0"|
                                    variable_class == "Bedroom_1"|
                                    variable_class == "Bedroom_2"|
                                    variable_class == "Bedroom_3"|
                                    variable_class == "Bedroom_4"|
                                    variable_class == "Bedroom_5"|
                                    variable_class == "Bedroom_Unknown"|
                                    variable_class == "BedCount"), select = c(ID, PropertyID, variable_class,variable_value.x,variable_value.y,match, select))

# Create formula for calculating difference between numeric values
subset4_sum <- subset4 %>% group_by(PropertyID) %>%
  summarize(sum.x=sum(as.numeric(variable_value.x)),
            sum.y=sum(as.numeric(variable_value.y)))

# abs function - absolute value of the percentage difference
subset4_sum$diff <- abs((subset4_sum$sum.x-subset4_sum$sum.y)/subset4_sum$sum.x)

# join back to subset4 table, so each row of data now has the percentage difference
subset4 <- merge(subset4, subset4_sum, by = "PropertyID")
rm(subset4_sum)

# Rows with "diff" of 12% or less will be selected - we want the WSHFC data
subset4$select <- ifelse(subset4$diff <= "0.12", subset4$variable_value.y, "")

# Rows where the sum.y is 0, we keep the sum.x data (if WSHFC data is 0, we keep IRHD data)
subset4$select <- ifelse(subset4$sum.y == "0", subset4$variable_value.x, subset4$select)

# Remove "diff" of greater than 12% from subset4
subset4 <- subset4 %>% subset(!(select == ""), select = c(ID, PropertyID, variable_class,variable_value.x,variable_value.y,match, select, sum.x, sum.y, diff))
long_compare <- anti_join(long_compare, subset4, by=c("ID"="ID")) # remove from long_compare

subset4 <- subset4[, -c(8,9,10)]
selected <- rbind(selected, subset4)
rm(subset4)

# Subset 5) If WSHFC field is blank, select IRHD data
subset5 <- long_compare %>% subset((is.na(variable_value.y)| variable_value.y == ""), select = c(ID, PropertyID, variable_class,variable_value.x,variable_value.y,match, select))
subset5$select <- subset5$variable_value.x
long_compare <- anti_join(long_compare, subset5, by=c("ID"="ID")) # remove from long_compare
selected <- rbind(selected, subset5)
rm(subset5)


# Subset 6-10) Various manual selections
subset6 <- long_compare %>% subset(str_detect(long_compare$variable_value.y, str_c("303 Howell Way & 417 3rd Ave, Edmonds, WA 98020")), select = c(ID, PropertyID, variable_class,variable_value.x,variable_value.y,match, select))
subset6$select <- subset6$variable_value.x
long_compare <- anti_join(long_compare, subset6, by=c("ID"="ID"))# remove from long_compare
selected <- rbind(selected, subset6)
rm(subset6)

subset7 <- long_compare %>% subset(str_detect(long_compare$variable_value.y, " Rainier Ave, Everett, WA 98201"), select = c(ID, PropertyID, variable_class,variable_value.x,variable_value.y,match, select))
subset7$select <- subset7$variable_value.x
long_compare <- anti_join(long_compare, subset7, by=c("ID"="ID"))# remove from long_compare
selected <- rbind(selected, subset7)
rm(subset7)

subset8 <- long_compare %>% subset(str_starts(long_compare$variable_value.y, ("[:alpha:]")), select = c(ID, PropertyID, variable_class,variable_value.x,variable_value.y,match, select))
subset8$select <- subset8$variable_value.x
long_compare <- anti_join(long_compare, subset8, by=c("ID"="ID"))# remove from long_compare
selected <- rbind(selected, subset8)
rm(subset8)

subset9 <- long_compare %>% subset(str_detect(long_compare$PropertyID, "18015|18016|16100|16101|16402|16002|18092|16002"), select = c(ID, PropertyID, variable_class,variable_value.x,variable_value.y,match, select))
subset9$select <- subset9$variable_value.x
long_compare <- anti_join(long_compare, subset9, by=c("ID"="ID"))# remove from long_compare
selected <- rbind(selected, subset9)
rm(subset9)

subset10 <- long_compare %>% subset(str_detect(long_compare$PropertyID, "18210|16044"), select = c(ID, PropertyID, variable_class,variable_value.x,variable_value.y,match, select))
subset10$select <- subset10$variable_value.y
long_compare <- anti_join(long_compare, subset10, by=c("ID"="ID"))# remove from long_compare
selected <- rbind(selected, subset10)
rm(subset10)

# Export remaining records and contact the corresponding housing authority
export_longcompare <- long_compare %>%
  inner_join(IRHD, by='PropertyID')

export_longcompare = export_longcompare[,c("ID","PropertyID","variable_class","variable_value.x","variable_value.y","DataSource","ProjectName","Owner","InServiceDate", "County","cleaned_address")]
write.csv(export_longcompare, export_4review_path, row.names=FALSE)

# Subset 11-14) As directed by housing authorities
#Everett Housing Authority
subset11 <- long_compare %>% subset(str_detect(long_compare$PropertyID, "15905|15932|15961|16024|16593|17818|17820|17821|18107|18108|18109|18110|17749|17748"), select = c(ID, PropertyID, variable_class,variable_value.x,variable_value.y,match, select))
subset11$select <- subset11$variable_value.x
long_compare <- anti_join(long_compare, subset11, by=c("ID"="ID"))# remove from long_compare
selected <- rbind(selected, subset11)
rm(subset11)

#Snohomish County Housing Authority
subset12 <- long_compare %>%
  inner_join(HASCO, join_by(PropertyID == PropertyID, variable_class == Variable))
subset12$select <- subset12$Corrected
subset12 <- subset12 %>% 
  rename("ID" = "ID.x")
subset12 %<>% select(c(ID,PropertyID,variable_class,variable_value.x,variable_value.y,match,select))
long_compare <- anti_join(long_compare, subset12, by=c("ID"="ID"))# remove from long_compare
selected <- rbind(selected, subset12)
rm(subset12)

#Tacoma Housing Authority
subset13 <- long_compare %>%
  inner_join(THA, join_by(PropertyID == PropertyID, variable_class == Variable))
subset13$select <- subset13$Corrected
subset13 <- subset13 %>% 
  rename("ID" = "ID.x")
subset13 %<>% select(c(ID,PropertyID,variable_class,variable_value.x,variable_value.y,match,select))
long_compare <- anti_join(long_compare, subset13, by=c("ID"="ID"))# remove from long_compare
selected <- rbind(selected, subset13)
rm(subset13)

#All remaining changes (select newer WSHFC data - assuming it is correct)
subset14 <- long_compare %>% subset((long_compare$select == ""), select = c(ID, PropertyID, variable_class,variable_value.x,variable_value.y,match, select))
subset14$select <- subset14$variable_value.y
long_compare <- anti_join(long_compare, subset14, by=c("ID"="ID"))# remove from long_compare
selected <- rbind(selected, subset14)
rm(subset14)


## 8) Take "selected" data and update IRHD records, create IRHD_clean table -------------------------
# Transform "selected" for updating existing IRHD
selected <- selected %>% pivot_wider(id_cols = c('PropertyID'), names_from = 'variable_class', values_from = 'select') %>%
  setDT()

class(selected$ProjectID) = "numeric"
class(selected$TotalUnits) = "numeric"
class(selected$TotalRestrictedUnits) = "numeric"
class(selected$ZIP) = "numeric"
class(selected$AMI20) = "numeric"
class(selected$AMI30) = "numeric"
class(selected$AMI35) = "numeric"
class(selected$AMI40) = "numeric"
class(selected$AMI45) = "numeric"
class(selected$AMI50) = "numeric"
class(selected$AMI60) = "numeric"
class(selected$AMI65) = "numeric"
class(selected$AMI80) = "numeric"
class(selected$Bedroom_0) = "numeric"
class(selected$Bedroom_1) = "numeric"
class(selected$Bedroom_2) = "numeric"
class(selected$Bedroom_3) = "numeric"
class(selected$Bedroom_4) = "numeric"
class(selected$Bedroom_Unknown) = "numeric"
class(selected$BedCount) = "numeric"
class(selected$Senior) = "numeric"
class(selected$Homeless) = "numeric"
class(selected$Disabled) = "numeric"

# Create new clean IRHD file
IRHD_clean <- copy(IRHD)

# Update records as determined by the "selected" dataframe
shared_fields <- intersect(names(selected), names(IRHD_clean))                                     # fields in common
dupes <- IRHD_clean[duplicated(PropertyID), cbind(.SD[1], number=.N), by=PropertyID] %>%           # duplicates (to exclude)
  pull(UniqueID)
blankfill <- IRHD_clean %>%                                                                        # create IRHD data that matches fields from selected
  .[!is.na(PropertyID) & UniqueID %not_in% (dupes), (colnames(.) %in% shared_fields), with=FALSE]  # include only common records, no duplicate keys
selected %<>% rows_patch(blankfill, by="PropertyID", unmatched="ignore")                           # replace NA in `selected` with values from `IRHD_clean`
IRHD_clean %<>% .[selected, (shared_fields):=mget(paste0("i.", shared_fields)), on=.(PropertyID)]  # carry over all matching variables from selected
rm(dupes, blankfill, shared_fields, long_IRHD, long_WSHFC, wshfc_colClasses, WSHFC_cols, irhd_colClasses, long_compare) # Clean up

# Add in new properties identified in newWSHFC
newWSHFC$HOMEcity <- as.character(newWSHFC$HOMEcity)
newWSHFC$HOMEcounty <- as.character(newWSHFC$HOMEcounty)
newWSHFC$HOMEstate <- as.character(newWSHFC$HOMEstate)

IRHD_clean <- bind_rows(IRHD_clean, newWSHFC)

## 9) Join IRHD_clean table with cleaned data from King County -------------------------
IRHD_clean <- rbind(IRHD_clean, KC,fill=TRUE)

IRHD_clean %<>%
  relocate(AMI120, .after = AMI100)

# Create new UniqueID value for each new record
IRHD_clean$tempID <- str_sub(IRHD_clean$UniqueID, start= -4)
first <- as.numeric(max(na.omit(IRHD_clean$tempID)))+1
last <- first + sum(is.na(IRHD_clean$tempID))-1
IRHD_clean$UniqueID[IRHD_clean$UniqueID == "" | is.na(IRHD_clean$UniqueID)] <- paste0('SH_', first:last)
IRHD_clean <- subset(IRHD_clean, select = -c(tempID))

anyDuplicated(IRHD_clean, by="UniqueID") #check for any duplicates - hopefully 0!

## 10) Update AMI_Unknown and Bedroom_Unknown field ----------------------
# This code cleans up the AMI_Unknown field, so it adequately represents how many units are truly "unknown" in their AMI limits
AMIcols<-as.character(quote(c(AMI20, AMI25, AMI30, AMI35, AMI40, AMI45, AMI50, AMI60, AMI65, AMI70, AMI75, AMI80, AMI85, AMI90,  AMI100, AMI120)))[-1]

IRHD_clean %<>%
  mutate(across(all_of(AMIcols), ~replace_na(.,0) )%>%
           mutate(AMI_Unknown = TotalRestrictedUnits - rowSums(across(AMIcols))))
IRHD_clean %<>% mutate(AMI_Unknown = if_else(AMI_Unknown < 0, 0, AMI_Unknown))

sum(IRHD_clean$AMI20, IRHD_clean$AMI25, IRHD_clean$AMI30, IRHD_clean$AMI35, IRHD_clean$AMI40, IRHD_clean$AMI45, IRHD_clean$AMI50, IRHD_clean$AMI60, IRHD_clean$AMI65, IRHD_clean$AMI70, IRHD_clean$AMI75, IRHD_clean$AMI80, IRHD_clean$AMI85, IRHD_clean$AMI90,  IRHD_clean$AMI100, IRHD_clean$AMI120, na.rm = T)

# This code cleans up the Bedroom_Unknown field, so it adequately represents how many units are truly "unknown" in their unit bedroom count
sizecols<-as.character(quote(c(Bedroom_0,Bedroom_1,Bedroom_2,Bedroom_3,Bedroom_4,Bedroom_5)))[-1]

IRHD_clean %<>%
  mutate(across(all_of(sizecols), ~replace_na(.,0) )%>%
           mutate(Bedroom_Unknown = TotalUnits - rowSums(across(sizecols))))
IRHD_clean %<>% mutate(Bedroom_Unknown = if_else(Bedroom_Unknown < 0, 0, Bedroom_Unknown))

sum(IRHD_clean$Bedroom_0,IRHD_clean$Bedroom_1,IRHD_clean$Bedroom_2,IRHD_clean$Bedroom_3,IRHD_clean$Bedroom_4,IRHD_clean$Bedroom_5, na.rm = T)

## 11) Summary table by County and AMI/Unit Size -------------------------
IRHD_county_bedrooms <- summary_county_bedrooms(IRHD_clean)
IRHD_county_ami <- summary_county_ami(IRHD_clean)

## 12) Explore new units -------------------------
new_IRHD <- IRHD_clean %>%
  filter(IRHD_clean$InServiceDate == vintage_year)

new_IRHD_county_bedrooms <- summary_county_bedrooms(new_IRHD)
new_IRHD_county_ami <- summary_county_ami(new_IRHD)
new_IRHD_county <- summary_county(new_IRHD)

## 13) Export to Elmer IRHD_clean -------------------------
export_version <- IRHD_clean
export_version <- export_version %>%
  rename(working_id = UniqueID,
         data_source = DataSource,
         general_notes = GeneralNotes,
         project_id = ProjectID,
         project_name = ProjectName,
         property_id = PropertyID,
         property_name = PropertyName,
         property_owner = Owner,
         in_service_date = InServiceDate,
         expiration_date = ExpirationDate,
         reported_address = Address, 
         total_units = TotalUnits,
         total_restricted_units = TotalRestrictedUnits,
         ami_20 = AMI20,
         ami_25 = AMI25,
         ami_30 = AMI30,
         ami_35 = AMI35,
         ami_40 = AMI40,
         ami_45 = AMI45,
         ami_50 = AMI50,
         ami_60 = AMI60,
         ami_65 = AMI65,
         ami_70 = AMI70,
         ami_75 = AMI75,
         ami_80 = AMI80,
         ami_85 = AMI85,
         ami_90 = AMI90,
         ami_100 = AMI100,
         ami_120 = AMI120,
         market_rate = MarketRate,
         manager_unit = ManagerUnit,
         units_preserved = UnitsPreserved,
         bed_count = BedCount,
         accessible_units = `Accessible Units`,
         large_household = LargeHousehold4plus,
         mixed_use = MixedUse,
         public_housing = `Public Housing`,
         funding_sources = FundingSources,
         section_8 = Section8,
         fed_income_subsidized = `Fed income subsidized`,
         units_with_rental_subsidy = `# Units with Rental Subsidy`,
         rental_subsidy_source = `Rental Subsidy Source`
  )

table_id <- Id(schema = "stg", table = "irhd")
dbWriteTable(conn = elmer_connection, name = table_id, value = export_version, overwrite = TRUE)
dbExecute(conn=elmer_connection, statement=sql_export)
dbDisconnect(elmer_connection)