## IRHD FUNCTIONS

# Summary functions for the IRHD. Summarize by county, by county & bedroom count, and by county & AMI limit
summary_county <- function(df){
  new_IRHD_county <- df %>%
    group_by(county) %>%
    summarize("unit count" = sum(na.omit(total_restricted_units)))
  
  # add total column
  new_IRHD_county <- new_IRHD_county %>%
    bind_rows(summarise(., across(where(is.numeric), sum),
                        across(where(is.character), ~'Total')))
  
  #transpose
  new_IRHD_county <- transpose(new_IRHD_county, keep.names = 'county')
  
  #fix column names
  colnames(new_IRHD_county) <- new_IRHD_county[1,]
  new_IRHD_county <- new_IRHD_county[-1, ] 
  new_IRHD_county %<>% rename(!!paste(vintage_year, "new units") := "county")
}

# BY UNIT SIZE
summary_county_bedrooms <- function(df){
  IRHD_county_bedrooms <- df %>%
    group_by(county) %>%
    summarize(`studio and one bedrooms` = sum(na.omit(bedroom_0 + bedroom_1)),`two and three bedrooms` = sum(na.omit(bedroom_2 + bedroom_3)),`four bedrooms and more` = sum(na.omit(bedroom_4 + bedroom_5)),`Unknown Size` = sum(na.omit(bedroom_unknown)))
  
  # add total column
  IRHD_county_bedrooms <- IRHD_county_bedrooms %>%
    bind_rows(summarise(., across(where(is.numeric), sum),
                        across(where(is.character), ~'Total')))
  # add total row
  IRHD_county_bedrooms %<>% mutate(total=rowSums(select_if(., is.numeric)))
  
  #transpose
  IRHD_county_bedrooms <- transpose(IRHD_county_bedrooms, keep.names = 'county')
  
  #fix column names
  colnames(IRHD_county_bedrooms) <- IRHD_county_bedrooms[1,]
  IRHD_county_bedrooms <- IRHD_county_bedrooms[-1, ] 
  IRHD_county_bedrooms %<>% rename("unit_size" = "county")
}

# BY AMI LIMIT
summary_county_ami <- function(df){
  IRHD_county_ami <- df %>%
    group_by(county) %>%
    summarize(`less than 30` = sum(na.omit(ami_20 + ami_25 + ami_30)),`31 to 50` = sum(na.omit(ami_35 + ami_40 + ami_45 +ami_50)),`51 to 80` = sum(na.omit(ami_60 + ami_65 + ami_70 + ami_75 + ami_80)),`81 to 100` = sum(na.omit(ami_85 + ami_90 + ami_100)),`100 plus` = sum(na.omit(ami_120)),`unknown AMI` = sum(na.omit(ami_unknown)))
  
  # add total column
  IRHD_county_ami <- IRHD_county_ami %>%
    bind_rows(summarise(., across(where(is.numeric), sum),
                        across(where(is.character), ~'Total')))
  # add total row
  IRHD_county_ami %<>% mutate(total=rowSums(select_if(., is.numeric)))
  
  #transpose
  IRHD_county_ami <- transpose(IRHD_county_ami, keep.names = 'county')
  
  #fix column names
  colnames(IRHD_county_ami) <- IRHD_county_ami[1,]
  IRHD_county_ami <- IRHD_county_ami[-1, ] 
  IRHD_county_ami %<>% rename("ami_limits" = "county")
}

# CREATE WORKINGID FOR NEW RECORDS
create_workingid <- function(df) {
  IRHD_clean <- df
  IRHD_clean$tempID <- str_sub(IRHD_clean$working_id, start= -4)
  first <- as.numeric(max(na.omit(IRHD_clean$tempID)))+1
  last <- first + sum(is.na(IRHD_clean$tempID))-1
  IRHD_clean$working_id[IRHD_clean$working_id == "" | is.na(IRHD_clean$working_id)] <- paste0('SH_', first:last)
  IRHD_clean <- subset(IRHD_clean, select = -c(tempID))
}

# CREATE AMI CLEANUP FUNCTION
# This code cleans up the AMI_Unknown field, so it adequately represents how many units are truly "unknown" in their AMI limits
ami_cleanup <- function(df) {
  
  IRHD_clean <- df
  AMIcols<-as.character(quote(c(ami_20, ami_25, ami_30, ami_35, ami_40, ami_45, ami_50, ami_60, ami_65, ami_70, ami_75, ami_80, ami_85, ami_90, ami_100, ami_120)))[-1]
  
  IRHD_clean %<>%
    mutate(across(all_of(AMIcols), ~replace_na(.,0) )%>%
             mutate(ami_unknown = total_restricted_units - rowSums(across(all_of(AMIcols)))))
  IRHD_clean %<>% mutate(ami_unknown = if_else(ami_unknown < 0, 0, ami_unknown))
}

# CREATE UNIT SIZE CLEANUP FUNCTION
# This code cleans up the Bedroom_Unknown field, so it adequately represents how many units are truly "unknown" in their unit bedroom count
unitsize_cleanup <- function(df) {
  
  IRHD_clean <- df
  sizecols<-as.character(quote(c(bedroom_0,bedroom_1,bedroom_2,bedroom_3,bedroom_4,bedroom_5)))[-1]
  
  IRHD_clean %<>%
    mutate(across(all_of(sizecols), ~replace_na(.,0) )%>%
             mutate(bedroom_unknown = total_units - rowSums(across(all_of(sizecols)))))
  IRHD_clean %<>% mutate(bedroom_unknown = if_else(bedroom_unknown < 0, 0, bedroom_unknown))
}

# CREATE DATA YEAR FIELD CLEANUP FUNCTION
datayear_cleanup <- function(df) {
  IRHD_clean <- subset(IRHD_clean, select = -c(data_year))
  IRHD_clean$data_year = vintage_year
  IRHD_clean %<>% select(data_year, everything())
}

# CREATE FUNCTION: IDENTIFY CHANGES ACROSS DATAFRAMES (df1 = IRHD. df2 = df to be matched. key = matching field)
identify_changes_irhd <- function(df1, df2, key) {
  # Pivot the IRHD data to make it long and thin
  long_IRHD <- df1 %>%
    pivot_longer(c('project_id',
                   'project_name',
                   'property_name',
                   'property_owner',
                   'manager',
                   'in_service_date',
                   'expiration_date',
                   'cleaned_address',
                   'full_address',
                   'county',
                   'total_units',
                   'total_restricted_units',
                   'ami_20','ami_25','ami_30','ami_35','ami_40','ami_45','ami_50','ami_60','ami_65','ami_70','ami_75','ami_80','ami_85','ami_90','ami_100', 'ami_120',
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
                   'sro',
                   'large_household',
                   'transitional',
                   'veterans',
                   'funding_sources',
                   'tenure'),
                 names_to='variable_class',
                 values_to='variable_value',
                 values_transform = list(variable_value=as.character))
  
  # Remove some fields that we don't need here
  long_IRHD %<>% select(c({{key}},variable_class,variable_value))
  long_IRHD %<>% drop_na({{key}})

  # Pivot the mocked-up data to make it long and thin
  long_df <- df2 %>%
    pivot_longer(c('project_id',
                   'project_name',
                   'property_name',
                   'property_owner',
                   'manager',
                   'in_service_date',
                   'expiration_date',
                   'cleaned_address',
                   'full_address',
                   'county',
                   'total_units',
                   'total_restricted_units',
                   'ami_20','ami_25','ami_30','ami_35','ami_40','ami_45','ami_50','ami_60','ami_65','ami_70','ami_75','ami_80','ami_85','ami_90','ami_100', 'ami_120',
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
                   'sro',
                   'large_household',
                   'transitional',
                   'veterans',
                   'funding_sources',
                   'tenure'),
                 names_to='variable_class',
                 values_to='variable_value',
                 values_transform = list(variable_value=as.character))

  # Remove some fields that we don't need here
  long_df %<>% select(c({{key}},variable_class,variable_value))


  # Compare the two data sets in long form to identify values that have been changed
  rectify <- long_IRHD %>%
    inner_join(long_df, by=c({{key}}, 'variable_class')) %>%
    mutate("match" = ifelse(mapply(identical, variable_value.x, variable_value.y), "y", "n")) %>%
    filter(match == "n")
  
  # Create field to indicate which variable to use
  rectify$select <- ""
  rectify <- tibble::rowid_to_column(rectify, "ID")
}

# CREATE FUNCTION: UPDATE IRHD WITH SELECTED CHANGES (df1 = "IRHD" df, df2 = "updates" df. key = matching field)
update_irhd <- function(df1, df2, key) {
  # Check inputs
  if (!key %in% colnames(df1) | !key %in% colnames(df2)) {
    stop(paste("Key column", key, "must exist in both input tables"))
  }
  
  # Create new clean IRHD file & wider updates file
  IRHD_clean <- setDT(df1)
  updates <- setDT(df2) %>% .[!is.na(select)] %>%
    dcast(formula = paste0(key, " ~ variable_class"), value.var = "select")
  
  # Specify column classes
  col_classes <- list(
    character = c("project_id", "in_service_date", "zip"),
    numeric = c("total_units", "total_restricted_units", 
                "ami_20", "ami_25", "ami_30", "ami_35", "ami_40", "ami_45", "ami_50", 
                "ami_60", "ami_65", "ami_70", "ami_75", "ami_80", "ami_85", "ami_90", 
                "ami_100", "ami_120", "market_rate", "manager_unit", 
                "bedroom_0", "bedroom_1", "bedroom_2", "bedroom_3", "bedroom_4", 
                "bedroom_5", "bedroom_unknown", "bed_count", "senior", "HOMEcity", 
                "HOMEcounty", "HOMEstate", "homeless", "sro", "transitional", 
                "large_household", "veterans", "disabled")
  )
  
  # Apply column classes
  char_cols <- intersect(col_classes$character, names(updates))
  num_cols <- intersect(col_classes$numeric, names(updates))
  updates[, (char_cols) := lapply(.SD, as.character), .SDcols = char_cols]
  updates[, (num_cols) := lapply(.SD, as.numeric), .SDcols = num_cols]
  
  # Identify shared fields between updates and IRHD_clean
  shared_fields <- intersect(names(updates), names(IRHD_clean))
  
  # Exclude duplicates from updates
  duplicate_keys <- updates[, .N, by = key][N > 1, get(key)]
  if (length(duplicate_keys) > 0) {
    updates <- updates[!get(key) %in% duplicate_keys]
  }
  
  # Update IRHD_clean with non-NA values from updates
  for (field in shared_fields) {
    IRHD_clean[updates[!is.na(get(field))], 
               (field) := get(paste0("i.", field)), 
               on = eval(key)]
  }
  
  return(IRHD_clean)
}
