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