library(postmastr)
library(tidyverse)


add_cleaned_addresses <- function(in.df) {
  # Returns a data frame with a new column "cleaned.address"
  # which is the formatted version of input column "fulladdress".
  #
  # Required parameter in.df must be a data frame containing a column "fulladdress"
  # that contains full address info (house-number street, city-name, WA, ZIP Code).
  tryCatch({
    cities <- pm_dictionary(type='city', filter="WA", case=c("title", "upper"), locale="us")
    in.df <- in.df %>%
      mutate(fulladdress = str_replace_all(fulladdress, "N.W.", "NW"))

    df_ident <- pm_identify(in.df, var='fulladdress', locale="us")
    df_min <- pm_prep(df_ident, var='fulladdress', type='street')

    my_dirs <- pm_append("directional",
             input=c("N.W.", "N.E.", "S.W.", "S.E.", "N.W", "N.E", "S.W", "S.E"),
             output=c("NW", "NE", "SW", "SE", "NW", "NE", "SW", "SE"),
             locale="us")
    dirs <- pm_dictionary("directional", append=my_dirs)

    df_min <- pm_postal_parse(df_min)
    df_min <- pm_state_parse(df_min)
    df_min <- pm_city_parse(df_min, dictionary=cities)
    df_min <- pm_house_parse(df_min)
    df_min <- pm_houseRange_parse(df_min)
    df_min <- pm_houseFrac_parse(df_min)

    df_min <- df_min %>%
      mutate(pm.address=str_replace_all(pm.address, "-", "")) %>%
      mutate(pm.address=str_trim(pm.address, side="left"))

    df_min <- pm_streetDir_parse(df_min, dictionary=dirs)
    df_min <- pm_streetSuf_parse(df_min)
    df_min <- pm_street_parse(df_min)
    df_parsed <- pm_replace(df_min, source=df_ident)
    df_parsed <- pm_rebuild(df_parsed,
                            output="full",
                            include_commas=TRUE,
                            keep_parsed="no",
                            keep_ids=TRUE) %>%
      select(c('pm.id', 'pm.address')) %>%
      mutate(pm.address = str_remove_all(pm.address, "\"")) %>%
      mutate(pm.address = str_replace_all(pm.address, " ,", ","))

    out.df <- df_ident %>%
      left_join(df_parsed, by='pm.id') %>%
      select(-c('pm.id', 'pm.uid', 'pm.type')) %>%
      rename('cleaned.address' = 'pm.address')

    return(out.df)
  }, error = function(w) {
    print(glue::glue("A warning popped up in add_cleaned_addresses: {w}"))
  })
}


