install.packages("stringdist")
install.packages("fuzzyjoin")
install.packages('campfin')
library(stringdist)
library(fuzzyjoin)
library(campfin)

# Create example data frames
df1 <- data.frame(id = 1:3, address = c("321 main street", "123 elm st", "456 oak avenue"))
df2 <- data.frame(id = 4:6, address = c("321 Main St.", "123 Elm Street", "456 Oak Ave."))

str1 = '321 main street, Apt. 2'
str2 = '321 Main St., Apt. 2'

normal_address(str1, abbs=usps_street, abb_end=FALSE)
normal_address(str2, abbs=usps_street, abb_end=FALSE)

address_compare <- function(str1, str2) {
  cleaned1 <- campfin::normal_address(str1, abbs=usps_street, abb_end=FALSE)
  cleaned2 <- campfin::normal_address(str2, abbs=usps_street, abb_end=FALSE)
  return(cleaned1 == cleaned2)
}


address_compare(str1, str2)
address_compare('123 Main St, Apt 2', '123 main Street, Apt.2')


# Define a custom function to clean and standardize addresses
clean_address <- function(x) {
  x <- tolower(x)
  x <- gsub("\\bstreet\\b", "st", x)
  x <- gsub("\\bavenue\\b", "ave", x)
  return(x)
}

# Clean and standardize addresses in both data frames
df1$clean_address <- clean_address(df1$address)
df2$clean_address <- clean_address(df2$address)

# Perform fuzzy join using stringdist method
result <- stringdist_left_join(df1, df2, by = "clean_address", method = "jw", max_dist = .1)

result
