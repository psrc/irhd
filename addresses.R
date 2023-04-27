install.packages("stringdist")
install.packages("fuzzyjoin")
library(stringdist)
library(fuzzyjoin)

# Create example data frames
df1 <- data.frame(id = 1:3, address = c("321 main street", "123 elm st", "456 oak avenue"))
df2 <- data.frame(id = 4:6, address = c("321 Main St.", "123 Elm Street", "456 Oak Ave."))

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
