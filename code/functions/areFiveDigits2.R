# #########################################################################
areFiveDigits2 <- function(zipcodes) {
  
  # Remove blank values and <NA> values
  non_blank_zipcodes <- zipcodes[!(nzchar(zipcodes) | is.na(zipcodes))]
  
  # Identify non-numeric and non-5-digit zipcodes using regular expression
  zipcode_pattern <- "^\\d{5}$"
  
  # identify non-compliant zipcodes
  not_valid_zipcodes <-
    non_blank_zipcodes[!grepl(zipcode_pattern, non_blank_zipcodes)]
  
  if (length(not_valid_zipcodes) > 0) {
    cat(
      "\n\n*****Non 5-digit and/or non-numeric zipcodes found: ",
      not_valid_zipcodes
    )
    return(FALSE)
  }
  return(TRUE)
}

#########################################################################