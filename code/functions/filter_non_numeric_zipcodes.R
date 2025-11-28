# #########################################################################
# Function to filter rows with non-numeric or non-5-digit zip codes for a specific field
filter_non_numeric_zipcodes <- function(df, zip_field) {
  
  # Define a logical condition to filter rows based on the selected field
  condition <- !is.na(df[[zip_field]]) & df[[zip_field]] != "" &
    !grepl("^[0-9]{5}$", df[[zip_field]])
  
  # Use the condition to subset the DataFrame
  invalid_rows <- df[condition, ]
  return(invalid_rows)
}

#########################################################################
