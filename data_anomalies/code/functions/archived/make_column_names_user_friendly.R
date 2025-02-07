#########################################################################
  
# Define the make_column_names_user_friendly function for a character vector
make_column_names_user_friendly <- function(column_names) {
  
  # Replace spaces with underscores
  column_names <- gsub(
    x = column_names,
    pattern = "\\s+",
    replacement = "_"
  )
  
  # Remove all parentheses
  column_names <- gsub(
    x = column_names,
    pattern = "\\(|\\)",
    replacement = ""
  )
  
  # Convert to lowercase
  column_names <- tolower(column_names)
  
  # Return the updated column names
  return(column_names)
}


#########################################################################