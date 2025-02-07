#########################################################################
  
# Define the make_column_names_user_friendly function for a character vector
hygiene_make_column_names_user_friendly <- function(column_names) {
  # Check if the input is a character vector
  if (!is.character(column_names)) {
    stop("Input must be a character vector.")
  }
  
  # Replace spaces with underscores
  column_names <- gsub(
    pattern = "\\s+",
    replacement = "_",
    x = column_names
  )
  
  # Remove all parentheses
  column_names <- gsub(
    pattern = "\\(|\\)",
    replacement = "",
    x = column_names
  )
  
  # Convert to lowercase
  column_names <- tolower(column_names)
  
  # Return the updated column names
  return(column_names)
}

#########################################################################