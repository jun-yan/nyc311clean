# Replace the hack with this robust filename conversion:

# Create a robust filename conversion function
create_safe_filename <- function(name) {
  # Convert to uppercase
  safe_name <- toupper(name)
  
  # Replace any non-alphanumeric characters with underscores
  safe_name <- gsub("[^A-Z0-9]", "_", safe_name)
  
  # Replace multiple consecutive underscores with single underscore
  safe_name <- gsub("_+", "_", safe_name)
  
  # Remove leading and trailing underscores
  safe_name <- gsub("^_|_$", "", safe_name)
  
  return(safe_name)
}

