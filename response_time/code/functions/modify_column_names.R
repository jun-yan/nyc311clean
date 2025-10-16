#########################################################################

# Define the function to modify column names for an entire dataset
modify_column_names <- function(dataset) {
  # Ensure input is a data.frame or data.table
  if (!is.data.frame(dataset)) {
    stop("❌ Input must be a data frame or data table.")
  }
  
  # Apply transformations to column names
  new_column_names <- names(dataset)
  new_column_names <- gsub("\\s+", "_", new_column_names) # Replace spaces with underscores
  new_column_names <- gsub("[()]", "", new_column_names)  # Remove parentheses
  new_column_names <- tolower(new_column_names)           # Convert to lowercase
  
  # Assign cleaned names back to the dataset
  names(dataset) <- new_column_names
  
  return(dataset)
}

#########################################################################