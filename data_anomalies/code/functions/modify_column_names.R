#########################################################################

# Define the function to modify column names for an entire dataset
modify_column_names <- function(dataset) {
  # Ensure the input is a data frame
  if (!is.data.frame(dataset)) {
    stop("Input must be a data frame.")
  }
  
  # Modify column names using the existing transformation logic
  new_column_names <- gsub("\\s+", "_", names(dataset))  # Replace spaces with underscores
  new_column_names <- gsub("\\(|\\)", "", new_column_names)  # Remove parentheses
  new_column_names <- tolower(new_column_names)  # Convert to lowercase
  
  # Assign modified names back to the dataset
  names(dataset) <- new_column_names
  
  # Return the updated dataset
  return(dataset)
}

#########################################################################