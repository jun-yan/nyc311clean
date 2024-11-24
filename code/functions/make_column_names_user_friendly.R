#########################################################################
  # names(dataset) <- gsub(
  #   x = names(dataset),
  #   pattern = "(\\.|\\s)+",
  #   replacement = "_"
  # )
  # 
  # ## Drop the trailing "."s
  # names(dataset) <- gsub(
  #   x = names(dataset),
  #   pattern = "(_)+$",
  #   replacement = ""
  # )
  # 
  # ## Convert to lower case.
  # names(dataset) <- tolower(names(dataset))
  # 
  # ## Return the revised column names
  # return(dataset)
  #}

# Define the make_column_names_user_friendly function
make_column_names_user_friendly <- function(dataset) {
  
  # Replace dots and spaces with underscores
  names(dataset) <- gsub(
    x = names(dataset),
    pattern = "(\\.|\\s)+",
    replacement = "_"
  )
  
  # Remove all parentheses
  names(dataset) <- gsub(
    x = names(dataset),
    pattern = "\\(|\\)",
    replacement = ""
  )
  
  # Drop trailing underscores
  names(dataset) <- gsub(
    x = names(dataset),
    pattern = "_+$",
    replacement = ""
  )
  
  # Convert to lower case
  names(dataset) <- tolower(names(dataset))
  
  # Return the dataset with modified column names
  return(dataset)
}
# 
# # Apply the function
# d311 <- make_column_names_user_friendly(dataset)
# 
# # Step 2: Reattach parentheses to the "location" field
# if ("location" %in% names(dataset)) {
#   dataset$location <- paste0("(", d311$location, ")")
# }
# 
# # Print the resulting column names
# colnames(d311)



#########################################################################