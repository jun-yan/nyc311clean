
# #########################################################################
# Validate that all fields are in the list of allowable values
# This function works on two lists: dataset and list_of_valid_values
are_valid_values <- function(
    dataset,
    list_of_valid_values,
    field_name) {
  
  # Count the number of blank entires. Use later when computing percentage.
  number_of_blank_entries <- sum(!nzchar(dataset) | is.na(dataset))
  
  # Subset the vector to keep only the non-blank values
  dataset <- dataset[nzchar(dataset) & !is.na(dataset) & dataset != ""]
  dataset_length <- length(dataset)
  
  in_list <- dataset %in% list_of_valid_values
  not_in_list <- dataset[!in_list]
  
  num_invalid <- length(not_in_list)
  num_valid <- length(in_list)
  
  if (num_invalid > 0) {
    invalid_rows <- dataset[dataset %in% not_in_list]
    percentage_invalid <- round((num_invalid/(dataset_length - number_of_blank_entries)) * 100, 2)
    unique_invalid <- length(unique(not_in_list))
    
    cat(
      "\n\nThere are",
      format(num_invalid, big.mark = ","),
      "invalid", field_name, "entries \nrepresenting",
      percentage_invalid,
      "% of non-blank data,\n"
    )
    cat(
      "comprised of",
      unique_invalid,
      "different", field_name, "entries.\n"
    )
    
    # Sort the table in descending order
    sorted_invalid_table <- sort(table(not_in_list), decreasing = TRUE)
    
    # Convert the table to a data frame and calculate the percentage column
    invalid_df <- data.frame(
      invalid_names = names(sorted_invalid_table),
      count = as.numeric(sorted_invalid_table)
    )
    invalid_df$percentage <- round(prop.table(invalid_df$count) * 100, 2)
    invalid_df$cumulative_percentage <- cumsum(invalid_df$percentage)
    
    # Print the top 10 values
    cat("\nTop Ten invalid '", field_name, "':\n")
    print(head(invalid_df, 10), right = FALSE)

    results <- list(valid = all(in_list), invalid = not_in_list)
    return(results)
  } else {
    cat("\n\nAll values of '", field_name, "' are valid.")
    results <- list(all(in_list), not_in_list)
    return(results)
  }
}

#########################################################################