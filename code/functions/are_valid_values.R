# #########################################################################
# Validate that all fields are in the list of allowable values
are_valid_values <- function(
    dataset,
    listValidValues,
    field_name) {
  # Subset the vector to keep only the non-blank values
  dataset <- dataset[nzchar(dataset) & !is.na(dataset) & dataset != ""]
  
  # determine valid values
  check_list <- (dataset %in% listValidValues[, 1])
  inList <- dataset[check_list]
  notInList <- dataset[!check_list]
  num_invalid <- length(notInList)
  
  if (num_invalid > 0) {
    invalid_d311_rows <- d311[(d311[[field_name]] %in% notInList), ]
    
    # retrieve the number of blank fields for calculation purposes
    number_of_blank_entries <-
      missingDataPerColumn$blanks[missingDataPerColumn$field == field_name]
    percentage_invalid <- round((num_invalid / (num_rows_d311 - number_of_blank_entries)) * 100, 2)
    unique_invalid <- length(unique(notInList))
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
    sorted_invalid_table <-
      sort(table(notInList), decreasing = TRUE)
    
    # Convert the table to a data frame and calculate the percentage column
    invalid_df <-
      data.frame(
        invlaid_names = names(sorted_invalid_table),
        count = as.numeric(sorted_invalid_table)
      )
    invalid_df$percentage <- round(prop.table(invalid_df$count) * 100, 2)
    invalid_df <- invalid_df[order(invalid_df$percentage, invalid_df$invlaid_names, decreasing = TRUE), ]
    invalid_df$cumulative_percentage <- cumsum(invalid_df$percentage)
    
    # Print the top 10 values
    cat("\nTop Ten invalid '", field_name, "':\n")
    print(head(invalid_df, 10), right = FALSE)
    chart_title <-
      x <- rank_by_agency(invalid_d311_rows)
    results <- list(all(check_list), notInList, invalid_d311_rows)
    return(results)
  } else {
    cat("\n\nAll values of '", field_name, "'are valid.")
    results <- list(all(check_list), notInList)
    return(results)
  }
}

#########################################################################