#########################################################################

# normalize_street <- function(street_column, replacement_list) {
#   # Create a copy to modify, excluding NAs
#   modified_column <- street_column
#   
#   # Apply replacements, skipping NAs
#   for (word in names(replacement_list)) {
#     modified_column <- ifelse(
#       !is.na(modified_column),
#       gsub(paste0("\\b", word, "\\b"), replacement_list[[word]], modified_column, ignore.case = TRUE),
#       modified_column
#     )
#   }
#   
#   # Calculate changes, excluding NA comparisons
#   changes <- !is.na(street_column) & street_column != modified_column
#   affected_count <- sum(changes, na.rm = TRUE)
#   
#   # Track and count each unique change
#   if (affected_count > 0) {
#     # Filter out only the modified rows
#     modified_data <- data.frame(
#       Original = street_column[changes],
#       Modified = modified_column[changes]
#     )
#     
#     # Count the occurrences of each unique modification
#     modification_counts <- as.data.frame(table(modified_data$Original, modified_data$Modified))
#     colnames(modification_counts) <- c("Original", "Modified", "Count")
#     
#     # Filter out rows with zero counts (just in case)
#     modification_counts <- modification_counts[modification_counts$Count > 0, ]
#     
#     # Sort by Count in descending order
#     modification_counts <- modification_counts[order(-modification_counts$Count), ]
#     
#     # Display the modification count summary
#     cat("Number of entries affected by 'normalize_street':", affected_count, "\n")
# #    cat("Summary of modifications (Original vs Modified with Counts):\n")
# #    print(modification_counts, row.names = FALSE)
#     
#     # Sample printout for example (if you still want a random sample)
#     sample_size <- min(10, nrow(modified_data))
#     sample_rows <- modified_data[sample(1:nrow(modified_data), sample_size), ]
#     cat("\nSample of changes (original vs modified):\n")
#     print(sample_rows, row.names = FALSE)
#     
#   } else {
#     cat("No changes were made by 'normalize_street' for this column.\n")
#   }
#   
#   return(modified_column)  # Return the modified column
# }


normalize_street <- function(street_column, replacement_list) {
  # Create a copy to modify
  modified_column <- street_column
  
  # Create a logical vector to track which rows have been modified
  modified_flag <- rep(FALSE, length(street_column))  # Initially, no modifications
  
  # Apply replacements, skipping NAs and already modified rows
  for (word in names(replacement_list)) {
    new_column <- ifelse(
      !is.na(modified_column) & !modified_flag,
      gsub(paste0("\\b", word, "\\b"), replacement_list[[word]], modified_column, ignore.case = TRUE),
      modified_column
    )
    
    # Identify rows that have changed in this iteration
    new_modifications <- (!is.na(street_column) & new_column != modified_column & !modified_flag)
    
    # Update modified_column and modified_flag where new modifications occurred
    modified_column <- ifelse(new_modifications, new_column, modified_column)
    modified_flag <- modified_flag | new_modifications
  }
  
  # Calculate the count of affected entries
  affected_count <- sum(modified_flag, na.rm = TRUE)
  
  # Track and count each unique change
  if (affected_count > 0) {
    # Filter out only the modified rows for analysis
    modified_data <- data.frame(
      Original = as.character(street_column[modified_flag]),
      Modified = as.character(modified_column[modified_flag])
    )
    
    # Ensure we’re only counting unique modifications (Original ≠ Modified)
    modification_counts <- as.data.frame(table(modified_data$Original, modified_data$Modified))
    colnames(modification_counts) <- c("Original", "Modified", "Count")
    
    # Convert Original and Modified to characters to avoid factor comparison issues
    modification_counts$Original <- as.character(modification_counts$Original)
    modification_counts$Modified <- as.character(modification_counts$Modified)
    
    # Filter out rows where Original == Modified
    modification_counts <- modification_counts[modification_counts$Count > 0 & modification_counts$Original != modification_counts$Modified, ]
    
    # Sort by Count in descending order
    modification_counts <- modification_counts[order(-modification_counts$Count), ]
    
    # Display the modification count summary
    cat("Number of entries affected by 'normalize_street':", affected_count, "\n")
    
    # Sample printout for example (if you still want a random sample)
    sample_size <- min(10, nrow(modified_data))
    sample_rows <- modified_data[sample(1:nrow(modified_data), sample_size), ]
    sample_rows <- sample_rows[sample_rows$Original != sample_rows$Modified, ]  # Remove any rows where Original == Modified
    cat("\nSample of changes (original vs modified):\n")
    print(sample_rows, row.names = FALSE)
    
  } else {
    cat("No changes were made by 'normalize_street' for this column.\n")
  }
  
  return(modified_column)  # Return the modified column
}



#########################################################################