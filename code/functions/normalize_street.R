#########################################################################

normalize_street <- function(street_column, word_to_number) {
  # Create a copy to modify, excluding NAs
  modified_column <- street_column
  
  # Apply replacements, skipping NAs
  for (word in names(word_to_number)) {
    modified_column <- ifelse(
      !is.na(modified_column),
      gsub(paste0("\\b", word, "\\b"), word_to_number[[word]], modified_column, ignore.case = TRUE),
      modified_column
    )
  }
  
  # Calculate changes, excluding NA comparisons
  changes <- !is.na(street_column) & street_column != modified_column
  affected_count <- sum(changes, na.rm = TRUE)
  
  if (affected_count > 0) {
    cat("Number of entries affected by 'normalize_street':", affected_count, "\n")
    cat("Sample of changes (original vs modified):\n")
    
    # Display a sample of 20 rows showing before and after, excluding NAs and rows with no change
    sample_rows <- data.frame(
      Original = street_column[changes][1:20],
      Modified = modified_column[changes][1:20]
    )
    print(sample_rows, row.names = FALSE)
  } else {
    cat("No changes were made by 'normalize_street' for this column.\n")
  }
  
  return(modified_column)  # Return the modified column
}

#########################################################################