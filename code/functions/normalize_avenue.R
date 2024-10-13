#########################################################################

normalize_avenue <- function(street_column, street_abbreviations) {
  # Create a copy to modify, excluding NAs
  modified_column <- ifelse(is.na(street_column), street_column, street_column)
  
  for (full in names(street_abbreviations)) {
    modified_column <- ifelse(
      is.na(modified_column),
      modified_column,
      gsub(paste0("\\b", full, "\\b"), street_abbreviations[[full]], modified_column, ignore.case = TRUE)
    )
  }
  
  # Calculate changes and display sample
  changes <- street_column != modified_column & !is.na(street_column)
  affected_count <- sum(changes, na.rm = TRUE)
  
  cat("Number of entries affected by 'normalize_avenue':", affected_count, "\n")
  cat("Sample of changes (original vs modified):\n")
  
  # Display a sample of 20 rows showing before and after
  sample_rows <- data.frame(
    Original = street_column[changes][1:20],
    Modified = modified_column[changes][1:20]
  )
  print(sample_rows, row.names = FALSE)
  
  return(modified_column)  # Return the modified column
}

#########################################################################