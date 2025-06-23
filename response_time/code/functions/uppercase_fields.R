################################################################################

uppercase_fields <- function(dt, columns, trim = TRUE) {
  stopifnot(data.table::is.data.table(dt))
  
  existing_cols <- intersect(columns, names(dt))
  char_cols <- existing_cols[sapply(dt[, ..existing_cols], is.character)]
  
  if (length(char_cols) == 0) {
    message("ℹ️ No character columns found to convert.")
    return(invisible(dt))
  }
  
  for (col in char_cols) {
    cat("  → Converting column", col, "to uppercase\n")
    
    # Get the column data
    col_data <- dt[[col]]
    
    # Find non-NA indices to process only those
    non_na_idx <- !is.na(col_data)
    
    if (any(non_na_idx)) {
      if (trim) {
        col_data[non_na_idx] <- trimws(col_data[non_na_idx])
      }
      
      col_data[non_na_idx] <- toupper(col_data[non_na_idx])
    }
    
    dt[[col]] <- col_data
  }
  
#  cat("✅ Converted", length(char_cols), "columns to uppercase\n")
  
  skipped <- setdiff(columns, char_cols)
  if (length(skipped) > 0) {
    cat("⚠️ Skipped", length(skipped), "columns (non-character or missing):", 
        paste(skipped, collapse = ", "), "\n")
  }
  
  invisible(dt)
}

################################################################################