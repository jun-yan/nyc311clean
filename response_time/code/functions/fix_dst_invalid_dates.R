################################################################################

fix_dst_invalid_dates <- function(dt, date_columns, 
                                  format = "%m/%d/%Y %I:%M:%S %p",
                                  timezone = "America/New_York",
                                  verbose = TRUE) {
								  
  cat("\n🔄 Fixing records w/DST issues that would prevent conversion to POSIX.\n")
								  
  valid_cols <- intersect(date_columns, names(dt))
  if (length(valid_cols) == 0) return(invisible(dt))
  
  for (col in valid_cols) {
    if (verbose) cat(sprintf("🔍 Checking field %s for DST issues...\n", col))
    
    # Performance optimization: exclude NA rows upfront
    non_na_idx <- which(!is.na(dt[[col]]))
    if (length(non_na_idx) == 0) {
      if (verbose) cat(sprintf("  ✅ No non-NA dates found in %s\n\n", col))
      next
    }
    
    # Only work with non-NA values
    raw_vals_subset <- dt[[col]][non_na_idx]
    parsed_vals_subset <- as.POSIXct(raw_vals_subset, format = format, 
                                     tz = timezone)
    
    # Find which of the non-NA values failed to parse
    bad_subset_idx <- which(is.na(parsed_vals_subset))
    
    if (length(bad_subset_idx) == 0) {
      if (verbose) cat(sprintf("  ✅ No invalid dates found in %s\n\n", col))
      next
    }
    
    # Map back to original indices
    bad_idx <- non_na_idx[bad_subset_idx]
    invalid_dates <- raw_vals_subset[bad_subset_idx]
    dst_fixes <- 0
    
    # Check if invalid dates fall in typical DST transition periods
    # Spring forward: 2nd Sunday in March around 2 AM
	

    for (i in seq_along(bad_idx)) {
      original_date <- invalid_dates[i]
      
      # Detect if this looks like a spring DST issue (2 AM hr doesn't exist)
      # Pattern: dates in March with 2 AM times
      if (grepl("^03/[0-9]{1,2}/[0-9]{4} 02:[0-5][0-9]:[0-5][0-9] [AP]M$", 
                original_date)) {
        
        # Try adding 1 hour by modifying the string
        time_part <- sub("^(\\d{2}/\\d{1,2}/\\d{4}) 02:(\\d{2}:\\d{2} [AP]M)$", 
                         "\\1 03:\\2", original_date)
        
        # Test if the modified version parses correctly
        test_parsed <- as.POSIXct(time_part, format = format, tz = timezone)
        
        if (!is.na(test_parsed)) {
          # Update the original data
          dt[bad_idx[i], (col) := time_part]
          dst_fixes <- dst_fixes + 1
          
          # if (verbose) {
          #    cat(sprintf("  🔧 DST fix: '%s' → '%s'\n", original_date, 
          #               time_part))
          # }
        }
      }
    }
    
    if (dst_fixes > 0) {
      cat(sprintf("  ✅ Fixed %d DST-related dates in column %s\n\n", dst_fixes, 
                  col))
    } else {
      cat(sprintf("  ⚠️ %d invalid dates remain in column %s (not DST-related)\n", 
                  length(bad_idx), col))
    }
  }
  
  invisible(dt)
}

################################################################################