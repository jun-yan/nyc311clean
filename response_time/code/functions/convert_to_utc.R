
#' Convert POSIXct date fields to UTC timezone
#'
#' @param dt A data.table with POSIXct date columns
#' @param date_columns Character vector of column names to convert to UTC
#' @param verbose Logical. If TRUE, prints detailed info about conversion
#' @return The modified data.table (invisibly)

convert_to_utc <- function(dt, date_columns, verbose = TRUE) {
  
  library(lubridate)
  
  stopifnot(data.table::is.data.table(dt))
  
  valid_cols <- intersect(date_columns, names(dt))
  if (length(valid_cols) == 0) {
    if (verbose) message("⚠️ No matching date columns found in dataset.")
    return(invisible(dt))
  }
  
  if (verbose) {
    cat("\n🌍 Converting POSIXct columns to UTC timezone...\n")
#    cat("   This eliminates DST effects and standardizes all timestamps\n\n")
  }
  
  # Convert each column to UTC
  for (col in valid_cols) {
    if (verbose) cat(sprintf("  🔄 Converting column %s to UTC...\n", col))
    
    # Check if column is actually POSIXct
    if (!inherits(dt[[col]], "POSIXct")) {
      if (verbose) cat(sprintf("    ⚠️ Column %s is not POSIXct - skipping\n", col))
      next
    }
    
    # Get sample of original timezone info
    original_tz <- attr(dt[[col]], "tzone")
    if (is.null(original_tz) || original_tz == "") {
      original_tz <- Sys.timezone()
    }
    
    # Count non-NA values before conversion
    non_na_before <- sum(!is.na(dt[[col]]))
    
    # Convert to UTC using lubridate
    dt[, (col) := with_tz(get(col), "UTC")]
    
    # Count non-NA values after conversion
    non_na_after <- sum(!is.na(dt[[col]]))
    
    if (verbose) {
#      cat(sprintf("    ✅ %s converted: %s → UTC\n", col, original_tz))
      cat(sprintf("    📊 Records converted: %s (lost %s due to invalid timestamps)\n\n", 
                  format(non_na_after, big.mark = ","),
                  format(non_na_before - non_na_after, big.mark = ",")))
    }
  }
  
  # if (verbose) {
  #   cat("✅ UTC conversion complete!\n")
  #   #cat("   All timestamps now in UTC - DST effects eliminated\n")
  #   
  #   # # Show sample of converted dates if available
  #   # if ("created_date" %in% valid_cols && sum(!is.na(dt$created_date)) > 0) {
  #   #   sample_date <- dt[!is.na(created_date), created_date][1]
  #   #   cat(sprintf("   Sample timestamp: %s\n", 
  #   #               format(sample_date, "%Y-%m-%d %H:%M:%S %Z")))
  #   # }
  #   # 
  #   # cat("   Ready for accurate response time calculations!\n")
  # }
  
  invisible(dt)
}