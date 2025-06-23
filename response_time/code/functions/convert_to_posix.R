#' Convert date fields to POSIXct format
#'
#' @param dt A data.table with date columns
#' @param date_columns Vector of column names to convert (if NULL, auto-detects)
#' @param timezone Timezone for conversion (default: "America/New_York")
#' @param verbose Logical. If TRUE, prints conversion info
#'
#' @return The modified data.table (invisibly)
convert_to_posix <- function(dt, date_columns = NULL, timezone = "America/New_York", verbose = TRUE) {
  
  library(lubridate)
  library(data.table)
  
  if (verbose) {
    cat("🕐 Converting date fields to POSIXct format...\n")
    cat(sprintf("   Timezone: %s\n\n", timezone))
  }
  
  # Auto-detect date columns if not specified
  if (is.null(date_columns)) {
    # Look for columns with "date" in the name
    date_columns <- names(dt)[grepl("date", names(dt), ignore.case = TRUE)]
    
    if (length(date_columns) == 0) {
      if (verbose) cat("  ⚠️ No date columns found (looking for 'date' in column names)\n")
      return(invisible(dt))
    }
    
    if (verbose) {
      cat(sprintf("  🔍 Auto-detected date columns: %s\n", 
                  paste(date_columns, collapse = ", ")))
    }
  }
  
  # Check that specified columns exist
  missing_cols <- date_columns[!date_columns %in% names(dt)]
  if (length(missing_cols) > 0) {
    if (verbose) cat(sprintf("  ⚠️ Missing columns: %s\n", paste(missing_cols, collapse = ", ")))
    date_columns <- date_columns[date_columns %in% names(dt)]
  }
  
  if (length(date_columns) == 0) {
    if (verbose) cat("  ⚠️ No valid date columns to convert\n")
    return(invisible(dt))
  }
  
  conversions_made <- 0 
  
  # Convert each date column
  for (col in date_columns) {
    if (verbose) {
      cat(sprintf("  📅 Converting column: %s\n", col))
#      cat(sprintf("      Current class: %s\n", paste(class(dt[[col]]), collapse = ", ")))
    }
    
    # Skip if already POSIXct
    if (inherits(dt[[col]], "POSIXct")) {
      if (verbose) cat(sprintf("      ✅ Already POSIXct - skipping\n\n"))
      next
    }
    
    original_nas <- sum(is.na(dt[[col]]))
    
    # Try to convert to POSIXct
    tryCatch({
      # First attempt: assume it's in a standard format
      if (is.character(dt[[col]]) || is.factor(dt[[col]])) {
        # Try common formats first
        sample_val <- dt[[col]][!is.na(dt[[col]])][1]
        
        # Check if it matches MM/DD/YYYY HH:MM:SS AM/PM format
        if (grepl("\\d{2}/\\d{2}/\\d{4} \\d{2}:\\d{2}:\\d{2} [AP]M", sample_val)) {
          dt[, (col) := as.POSIXct(get(col), format = "%m/%d/%Y %I:%M:%S %p", tz = timezone)]
        } else {
          # Try default parsing
          dt[, (col) := as.POSIXct(get(col), tz = timezone)]
        }
      } else if (inherits(dt[[col]], "Date")) {
        # Convert Date to POSIXct
        dt[, (col) := as.POSIXct(get(col), tz = timezone)]
      } else {
        # Try generic conversion
        dt[, (col) := as.POSIXct(get(col), tz = timezone)]
      }
      
      new_nas <- sum(is.na(dt[[col]]))
      conversions_made <- conversions_made + 1
      
      if (verbose) {
        cat(sprintf("      ✅ Converted successfully\n"))
        # cat(sprintf("      📊 NAs before: %s, after: %s\n", 
        #             format(original_nas, big.mark = ","), 
        #             format(new_nas, big.mark = ",")))
        if (new_nas > original_nas) {
          cat(sprintf("      ⚠️ Warning: %s additional NAs created during conversion\n", 
                      format(new_nas - original_nas, big.mark = ",")))
        }
        cat("\n")
      }
      
    }, error = function(e) {
      if (verbose) {
        cat(sprintf("      ❌ Conversion failed: %s\n", e$message))
        cat(sprintf("      📊 DIAGNOSTIC INFO:\n"))
        cat(sprintf("          • Column name: %s\n", col))
        cat(sprintf("          • Column class: %s\n", paste(class(dt[[col]]), collapse = ", ")))
        cat(sprintf("          • Total rows: %s\n", format(nrow(dt), big.mark = ",")))
        cat(sprintf("          • Non-NA values: %s\n", format(sum(!is.na(dt[[col]])), big.mark = ",")))
        
        # Show sample of problematic data
        sample_data <- head(dt[[col]][!is.na(dt[[col]])], 5)
        if (length(sample_data) > 0) {
          cat(sprintf("          • Sample values:\n"))
          for (i in seq_along(sample_data)) {
            cat(sprintf("            [%d] %s (%s)\n", i, as.character(sample_data[i]), class(sample_data[i])[1]))
          }
        } else {
          cat(sprintf("          • No non-NA sample values available\n"))
        }
        cat("\n")
      }
      
      stop("POSIXct conversion failed for column '", col, "'. Check diagnostic info above.")
    })
  }
  
  if (verbose) {
    cat(sprintf("  ✅ Successfully converted %s/%s date columns to POSIXct\n", 
                conversions_made, length(date_columns)))
  }
  
  invisible(dt)
}
