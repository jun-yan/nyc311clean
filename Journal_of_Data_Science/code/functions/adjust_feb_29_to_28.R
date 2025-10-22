adjust_feb_29_to_28 <- function(DT, date_col) {
  require(data.table)
  
  # Ensure it's a data.table
  stopifnot(data.table::is.data.table(DT))
  stopifnot(date_col %in% names(DT))
  
  # Convert character to POSIXct if needed
  if (is.character(DT[[date_col]])) {
    cat("Converting character dates to POSIXct...\n")
    DT[, (date_col) := as.POSIXct(get(date_col), format = "%Y-%m-%d %H:%M:%S")]
  }
  
  # Check if the column is now a date/datetime type
  if (!inherits(DT[[date_col]], c("Date", "POSIXct", "POSIXt"))) {
    stop("Column must be Date, POSIXct, or convertible character format")
  }
  
  # Count Feb 29 dates before adjustment
  feb_29_count <- DT[!is.na(get(date_col)) & 
                       month(get(date_col)) == 2 & 
                       day(get(date_col)) == 29, .N]
  
  if (feb_29_count == 0) {
    cat("No February 29th dates found in column '", date_col, "'\n", sep = "")
    return(invisible(DT))
  }
  
  cat("Found", feb_29_count, "February 29th dates in column '", date_col, "'\n")
  
  # Adjust Feb 29 to Feb 28 while preserving time component
  DT[!is.na(get(date_col)) & 
       month(get(date_col)) == 2 & 
       day(get(date_col)) == 29, 
     (date_col) := get(date_col) - lubridate::days(1)]
  
  # Verify the change
  remaining_feb_29 <- DT[!is.na(get(date_col)) & 
                           month(get(date_col)) == 2 & 
                           day(get(date_col)) == 29, .N]
  
  cat("Adjusted", feb_29_count, "dates from Feb 29 to Feb 28\n")
  cat("Remaining Feb 29 dates:", remaining_feb_29, "\n")
  
  # Show sample of adjusted dates
  if (feb_29_count > 0) {
    sample_feb_28 <- DT[!is.na(get(date_col)) & 
                          month(get(date_col)) == 2 & 
                          day(get(date_col)) == 28, 
                        get(date_col)][1:min(3, .N)]
    
    cat("Sample of adjusted Feb 28 dates:\n")
    print(head(sample_feb_28, 3))
  }
  
  invisible(DT)
}