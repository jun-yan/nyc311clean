# Define a function to adjust February 29 to February 28 for specified date columns
adjust_feb_29_to_28 <- function(data, date_cols) {
  
  for (col in date_cols) {
    
    if (col %in% names(data)) {
      
      # Identify rows where the date occurs on February 29
      feb_29_indices <- which(!is.na(data[[col]]) & format(data[[col]], "%m-%d") == "02-29")
      num_29Feb_rows <- length(feb_29_indices)
      
      if (num_29Feb_rows > 0) {
        cat("\nAdjusted", num_29Feb_rows, "rows in the", col, "column from 29 February to 28 February\n")
        
        # Replace February 29 dates with February 28 of the same year
        data[[col]][feb_29_indices] <- as.POSIXct(
          paste(
            format(data[[col]][feb_29_indices], "%Y-02-28"),
            format(data[[col]][feb_29_indices], "%H:%M:%S")
          ),
          tz = "UTC"
        )
      } else {
        cat("\nNo rows to adjust in the", col, "column. No February 29 dates found.\n")
      }
    }
  }
  
  return(data)
}
