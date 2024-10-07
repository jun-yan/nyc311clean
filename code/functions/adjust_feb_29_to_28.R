#########################################################################

# Define a function to adjust February 29 to February 28 for specified date columns
adjust_feb_29_to_28 <- function(data, date_cols) {
  for (col in date_cols) {
    if (col %in% names(data)) {
      # Ensure the column is in POSIXct format; if not, attempt to convert it
      if (!inherits(data[[col]], "POSIXct")) {
        data[[col]] <- as.POSIXct(data[[col]], format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
      }
      
      # Handle invalid conversion results (NA)
      valid_indices <- !is.na(data[[col]])
      
      # Identify rows where the date occurs on February 29th among valid rows
      feb_29_indices <- which(valid_indices & format(data[[col]], "%m-%d") == "02-29")
      num_29Feb_rows <- length(feb_29_indices)
      
      if (num_29Feb_rows > 0) {
        cat("\nAdjusted", num_29Feb_rows, "rows in the", col, "column from 29 February to 28 February\n")
        head(num_29Feb_rows, 50)
        # Replace February 29 dates with February 28 of the same year
        data[[col]][feb_29_indices] <- as.POSIXct(
          paste(format(data[[col]][feb_29_indices], "%Y-02-28"),
                format(data[[col]][feb_29_indices], "%H:%M:%S")),
          tz = "America/New_York"
        )
      }
    }
  }
  return(data)
}


#########################################################################