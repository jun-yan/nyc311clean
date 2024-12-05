# #########################################################################
# Function to check if a column contains valid dates
areAllDates <- function(dateField) {
  
  # remove blank and NAs
  allDates <-
    suppressWarnings(!is.na(as.Date(dateField[dateField != ""], format = "%m/%d/%Y %I:%M:%S %p")))
  
  if (!all(allDates)) {
    # find indices of values that are not dates
    not_date_indices <-
      which(is.na(as.Date(dateField[dateField], format = "%m/%d/%Y %I:%M:%S %p")))
    
    cat("Values that are not dates: ")
    print(dateField[not_date_indices], row.names = FALSE, right = FALSE)
  }
  return(all(allDates))
}

#########################################################################