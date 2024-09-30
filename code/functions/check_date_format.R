# #########################################################################
# Function to check if a column is a valid date format
check_date_format <- function(column_name) {
  date_column <- d311[[column_name]]
  date_check <- try(as.Date(date_column, format = "%m/%d/%Y %I:%M:%S %p"))
  
  if (inherits(date_check, "try-error")) {
    invalid_dates <- date_column[!grepl("^\\d{2}/\\d{2}/\\d{4} \\d{2}:\\d{2}:\\d{2} [APMapm]{2}$", date_column)]
    if (length(invalid_dates) > 0) {
      print(paste("Invalid dates in", column_name, ":", invalid_dates, sep = ""))
    }
    return(paste("Field '", column_name, "' is not in a valid date format", sep = ""))
  } else {
    return(paste("Field '", column_name, "' is in a valid date format", sep = ""))
  }
}

#########################################################################