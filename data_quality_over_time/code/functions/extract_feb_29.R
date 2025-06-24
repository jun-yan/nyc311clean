################################################################################
# 📅 Function to extract February 29th rows from a data.table
extract_feb_29 <- function(dt, date_col) {
  if (!requireNamespace("data.table", quietly = TRUE) || 
      !requireNamespace("lubridate", quietly = TRUE)) {
    stop("Please ensure that 'data.table' and 'lubridate' packages are installed.")
  }
  
  # Define known leap years – you can extend this as needed
  leap_years <- seq(2000, 2100, by = 4)
  
  # Use get() to access column by name inside data.table
  date_vector <- dt[[date_col]]
  
  dt[
    lubridate::year(date_vector) %in% leap_years &
      lubridate::month(date_vector) == 2 &
      lubridate::mday(date_vector) == 29
  ]
}



################################################################################