#########################################################################
# Define a function that filters based on DST days and times between 02:00:00 AM and 02:59:59 AM
filter_dst_rows <- function(data, date_column) {
  # List of DST transition days in March (for 2014-2024)
  dst_days <- c("03/09/2014", "03/08/2015", "03/13/2016", "03/12/2017", 
                "03/11/2018", "03/10/2019", "03/08/2020", "03/14/2021", 
                "03/13/2022", "03/12/2023", "03/10/2024")
  
  # Step 1: Extract the date part (MM/DD/YYYY) from the passed date column
  date_part <- substr(data[[date_column]], 1, 10)
  
  # Step 2: Extract the time part (HH:MM:SS AM/PM format)
  time_part <- substr(data[[date_column]], 12, 22)
  
  # Step 3: Filter for rows where the date is one of the DST days
  dst_day_indices <- date_part %in% dst_days
  
  # Step 4: Filter for times between 02:00:00 AM and 02:59:59 AM
  time_between_0200_0300 <- grepl("^02:[0-5][0-9]:[0-5][0-9] AM", time_part)
  
  # Step 5: Combine both conditions to filter rows
  dst_rows <- data[dst_day_indices & time_between_0200_0300, ]
  
  # Step 6: Print the result count
  cat("\nRows in", date_column, "during DST:", nrow(dst_rows), "\n")
  
  # Return the filtered rows
  return(dst_rows)
}

#########################################################################