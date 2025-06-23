################################################################################

make_and_save_subset <- function(years_back, data = raw_data, 
                                 date_column = "created_date", 
                                 as_of = as_of_date, 
                                 out_dir = data_dir,
                                 timezone = "America/New_York") {
  stopifnot(data.table::is.data.table(data))
  stopifnot(date_column %in% names(data))
  
  # Compute time window
  start_year <- max_year - years_back + 1
  start_date <- as.POSIXct(sprintf("%d-01-01 00:00:00", start_year), tz = timezone)
  end_date <- max(data[[date_column]], na.rm = TRUE)
  
  # Filter the data
  subset <- data[get(date_column) >= start_date & get(date_column) <= end_date]
  
  # Format dates for filename
  s_date <- format(start_date, "%m-%d-%Y")
  e_date <- format(end_date, "%m-%d-%Y")
  
  # Compose filename
  file_name <- sprintf(
    "%d-year_311SR_%s_thru_%s_AS_OF_%s.rds",
    years_back, s_date, e_date, as_of
  )
  full_path <- file.path(out_dir, file_name)
  
  # Save the file
  saveRDS(subset, full_path)
  
  cat(sprintf(
    "\n📁 %d-year dataset complete... %s End date: %s",
    years_back, format(nrow(subset), big.mark = ","), 
    format(end_date, "%m-%d-%Y %H:%M:%S")
    ))
  cat("\n")
}

################################################################################