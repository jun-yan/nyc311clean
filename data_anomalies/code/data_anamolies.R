#########################################################################

########## Core Data Anomalies Program ##########

#########################################################################
# -------------------------------------------------------------
# 📦 INSTALL AND LOAD REQUIRED PACKAGES
# -------------------------------------------------------------
required_packages <- c(  
  "arrow",
  "bslib",
  "DT",
  "data.table",
  "dplyr",
  "ggplot2",
  "ggpmisc",
  "httr",
  "lubridate",
  "renv",
  "rlang",
  "scales",
  "sf",
  "shiny",
  "stringdist",
  "stringr",
  "styler",
  "tidyverse",
  "zoo"
)

# Check and install missing packages
missing_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(missing_packages)) {
  install.packages(missing_packages)
  cat("📦 Installed missing packages:", paste(missing_packages, collapse = ", "), "\n")
}

# Load the required packages
lapply(required_packages, library, character.only = TRUE)

########## Set global options for numeric values ###########
options(scipen = 999) # Set scipen option to a large value.
options(digits = 15) # Set the number of decimal places to 15, the max observed.

########## Start program timing ###########
programStart <- as.POSIXct(Sys.time())
formattedStartTime <- format(programStart, "%Y-%m-%d %H:%M:%S")

cat("\nExecution begins at:", formattedStartTime)

######### Commence directing console output to the file ##########
#sink(output_file)

cat("\nExecution begins at:", formattedStartTime)

# -------------------------------------------------------------
# 📁 Set Working Directory for the Project.
# -------------------------------------------------------------
# Set working directory to the location of the initialization script
getwd()

#########################################################################
main_data_file <- "3-month_311SR_10-01-2024_thru_12-31-2024_AS_OF_02-02-2025.csv"

# Extract the 10-character date after "AS_OF_"
max_closed_date <- sub(".*AS_OF_([0-9-]+)\\.csv$", "\\1", main_data_file)

# Convert to POSIXct format
max_closed_date <- as.POSIXct(max_closed_date, format = "%m-%d-%Y", tz = "America/New_York") + (23*3600 + 59*60 + 59)
message("\nMax closed date:", max_closed_date)

#########################################################################
# Set the base directory under the working directory base_dir <- getwd()
base_dir <-file.path("datacleaningproject", "nyc311clean","data_anomalies")

# Define the console output directory and file name.
output_dir <- file.path(base_dir, "console_output")
output_file <- file.path(output_dir, "data_anomalies_console_output.txt")

# Define the path for the main data file (CSV file)
data_dir <- file.path(base_dir, "data")

# Define the path for the charts
chart_dir <- file.path(base_dir, "charts")

# Define the path to the directory containing your function scripts
functions_path <- file.path(base_dir, "code", "functions")

########## Source the function files ##########
# Get all .R files in the "functions" sub-directory
function_files <- list.files(functions_path, pattern = "\\.R$", full.names = TRUE)

# Source each file with error handling and message logging
lapply(function_files, function(file) {
  tryCatch({
    source(file)
#    message("Successfully sourced: ", file)
  }, error = function(e) {
    message("Error sourcing: ", file, " - ", e$message)
  })
})

##########################################################################
save_shiny_data <- function(data, required_fields, app_name, filter_expr = NULL, metadata = NULL) {
  # Subset dataset based on required fields
  dataset <- data[, ..required_fields]
  
  # Apply any filtering if a filter expression is provided
  if (!is.null(filter_expr)) {
    dataset <- dataset[eval(filter_expr)]
  }
  
  # Attach metadata (only if provided)
  if (!is.null(metadata)) {
    for (name in names(metadata)) {
      attr(dataset, name) <- metadata[[name]]
    }
  }
  
  cat("\nSaving data in directory\n")
  print(output_dir)
  
  
  output_dir <- file.path("datacleaningproject", 
                          "nyc311clean",
                          "data_anomalies",
                          "shiny_apps", 
                          app_name, 
                          "data")
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  # Save dataset
  saveRDS(dataset, file = file.path(output_dir, "dataset.rds"))
  
return(invisible(NULL)) # Keeps function output clean while still printing
}

##########################################################################
#========= Configuration =========

# Define columns to convert to uppercase
columns_to_upper <- c(
  "agency",
  "agency_name",
  "complaint_type",
  "descriptor",
  "location_type",
  "incident_address",
  "street_name",
  "cross_street_1",
  "cross_street_2",
  "intersection_street_1",
  "intersection_street_2",
  "address_type",
  "city",
  "landmark",
  "facility_type",
  "status",
  "resolution_description",
  "community_board",
  "borough",
  "open_data_channel_type",
  "park_facility_name",
  "park_borough",
  "vehicle_type",
  "taxi_company_borough",
  "taxi_pick_up_location",
  "bridge_highway_name",
  "bridge_highway_direction",
  "road_ramp",
  "bridge_highway_segment"
)

# Define date columns
date_columns <- c(
  "created_date",
  "closed_date",
  "due_date",
  "resolution_action_updated_date"
)

##########################################################################
#========= Main Execution =========

# Process 311 data
cat("\nProcessing 311 Service Request data...\n")
main_data_path <- file.path(data_dir, main_data_file)
rds_file <- gsub("\\.csv$", ".rds", main_data_file)
rds_path <- file.path(data_dir, rds_file)

# Read data
raw_data <- fread(main_data_path)
cat("Initial row count:", nrow(raw_data), "\n")

# Clean column names
cleaned_data <- modify_column_names(raw_data)
cat("Column names cleaned\n")

# Make columns upper case
# Ensure the specified columns exist in the dataset before applying transformation
valid_columns <- intersect(columns_to_upper, names(cleaned_data))

# Convert only character columns in the list to uppercase
cleaned_data[, (valid_columns) := lapply(.SD, toupper), .SDcols = valid_columns]
cat("\nColumns converted to upper case")

#Consolidate agency names
  cat("Consolidating agency names...\n")
  cleaned_data <- consolidate_agencies(cleaned_data)
  cat("Agency consolidation complete\n")
  
# Standardize date formats
  cat("Standardizing date formats...\n")
  cleaned_data[, (date_columns) := lapply(.SD, function(x) {
    as.POSIXct(x, format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC")
  }), .SDcols = date_columns]
#  cat("Date standardization complete\n")
  
# Adjust February 29 dates
  cat("\nChecking for February 29 dates to adjust...\n")
  date_columns_present <- intersect(date_columns, names(cleaned_data))
  if (length(date_columns_present) > 0) {
    cleaned_data <- adjust_feb_29_to_28(cleaned_data, date_columns_present)
    cat("February date adjustments complete\n")
  } else {
    cat("Warning: No date columns found for February adjustment\n")
  }

##########################################################################
  # Ensure cleaned_data is a data.table
  setDT(cleaned_data)  
  
  # Extract Date & Hour Properly (Avoid Copying Issues)
  cleaned_data[, `:=`(
    date = as.Date(created_date),  # Extract only the date
    hour = format(created_date, "%H")  # Extract hour (00-23)
  )]
  
  # Group Data by Date & Hour (Collate Counts)
  pre_merge_counts <- cleaned_data[, .N, by = .(date, hour)][order(date, hour)]
  
  # Ensure all 24 hours are present for every date
  full_hours <- CJ(date = unique(cleaned_data$date), hour = sprintf("%02d", 0:23))  
  
  # Merge to ensure missing hours are filled with 0
  hourly_counts <- merge(full_hours, pre_merge_counts, by = c("date", "hour"), all.x = TRUE)
  hourly_counts[is.na(N), N := 0]  # Replace NA counts with 0
  
  # Save processed data
  saveRDS(cleaned_data, rds_path)
  cat("\nSaved cleaned 311 data to:", rds_path, "\n") 
  
##########################################################################    
  # Save dataset for duplicate_fields (selected location-related fields)
  save_shiny_data(cleaned_data,
                  required_fields = c("latitude", "longitude", "location", "borough",
                                      "park_borough", "taxi_company_borough", "street_name",
                                      "landmark", "cross_street_1", "cross_street_2",
                                      "intersection_street_1", "intersection_street_2"),
                  app_name = "duplicate_fields")
  
  # Save dataset for illogical_dates with max_closed_date metadata
  save_shiny_data(cleaned_data,
                  required_fields = c("created_date", "closed_date", 
                                      "due_date", "resolution_action_updated_date", "status"),
                  app_name = "illogical_dates",
                  metadata = list(max_closed_date = max_closed_date))
  
   
  # Save dataset for missing values (street and intersection-related fields)
  save_shiny_data(cleaned_data,
                  required_fields = names(cleaned_data),
                  app_name = "missing_values")
  
  # Save dataset for schema validation (street and intersection-related fields)
  save_shiny_data(cleaned_data,
                  required_fields = names(cleaned_data),
                  app_name = "schema_validator")
  
  # Save dataset for zip code validation (street and intersection-related fields)
  save_shiny_data(cleaned_data,
                  required_fields = "incident_zip",
                  app_name = "zip_validator")
  
  # # Save dataset for daylight_saving_time (only Nov 2024 created_date)
  # save_shiny_data(cleaned_data,
  #                 required_fields = "created_date",
  #                 app_name = "daylight_saving_time",
  #                 filter_expr = quote(format(created_date, "%Y-%m") == "2024-11"))
  # 
  
  
  # # Save dataset for fuzzy_matching (street and intersection-related fields)
  # save_shiny_data(cleaned_data,
  #                 required_fields = c("cross_street_1", "cross_street_2", "street_name",
  #                                     "intersection_street_1", "intersection_street_2",  
  #                                     "landmark"),
  #                 app_name = "fuzzy_matching")
  
  
##########################################################################    
  # Define the path to the directory containing your function scripts
  code_path <- file.path(base_dir, "code")
  
  # Construct the full path to the deployment script
  deploy_script <- file.path(code_path, "deploy_all_shiny_apps.R")
  
  # Source the script
  source(deploy_script)
  
##########################################################################  
  # Process USPS data

   usps_data_file <- "USPS_zipcodes.csv"

  cat("\nProcessing USPS Zipcode data...\n")
  usps_path <- file.path(data_dir, usps_data_file)
  usps_rds_file <- gsub("\\.csv$", ".rds", usps_data_file)
  
  usps_rds_path <- file.path("shiny_apps", "zip_validator", "data")
  
  zipcode_data <- fread(
    usps_path,
   select = "DELIVERY ZIPCODE",
    colClasses = "character"
  )
  
  zipcode_data <- unique(zipcode_data)
  zipcode_data <- modify_column_names(zipcode_data)
  
  usps_rds_file <- gsub("\\.csv$", ".rds", usps_data_file)
  usps_rds_dir <- file.path("shiny_apps", "zip_validator", "data")
  
  if (!dir.exists(usps_rds_dir)) dir.create(usps_rds_dir, recursive = TRUE)
  
  # Save dataset
  saveRDS(zipcode_data, file = file.path(usps_rds_dir, "USPS_zipcodes.rds"))
  
  # Print summary
  cat("\nUSPS data rds:", usps_rds_path, "\n")
  cat("\nSetup complete.\n")

##########################################################################
# Store the program end time and calculate the duration
programStop <- as.POSIXct(Sys.time())
formatted_end_time <- format(programStop, "%Y-%m-%d %H:%M:%S")

# Calculate the duration of the program (in seconds)
duration_seconds <- as.numeric(difftime(programStop, programStart, units = "secs"))

# Convert the duration to a formatted string (hours, minutes, and seconds with 4 decimal places)
hours <- floor(duration_seconds / 3600)
minutes <- floor((duration_seconds %% 3600) / 60)
seconds <- round(duration_seconds %% 60, 4)  # Round to 4 decimal places

# Create the formatted duration string
duration_string <- paste0(
  if (hours > 0) paste0(hours, " hours, ") else "",
  if (minutes > 0) paste0(minutes, " minutes, ") else "",
  seconds, " seconds"
)

# Print the final program information to the console
cat("\n\n*****END OF PROGRAM*****\n")
cat("\n📅 Execution ends at:", formatted_end_time, "\n")
cat("\n⏱️ Program run-time:", duration_string, "\n")

#########################################################################