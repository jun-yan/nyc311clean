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

# -------------------------------------------------------------
# 📁 Set Working Directory for the Project.
# -------------------------------------------------------------
# Set working directory to the location of the initialization script
setwd("C:/Users/David/OneDrive/Documents/datacleaningproject/data_anomalies/code")
 
#########################################################################
rm(list = ls())

main_data_file <- "3-month_311SR_10-01-2024_thru_12-31-2024_AS_OF_02-02-2025.csv"

# Extract the 10-character date after "AS_OF_"
max_closed_date <- sub(".*AS_OF_([0-9-]+)\\.csv$", "\\1", main_data_file)

# Convert to POSIXct format
max_closed_date <- as.POSIXct(max_closed_date, format = "%m-%d-%Y", tz = "UTC") + (23*3600 + 59*60 + 59)

#########################################################################
programStart <- as.POSIXct(Sys.time())
formattedStartTime <- format(programStart, "%Y-%m-%d %H:%M:%S")

cat("\n***** Program initialization *****")

# Set the base directory under the working directorybase_dir <- getwd()
base_dir <- getwd()

# Define the console output directory and file name.
output_dir <- file.path(base_dir, "console_output")
output_file <- file.path(output_dir, "core_console_output.txt")

# Define the path for the main data file (CSV file)
data_dir <- file.path(base_dir, "data")

# Define the path for the charts
chart_dir <- file.path(base_dir, "charts")

# Define the path to the directory containing your function scripts
functions_path <- file.path(base_dir, "functions")

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

########## Set global options for numeric values ###########
options(scipen = 999) # Set scipen option to a large value.
options(digits = 15) # Set the number of decimal places to 15, the max observed.

cat("\nExecution begins at:", formattedStartTime)

######### Commence directing console output to the file ##########
#sink(output_file)

cat("\nExecution begins at:", formattedStartTime)

# #########################################################################

#========= Configuration =========
usps_data_file <- "zip_code_database_AS_of_02-03-2025_uszcdotorg.csv"

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

#========= Main Execution =========
library(data.table)

main <- function() {
  
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
  
  # Convert specified columns to uppercase
  cat("Converting specified columns to uppercase...\n")
  cleaned_data[, (columns_to_upper) := lapply(.SD, toupper), .SDcols = columns_to_upper]
  cat("Uppercase conversion complete\n")
  
  # Consolidate agency names
  cat("Consolidating agency names...\n")
  cleaned_data <- consolidate_agencies(cleaned_data)
  cat("Agency consolidation complete\n")
  
  # Standardize date formats
  cat("Standardizing date formats...\n")
  cleaned_data[, (date_columns) := lapply(.SD, function(x) {
    as.POSIXct(x, format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC")
  }), .SDcols = date_columns]
  cat("Date standardization complete\n")
  
  # Adjust February 29 dates
  cat("\nChecking for February 29 dates to adjust...\n")
  date_columns_present <- intersect(date_columns, names(cleaned_data))
  if (length(date_columns_present) > 0) {
    cleaned_data <- adjust_feb_29_to_28(cleaned_data, date_columns_present)
    cat("February date adjustments complete\n")
  } else {
    cat("Warning: No date columns found for February adjustment\n")
  }
  
  # Save processed data
  saveRDS(cleaned_data, rds_path)
  cat("\nSaved cleaned 311 data to:", rds_path, "\n")
  
  # Create a copy and assign to global environment
  cleaned_data_copy <- copy(cleaned_data)  # Ensure an independent copy
  assign("cleaned_data_global", cleaned_data_copy, envir = .GlobalEnv)
  
  # Process USPS data
  cat("\nProcessing USPS Zipcode data...\n")
  usps_path <- file.path(data_dir, usps_data_file)
  usps_rds_file <- gsub("\\.csv$", ".rds", usps_data_file)
  usps_rds_path <- file.path(data_dir, usps_rds_file)
  
  zipcode_data <- fread(
    usps_path,
    select = "zip",
    colClasses = "character"
  )
  
  zipcode_data <- unique(zipcode_data)
  
  zipcode_data <- modify_column_names(zipcode_data)
  saveRDS(zipcode_data, usps_rds_path)
  cat("Saved USPS zipcode data to:", usps_rds_path, "\n")
  
  # Create a copy and assign to global environment
  zipcode_data_copy <- copy(zipcode_data)
  assign("zipcode_data_global", zipcode_data_copy, envir = .GlobalEnv)
  
  # Print summary
  cat("\nSetup complete.\n")
  cat("311 data RDS:", rds_path, "\n")
  cat("USPS data RDS:", usps_rds_path, "\n")
}

#########################################################################
# Run the setup
main()

# Print the final program information to the console
cat("\n\n*****END OF PROGRAM*****\n")
cat("\n📅 Execution ends at:", formatted_end_time, "\n")
cat("\n⏱️ Program run-time:", duration_string, "\n")

#########################################################################
