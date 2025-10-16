################################################################################
################################################################################
main_data_file <- "15-year_311SR_01-01-2010_thru_12-31-2024_AS_OF_05-05-2025.csv"

################################################################################
# -------------------------------------------------------------
# 📦 INSTALL AND LOAD REQUIRED PACKAGES
# -------------------------------------------------------------
load_required_packages <- function(verbose = TRUE) {
  # Ordered to avoid common masking issues
  required_packages <- c(
    "data.table",
    "arrow",
    "fasttime",
    "lubridate",
    "zoo",
    "sf",
    "dplyr",
    "stringr",
    "tidyverse",
    "ggplot2",
    "ggpmisc",
    "qcc",
    "qicharts2",
    "bslib",
    "shiny",
    "DT",
    "gridExtra",
    "grid",
    "gt",
    "styler",
    "rlang",
    "renv",
    "httr",
    "stringdist"
  )
  
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      if (verbose) message(sprintf("📦 Installing missing package: %s", pkg))
      tryCatch(
        install.packages(pkg),
        error = function(e) message(sprintf("❌ Failed to install %s: %s", pkg, e$message))
      )
    }
    
    # Try loading the package
    tryCatch({
      suppressPackageStartupMessages(library(pkg, character.only = TRUE))
      if (verbose) message(sprintf("✅ Loaded: %s", pkg))
    }, error = function(e) {
      message(sprintf("❌ Failed to load %s: %s", pkg, e$message))
    })
  }
}


# Default verbose output
load_required_packages()

# Or silent load
load_required_packages(verbose = FALSE)


################################################################################
########## Set global options for numeric values ###########
options(scipen = 999) # Set scipen option to a large value.
options(digits = 15) # Set the number of decimal places to 15, the max observed.

########## Start program timing ###########
programStart <- as.POSIXct(Sys.time())
formattedStartTime <- format(programStart, "%Y-%m-%d %H:%M:%S")

cat("\nExecution begins at:", formattedStartTime, "\n")

# Set the base directory under the working directory base_dir <- getwd()
working_dir <- getwd()
base_dir <- file.path(
  working_dir, "datacleaningproject", "nyc311clean",
  "data_quality_over_time"
)

# Define the path for the main data file (CSV file)
data_dir <- file.path(base_dir, "data")

# Define the path for the charts
chart_dir <- file.path(base_dir, "charts")

# Define the path to the directory containing your function scripts
functions_path <- file.path(base_dir, "code", "functions")

################################################################################
########## Source the function files ##########
# Get all .R files in the "functions" sub-directory
function_files <- list.files(functions_path,
  pattern = "\\.R$",
  full.names = TRUE
)

# Source each file with error handling and message logging
lapply(function_files, function(file) {
  tryCatch(
    {
      source(file)
      #    message("Successfully sourced: ", file)
    },
    error = function(e) {
      message("Error sourcing: ", file, " - ", e$message)
    }
  )
})

################################################################################
######### Commence directing console output to the file ##########
# Define the path for the console output
console_dir <- file.path(base_dir, "console_output")
console_output_file <- file.path(
  console_dir,
  "quality_over_time_console_output.txt"
)

# sink(console_output_file)
# cat("\nExecution begins at:", formattedStartTime, "\n")

################################################################################
# Function to generate and save each subset
make_and_save_subset <- function(years_back) {
  start_date <- as.POSIXct(paste0(max_year - years_back + 1, 
                                  "-01-01 00:00:00"), tz = "UTC")
  end_date <- as.POSIXct(paste0(max_year, "-12-31 23:59:59"), tz = "UTC")
  
  subset <- raw_data[created_date >= start_date & created_date <= end_date]
  
  s_date <- format(start_date, "%m-%d-%Y")
  e_date <- format(end_date, "%m-%d-%Y")
  
  file_name <- sprintf(
    "%d-year_311SR_%s_thru_%s_AS_OF_%s.rds",
    years_back, s_date, e_date, as_of_date
  )
  
  full_path <- file.path(data_dir, file_name)
  saveRDS(subset, full_path)
  
  cat(sprintf("\n%d-year dataset: %s rows written to %s\n", years_back, 
              nrow(subset), full_path))
}

################################################################################
# ========= Configuration =========

# Define columns to convert to uppercase
columns_to_upper <- c(
  "address_type",
  "agency",
  "agency_name",
  "borough",
  "bridge_highway_direction",
  "bridge_highway_name",
  "bridge_highway_segment",
  "city",
  "community_board",
  "complaint_type",
  "cross_street_1",
  "cross_street_2",
  "descriptor",
  "facility_type",
  "incident_address",
  "intersection_street_1",
  "intersection_street_2",
  "landmark",
  "location_type",
  "open_data_channel_type",
  "park_borough",
  "park_facility_name",
  "resolution_description",
  "road_ramp",
  "status",
  "street_name",
  "taxi_company_borough",
  "taxi_pick_up_location",
  "vehicle_type"
)

# Define date columns
date_columns <- c(
  "created_date",
  "closed_date",
  "due_date",
  "resolution_action_updated_date"
)

################################################################################
# ========= Main Execution =========

#####################
# Process 311 data
cat("\nReading in 311 Service Request data... \n")
main_data_path <- file.path(data_dir, main_data_file)

rds_file <- gsub("\\.csv$", ".rds", main_data_file)
rds_path <- file.path(data_dir, rds_file)

#####################
# Read only the columns you need from raw data with optimized settings
# columns_to_select <- c(
#   "Unique Key",
#   "Created Date",
#   "Closed Date",
#   "Resolution Action Updated Date",
#   "Incident Zip",
#   "Status",
#   "Community Board"
# )
#
# column_classes <- c(
#   "Created Date" = "character",
#   "Closed Date" = "character",
#   "Resolution Action Updated Date" = "character",
#   "Incident Zip" = "character",
#   "Status" = "character",
#   "Community Board" = "character"
# )

raw_data <- fread(
  main_data_path,
  # select = columns_to_select,
  # colClasses = column_classes,
  nThread = parallel::detectCores() - 1,
  check.names = FALSE,
  strip.white = TRUE,
  showProgress = TRUE
)

num_rows_raw_data <- nrow(raw_data)
cat("\nRaw Data row count:", format(num_rows_raw_data, big.mark = ","))

################################################################################
# # Extract the 10-character date after "AS_OF_"
# max_closed_date_character <- sub(
#   ".*AS_OF_([0-9-]+)\\.csv$", "\\1",
#   main_data_file
# )
# 
# # Convert to POSIXct format
# max_closed_date <- as.POSIXct(max_closed_date_character,
#                               format = "%m-%d-%Y", tz = "America/New_York"
#                               ) + (23 * 3600 + 59 * 60 + 59)

#####################
# Standardize column names
raw_data <- modify_column_names(raw_data)
cat("\nColumn names standardized")

# #####################
# Define mandatory fields and ensure they are there
mandatory_fields <- c(
  "unique_key",
  "created_date",
  "agency",
  "complaint_type",
  "status"
)

# Keep only those fields that actually exist in the data
present_mandatory_fields <- intersect(mandatory_fields, names(raw_data))

# Remove rows with NA or blank values in any present mandatory field
for (field in present_mandatory_fields) {
  raw_data <- raw_data[!is.na(get(field)) & trimws(get(field)) != ""]
}

cat("\nRows removed due to missing mandatory fields:", 
                                  num_rows_raw_data - nrow(raw_data), "\n")

#####################
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

# Step 1: Filter to columns that exist
existing_cols <- columns_to_upper[columns_to_upper %in% names(raw_data)]

# Step 2: Keep only those that are character columns
valid_cols <- existing_cols[sapply(raw_data[, ..existing_cols], is.character)]

# Step 3: Convert to uppercase
for (col in valid_cols) {
  raw_data[, (col) := toupper(get(col))]
}

cat("\nColumn names converted to uppercase")

#####################
# Standardize date formats
valid_date_columns <- intersect(date_columns, names(raw_data))

if (length(valid_date_columns) > 0) {
  cat("\nStandardizing date formats...")
  raw_data[, (valid_date_columns) := lapply(.SD, function(x) {
    as.POSIXct(x, format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC")
  }), .SDcols = valid_date_columns]
} else {
  cat("Skipping date standardization: No matching date columns found.\n")
}

cat(
  "\nCreated Dates span from",
  format(min(raw_data$created_date), "%Y-%m-%d %H:%M:%S"), "to",
  format(max(raw_data$created_date), "%Y-%m-%d %H:%M:%S", "\n")
)

#####################
# Adjust February 29 dates in all date columns using extract_feb_29()
if (length(valid_date_columns) > 0) {
  all_feb29_unique_keys <- c()

  for (col in valid_date_columns) {
    if (col %in% names(raw_data)) {
      feb29_rows <- extract_feb_29(raw_data, col)

      if (nrow(feb29_rows) > 0) {
        # Get the indices of these rows in the original raw_data
        feb29_keys <- feb29_rows$unique_key
        feb29_indices <- which(raw_data$unique_key %in% feb29_keys)

        # Replace Feb 29 with Feb 28, preserving time
        raw_data[[col]][feb29_indices] <- as.POSIXct(
          paste(
            format(raw_data[[col]][feb29_indices], "%Y-02-28"),
            format(raw_data[[col]][feb29_indices], "%H:%M:%S")
          ),
          tz = "UTC"
        )

        all_feb29_unique_keys <- unique(c(all_feb29_unique_keys, feb29_keys))

        cat(
          "\nAdjusted", length(feb29_indices), "rows in the", col,
          "column from 29 February to 28 February"
        )
      } else {
        cat("\nNo Feb 29 dates found in leap years for column", col, "\n")
      }
    }
  }

  if (length(all_feb29_unique_keys) > 0) {
    cat(
      "\nTotal unique rows adjusted with February 29 dates:",
      format(length(all_feb29_unique_keys), big.mark = ","), "\n\n"
    )
  } else {
    cat("\nNo Feb 29 dates in leap years found, skipping adjustments\n")
  }
} else {
  cat("Skipping Feb 29 adjustments: No matching date columns found.\n")
}

################################################################################
# Save the 2, 5, 10, and 15-year RDS files
# Extract AS_OF date from filename
as_of_date <- sub(".*AS_OF_([0-9-]+)\\.csv$", "\\1", main_data_file)

# Determine max year based on created_date
max_year <- year(max(raw_data$created_date, na.rm = TRUE))

cat("\nPreparing datasets for 2, 5, 10, and 15 years")
# Run for 2, 5, and 10 years
for (years in c(2, 5, 10, 15)) {
  make_and_save_subset(years)
}

################################################################################
usps_rds_file <- file.path(data_dir, "USPS_zipcodes.rds")

if (!file.exists(usps_rds_file)) {
  cat("\nProcessing USPS Zipcode data...\n")

  usps_data_file <- "zip_code_database.csv"
  usps_path <- file.path(data_dir, usps_data_file)

  columns_to_select <- c("zip")
  column_classes <- c("zip" = "character")

  # Load ZIP code data
  zipcode_data <- fread(
    usps_path,
    select = columns_to_select,
    colClasses = column_classes,
    nThread = parallel::detectCores() - 1,
    check.names = FALSE,
    strip.white = TRUE,
    showProgress = TRUE
  )

  # Filter for valid zip codes
  non_blank_zips_only <- raw_data$incident_zip[!is.na(raw_data$incident_zip) &
                                                raw_data$incident_zip != ""]

  # Get frequency count of valid ZIP codes
  zip_frequencies <- table(non_blank_zips_only)

  # Create a data table for the results
  zip_frequency_dt <- data.table(
    zip = names(zip_frequencies),
    frequency = as.numeric(zip_frequencies)
  )

  # Sort by frequency in descending order
  setorder(zip_frequency_dt, -frequency)

  # Filter frequencies to include only USPS-referenced ZIPs
  zip_frequency_dt <- zip_frequency_dt[zip %in% zipcode_data$zip]

  # Get the remaining valid zip codes that don't appear in the incident data
  unused_valid_zips <- setdiff(zipcode_data$zip, zip_frequency_dt$zip)
  unused_zips_dt <- data.table(zip = unused_valid_zips, frequency = 0)

  # Combine both and save
  ordered_zipcode_data <- rbindlist(list(zip_frequency_dt, unused_zips_dt))
  saveRDS(ordered_zipcode_data, file = usps_rds_file)

  cat("\nUSPS ZIPcode processing complete. Data saved to", usps_rds_file, "\n")
} else {
  cat("\nUSPS ZIPcode data already processed. Skipping USPS processing step.\n")
}

################################################################################
# Store the program end time and calculate the duration
programStop <- as.POSIXct(Sys.time())
formatted_end_time <- format(programStop, "%Y-%m-%d %H:%M:%S")

# Calculate the duration of the program (in seconds)
duration_seconds <- as.numeric(difftime(programStop, programStart,
  units = "secs"
))

# Convert the duration to a formatted string (hours, minutes, and seconds)
hours <- floor(duration_seconds / 3600)
minutes <- floor((duration_seconds %% 3600) / 60)
seconds <- round(duration_seconds %% 60, 4) # Round to 4 decimal places

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
# sink()

cat("\n\n*****END OF PROGRAM*****\n")
cat("\n📅 Execution ends at:", formatted_end_time, "\n")
cat("\n⏱️ Program run-time:", duration_string, "\n")

################################################################################
################################################################################
