################################################################################
# Read-in raw csv data (main_data_file)
# Convert text columns to upper case
# Convert data columns to POSIXct format
# Modify and standardize column names
# Check for presence o    f mandatory fields
# Combine NYC Agencies
# Replace missing values with NA for standardization
# Write out several files in RDS format

################################################################################
main_data_file <- "raw_data_5_years_AS_OF_10-10-2025.csv"


# Boolean flag. TRUE to redirect console output to text file
# FALSE to display  console output on the screen
enable_sink <- FALSE    

######################### #######################################################
# -------------------------------------------------------------
# 📦 INSTALL AND LOAD REQUIRED PACKAGES
# -------------------------------------------------------------
load_required_packages <- function(verbose = TRUE) {
  # Ordered to avoid common masking issues
  # Package loading order optimized for large NYC datasets
  # Load data.table LAST to preserve dplyr functionality
  
  required_packages <- c(
    # Time/date packages (load early)
    "fasttime",
    "clock", 
    "zoo",
    "lubridate",
    
    # Spatial data
    "sf",
    
    # String manipulation 
    "stringr",
    "stringdist",
    
    # Arrow for file I/O
    "arrow",
    
    # Tidyverse packages (load before data.table)
    "ggplot2",
    "dplyr", 
    "tidyverse",
    
    # Plotting extensions
    "ggpmisc",
    "gridExtra",
    "grid",
    
    # Quality control charts
    "qcc",
    "qicharts2",
    
    # Tables and reporting
    "gt",
    "DT",
    
    # Shiny and web
    "bslib",
    "shiny",
    "httr",
    
    # Development tools
    "rlang",
    "styler",
    "renv",
    
    # Load data.table LAST
    "data.table"
  )
  
  # Load all packages
  invisible(lapply(required_packages, library, character.only = TRUE))
  
  # Note: S3 method overwrite by 'ggpp' (via ggpmisc) is expected and beneficial
  # It provides enhanced titleGrob handling for better ggplot2 performance
  
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      if (verbose) message(sprintf("📦 Installing missing package: %s", pkg))
      tryCatch(
        install.packages(pkg),
        error = function(e) message(sprintf("❌ Failed to install %s: %s", 
                                            pkg, e$message))
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

################################################################################
########## Set global options for numeric values ###########
options(scipen = 999) # Set scipen option to a large value.
options(digits = 15) # Set the number of decimal places to 15, the max observed.

########## Start program timing ###########
programStart <- as.POSIXct(Sys.time())
formattedStartTime <- format(programStart, "%Y-%m-%d %H:%M:%S")

cat("\nExecution begins at:", formattedStartTime, "\n")

# Manually set the base directory
base_dir <- file.path(
  "C:",
  "Users",
  "David",
  "OneDrive",
  "Documents", 
  "datacleaningproject", 
  "nyc311clean",
  "Journal_of_Data_Science"
)

# Define the path for the main data file (CSV file)
data_dir <- file.path(base_dir, "data")

raw_data_dir <- file.path(data_dir, "raw_data")

# Define the path for the charts
chart_dir <- file.path(base_dir, "charts")

# Define the path to the directory containing the console output
console_dir <- file.path(base_dir, "console_output")

# Define the name of the corresponding RDS file
# rds_file <- gsub("\\.csv$", ".rds", main_data_file)
# rds_path <- file.path(data_dir, rds_file)

######### Commence directing console output to the file ##########
# Name the file for the for the console output
console_output_file <- file.path(
  console_dir,
  "JDS_data_prep_console_output.txt"
)

if (isTRUE(enable_sink)) {
  sink(console_output_file)
}

if (sink.number(type = "output") > 0L) {
  cat("\nExecution begins at:", formattedStartTime)
}

cat("\nExecution begins at:", formattedStartTime, "\n")

################################################################################
########## Source the function files ##########
# Source all .R files in the "functions" sub-directory
# More robust sourcing with verification

# Define the path to the directory containing your function scripts
functions_dir <- file.path(base_dir, "code", "functions")

source_functions_safely <- function(functions_dir) {
  function_files <- list.files(functions_dir, pattern = "\\.R$", full.names = TRUE)
  
  sourced_count <- 0
  failed_count <- 0
  
  for (file in function_files) {
    tryCatch({
      # Get function count before sourcing
      functions_before <- sum(sapply(ls(.GlobalEnv), function(x) is.function(get(x))))
      
      # Source the file
      source(file, local = FALSE)
      
      # Verify sourcing worked
      functions_after <- sum(sapply(ls(.GlobalEnv), function(x) is.function(get(x))))
      
      message("Successfully sourced: ", basename(file), 
              " (added ", functions_after - functions_before, " functions)")
      sourced_count <- sourced_count + 1
      
    }, error = function(e) {
      message("ERROR sourcing: ", basename(file), " - ", e$message)
      failed_count <- failed_count + 1
    })
  }
  
  message("\nSourcing complete: ", sourced_count, " files sourced, ", failed_count, " failed")
}

# Usage
source_functions_safely(functions_dir)

################################################################################
# ========= Main Execution =========

#####################

valid_date_columns <- c(
  "Created Date",
  "Closed Date",
  "Due Date",
  "Resolution Action Updated Date"
)

# Read raw 311 data
cat("\nReading in raw 311 Service Request data... \n")
main_data_path <- file.path(raw_data_dir, main_data_file)

# Example: ensure date columns come in as text
raw_data <- fread(
  main_data_path,
  nThread       = parallel::detectCores() - 1,
  check.names   = FALSE,
  strip.white   = TRUE,
  showProgress  = TRUE,
  colClasses    = "character"
)

# Make copy for troubleshooting purposes.
copy_raw_data <- raw_data

num_rows_raw_data <- nrow(raw_data)
cat("\nRaw Data row count:", format(num_rows_raw_data, big.mark = ","))

################################################################################
# Standardize column names
raw_data <- modify_column_names(raw_data)
cat("\n\nColumn names standardized")

################################################################################
# Standardize the representation of missing data to all be NA
standardize_missing_chars(raw_data)
#cat("\nMissing data representation standardized to NA.")

################################################################################
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

removed_rows <- num_rows_raw_data - nrow(raw_data)

if (removed_rows > 0) {
  cat("\n\nRows removed due to missing mandatory fields:", removed_rows, "\n")
} else {
  cat("\nNo rows removed due to missing mandatory fields.\n")
}

################################################################################
# consolidate Agencies (DCA, DOITT, NYC311-PRD)
raw_data <- consolidate_agencies((raw_data))

################################################################################
# Check date fields for missingHH:MM:SS time values

# Define date columns to be converted to POSIXct format
date_columns <- c(
  "created_date",
  "closed_date",
  "due_date",
  "resolution_action_updated_date"
)

# Example call:
summary_dt <- date_checks_character(
  DT = raw_data,
  date_cols = date_columns
)

result <- parse_date_column(
  temp_raw_data      = raw_data,
  valid_date_columns = date_columns
)

raw_data <- result$parsed_data
failed_to_parse <- result$failures
parsed_summmary <- result$summary

################################################################################
# Define columns containing text to convert to uppercase
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

# Step 1: Ensure columns where text to be converted actually exist in dataset
existing_cols <- columns_to_upper[columns_to_upper %in% names(raw_data)]

# Warn about any missing columns
missing_cols <- setdiff(columns_to_upper, existing_cols)
if (length(missing_cols) > 0) {
  cat("\n[WARNING] The following columns were not found in raw_data:\n",
      paste(" -", missing_cols, collapse = "\n"), "\n")
} 

# Step 2: Keep only those that are character columns
valid_cols <- existing_cols[sapply(raw_data[, ..existing_cols], is.character)]

# Step 3: Convert to uppercase
for (col in valid_cols) {
  raw_data[, (col) := toupper(get(col))]
  cat("\n -", col, "converted to upper case")
}

cat("\nAll text Columns converted to uppercase")

################################################################################

# Year-sliced RDS files (Calendar Years)
# - End at the latest COMPLETE CY
# - If start coverage is missing, create the LONGEST POSSIBLE subset
# - Verify by reading back and print row counts + whether truncated

# Inputs assumed:
# - raw_data: data.table with POSIXct 'created_date' in local_tz
# - data_dir: output directory
# - main_data_file: path string that includes "...AS_OF_YYYY-MM-DD.csv"

###########################
# Extract AS_OF date from filename (e.g., ...AS_OF_2025-08-10.csv -> 2025-08-10)
as_of_date <- sub(".*AS_OF_([0-9-]+)\\.csv$", "\\1", main_data_file)


local_tz <- "America/New_York"
# Coverage from the data
min_date <- suppressWarnings(min(raw_data$created_date, na.rm = TRUE))
max_date <- suppressWarnings(max(raw_data$created_date, na.rm = TRUE))

if (!is.finite(min_date) || !is.finite(max_date)) 
  stop("created_date has no finite values.")

min_date <- as.POSIXct(min_date, tz = local_tz)
max_date <- as.POSIXct(max_date, tz = local_tz)

# Spans to generate
selected_year_spans <- c(2, 5)

# Ensure output directory exists
if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)

# ============================= Calendar Years ===============================
# Find max complete year (exclude current partial year)
current_year <- year(Sys.Date())
max_complete_year <- raw_data[year(created_date) < current_year, 
                              year(max(created_date, na.rm = TRUE))]

date_tz <- attr(raw_data$created_date, "tzone")

# Collect results for summary
summary_list <- list()

for (span in selected_year_spans) {
  start_date <- as.POSIXct(
    sprintf("%d-01-01 00:00:00", max_complete_year - span + 1), tz = date_tz
  )
  end_date <- as.POSIXct(
    sprintf("%d-01-01 00:00:00", max_complete_year + 1), tz = date_tz
  )
  
  cat("\nFiltering for last", span, "complete years...\n")
  
  filtered_data <- raw_data[created_date >= start_date & created_date < end_date]
  
  if (nrow(filtered_data) == 0L) {
    cat(sprintf("No data found for %d-year span [%s .. %s]\n",
                span,
                format(start_date, "%Y-%m-%d"),
                format(end_date, "%Y-%m-%d")))
    next
  }
  
  s_date <- format(start_date, "%m-%d-%Y")
  e_date <- format(max(filtered_data$created_date), "%m-%d-%Y")
  
  file_name <- sprintf(
    "%d-year_311SR_%s_thru_%s_AS_OF_%s.rds",
    span, s_date, e_date, as_of_date
  )
  full_path <- file.path(data_dir, file_name)
  
  save_and_verify(filtered_data, full_path)
  
  # Collect summary
  summary_list[[length(summary_list) + 1]] <- data.table(
    span_years = span,
    years_included = sprintf("%d–%d", max_complete_year - span + 1, max_complete_year),
    start_date = format(start_date, "%Y-%m-%d"),
    end_date   = format(end_date - 1, "%Y-%m-%d"),  # inclusive end
    rows       = nrow(filtered_data),
    file       = basename(full_path)
  )
}

# ============================= Summary ===============================
if (length(summary_list) > 0) {
  summary_dt <- rbindlist(summary_list)
  
  # Format rows with commas
  summary_dt[, rows := prettyNum(rows, big.mark = ",")]
  
  cat("\n=== Summary of Generated Datasets ===\n")
  print(summary_dt)
}

################################################################################
usps_data_file <- "zip_code_database_UNITEDSTATESZIPCODESDOTORG_2025.csv"
usps_path      <- file.path(raw_data_dir, usps_data_file)
usps_rds_file  <- file.path(data_dir, "USPS_zipcodes.rds")

if (!file.exists(usps_path)) {
  stop("USPS CSV not found at: ", usps_path)
}

cat("\nProcessing USPS Zipcode data...\n")

# Try to read only a 'zip' column; if not found exactly, read headers and detect it
zipcode_data <- tryCatch(
  fread(
    usps_path,
    select      = "zip",
    colClasses  = c(zip = "character"),
    nThread     = max(1L, parallel::detectCores() - 1L),
    check.names = FALSE,
    strip.white = TRUE,
    showProgress = TRUE
  ),
  error = function(e) {
    # Fallback: read header to find a plausible ZIP column (case-insensitive)
    hdr <- names(fread(usps_path, nrows = 0, check.names = FALSE, showProgress = FALSE))
    cand <- grep("^zip(code)?$", hdr, ignore.case = TRUE, value = TRUE)
    if (length(cand) == 0L) stop("No 'zip' column found (case-insensitive) in USPS CSV.")
    fread(
      usps_path,
      select      = cand[1],
      colClasses  = setNames("character", cand[1]),
      nThread     = max(1L, parallel::detectCores() - 1L),
      check.names = FALSE,
      strip.white = TRUE,
      showProgress = TRUE
    )
  }
)

# Ensure the column is named exactly 'zip'
if (!identical(names(zipcode_data), "zip")) {
  setnames(zipcode_data, 1L, "zip")
}

# Light cleanup
zipcode_data[, zip := trimws(zip)]

# Save just the single column, always overwrite
saveRDS(zipcode_data[, .(zip)], usps_rds_file)

# Verify
read_back <- readRDS(usps_rds_file)
rows_written <- nrow(zipcode_data)
rows_read    <- nrow(read_back)
has_zip_char <- ("zip" %in% names(read_back)) && is.character(read_back$zip)
status <- if (rows_written == rows_read && has_zip_char) "OK" else "MISMATCH"

cat(sprintf(
  "     USPS_zipcodes.rds | written: %d | read back: %d | %s\n",
  rows_written, rows_read, status
))
 
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

# Close sink() if set
if (enable_sink){ 
  sink() 
  cat("\n\n*****END OF PROGRAM*****\n")
  cat("\n📅 Execution ends at:", formatted_end_time, "\n")
  cat("\n⏱️ Program run-time:", duration_string, "\n")
}

################################################################################
################################################################################
