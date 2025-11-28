################################################################################
################################################################################

main_data_file <-
  "311_Service_Requests_from_2010_to_Present_20250707_AS_OF_07-07-2025.csv"

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
  "response_time"
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
  "response_time_data_prep_console_output.txt"
)

#sink(console_output_file)
cat("\nExecution begins at:", formattedStartTime, "\n")

################################################################################

# ========= Main Execution =========

################################################################################

cat("\n📥 Reading in 311 Service Request data... \n")

# Construct full path to the CSV input
main_data_path <- file.path(data_dir, main_data_file)

# Validate that file exists and has a .csv extension
if (!file.exists(main_data_path)) {
  stop("❌ File not found: ", main_data_path)
}
if (!grepl("\\.csv$", main_data_file, ignore.case = TRUE)) {
  stop("❌ Expected a .csv file but got: ", main_data_file)
}

# Report file metadata
file_size_mb <- file.info(main_data_path)$size / 1e6
cat("→ Base Data File:", basename(main_data_path), "\n")
cat("→ File Size:", format(round(file_size_mb), big.mark = ","), "MB\n")

# Check the current number of threads
num_cores <- getDTthreads()

# Set the number of threads to max -1
setDTthreads(num_cores - 1)

# Read the raw CSV data
raw_data <- fread(
  main_data_path,
  check.names = FALSE,
  strip.white = TRUE,
  showProgress = TRUE
)

# Record and report row count
num_rows_raw_data <- nrow(raw_data)
cat("✅ Raw data row count:", format(num_rows_raw_data, big.mark = ","), "\n")



backup_raw_data <- raw_data # create a backup copy for ease of repeating testing



################################################################################
# Step 1: Remove rows where the mandatory fields are missing
cat("\n\n🔄 Step 1 starting:  Checking for the presence of mandatory fields...\n")

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
initial_rows <- nrow(raw_data)
for (field in present_mandatory_fields) {
  raw_data <- raw_data[!is.na(get(field)) & trimws(get(field)) != ""]
}
rows_removed <- initial_rows - nrow(raw_data)

# Print informative message
if (rows_removed > 0) {
  cat("✅ Step 1 complete:", format(rows_removed, big.mark = ","), 
      "rows removed due to missing mandatory fields\n")
} else {
  cat("✅ Step 1 complete: No rows removed - all mandatory fields present.")
}

################################################################################
# Step 2: Standardize column names
cat("\n\n🔄 Step 2 starting:  Standardizing column names and formats... \n")

raw_data <- modify_column_names(raw_data)

cat("✅ Step2 complate: Column names standardized")

################################################################################
# Step 3: Standardize all missing/absent values to N/A
cat("\n\n🔄 Step 3: Standardizing missing values to NA...\n")

corrections_made <- 0 
columns_processed <- 0

raw_data[] <- lapply(names(raw_data), function(col_name) {
  x <- raw_data[[col_name]]
  
  if(is.character(x) | is.factor(x)) {
    # Count conversions for this column
    to_convert <- x == "" | x == "N/A" | x == "NULL"
    to_convert[is.na(to_convert)] <- FALSE
    col_corrections <- sum(to_convert)
    
    corrections_made <<- corrections_made + col_corrections
    columns_processed <<- columns_processed + 1
    
    # Convert missing indicators to NA
    x[x == "" | x == "N/A" | x == "NULL"] <- NA
  }
  x
})

cat("✅ Step 3 complete:", format(corrections_made, big.mark = ","), 
    "missing values standardized across", columns_processed, "columns")

################################################################################
# Step 4: Convert selected fields to uppercase
cat("\n\n🔄 Step 4 starting: Converting selected text fields to uppercase...\n")

columns_to_upper <- c( 
  # "address_type",
  "agency",
  # "agency_name",
  "borough",
  # "bridge_highway_direction",
  # "bridge_highway_name",
  # "bridge_highway_segment",
  # "city",
  # "community_board",
   "complaint_type"
  # "cross_street_1",
  # "cross_street_2",
  # "descriptor",
  # "facility_type",
  # "incident_address",
  # "intersection_street_1",
  # "intersection_street_2",
  # "landmark",
  # "location_type",
  # "open_data_channel_type",
  # "park_borough",
  # "park_facility_name",
  # "resolution_description",
  # "road_ramp",
  # "status",
  # "street_name",
  # "taxi_company_borough",
  # "taxi_pick_up_location",
  # "vehicle_type"
)

# Convert selected fields to uppercase, trimming, standardizing "NA"-like values
raw_data <- uppercase_fields(
  dt = raw_data,
  columns = columns_to_upper,
  trim = TRUE
)

cat("✅ Step 4 complete: Selected text fields converted to uppercase.")

################################################################################
# Step 5: Consolidate agency values
cat("\n\n🔄 Step 5 starting: Consolidating agency names...\n")

# Define the known replacement pairs
agency_replacements <- c(
  "3-1-1" = "OTI",
  "311" = "OTI", 
  "DOITT" = "OTI",
  "NYC311-PRD" = "OTI",
  "DCA" = "DCWP",
  "DEPARTMENT OF CONSUMER AND WORKER PROTECTION" = "DCWP",
  "OEM" = "NYCEM"
)

# Apply strict consolidation 
raw_data <- consolidate_agencies(
  dt = raw_data,
  replacements = agency_replacements
)

cat("✅ Step 5 complete: Agency values consolidated.")

################################################################################
#  Step 6 Convert dates to UTC timezone
cat("\n\n🔄 Step 6 starting: Converting dates to POSIXct & UTC timezone...\n")

date_columns <- c("created_date",
                  "closed_date",
                  "due_date",
                  "resolution_action_updated_date")

# Step 6-1: Fix non-compliant dates with DST-related issue
raw_data <- fix_dst_invalid_dates(raw_data, date_columns)

# Step 6-2: Convert to POSIX
raw_data <- convert_to_posix(raw_data, 
                             timezone = "America/New_York", 
                             verbose = TRUE)

# Step 6-3: Convert to UTC timezone
raw_data <- convert_to_utc(raw_data, date_columns, verbose = TRUE)

cat("✅ Step 6 complete: Date fields converted to POSIXct and UTC timezone.")

################################################################################
# Step 7: Address issues with the closed_date field resulting from DST
cat("\n\n🔄 Step 7 starting:Adjusting 'closed_date' for DST-related issues..\n")

# Step 7-1: Fix spring DST anomalies
raw_data <- fix_spring_dst_utc(raw_data, verbose = TRUE)

# Step 7-2: Fix fall DST anomalies
raw_data <- fix_fall_dst_utc(raw_data, verbose = TRUE)

cat("✅ Step 7 complete: The 'closed_date' field corrected for DST.\n")

################################################################################
# Step 8: Extract AS_OF date from filename
as_of_date <- sub(".*AS_OF_([0-9-]+)\\.csv$", "\\1", main_data_file)
cat("\n\n📁 Extracted AS_OF date from filename:", as_of_date)

 # Extract year, max/min dates from raw_data
max_year <- max(year(raw_data$created_date), na.rm = TRUE)
min_date <- min(raw_data$created_date, na.rm = TRUE)
max_date <- max(raw_data$created_date, na.rm = TRUE)

# Display results
cat(sprintf("📊 Dataset Date Range Summary:\n"))
cat(sprintf("   Min Date: %s\n", format(min_date, "%Y-%m-%d %H:%M:%S %Z")))
cat(sprintf("   Max Date: %s\n", format(max_date, "%Y-%m-%d %H:%M:%S %Z")))
#cat(sprintf("   Max Year: %s\n", max_year))
cat(sprintf("   Span: %.1f years\n", 
            as.numeric(difftime(max_date, min_date, units = "days")) / 365.25))
cat("✅ Step 8 complete: 'AS-OF' date extracted. Year dates extracted.")

################################################################################
# Step 9: Generate RDS subsets for selected durations
cat("\n\n🔄 Step 9 starting:Generating RDS subset for selected durations...\n")
# 
# #year_spans <- c(3, 6, 11, 16)
# year_spans <- c(3, 6)
# 
# cat("\n💾 Preparing datasets for", paste(year_spans, collapse = ", "), 
#     "years\n")
# 
# for (years in year_spans) { 
#   make_and_save_subset(years)
# }


# Determine min and max years in the data
min_year <- year(min(raw_data$created_date, na.rm = TRUE))
max_year <- year(max(raw_data$created_date, na.rm = TRUE))

cat("\nPreparing datasets starting from specified years")

# Define the years you want to generate datasets from
start_years <- c(2010, 2015, 2020)

############
# Function to create and save subsets from a specific start year
make_and_save_subset_from_year <- function(start_year) {
  
  # Determine the start and end datetime
  start_date <- as.POSIXct(sprintf("%d-01-01 00:00:00", start_year), tz = "UTC")
  end_date <- as.POSIXct(sprintf("%d-12-31 23:59:59", max_year), tz = "UTC")
  
  # Subset the data
  subset <- raw_data[created_date >= start_date & created_date <= end_date]
  
  # Format dates for file naming
  s_date <- format(start_date, "%m-%d-%Y")
  e_date <- format(end_date, "%m-%d-%Y")
  
  # Compose filename
  file_name <- sprintf(
    "dataset_from_%s_thru_%s_ASOF_%s.rds",
    s_date, e_date, as_of_date
  )
  
  # Save
  full_path <- file.path(data_dir, file_name)
  saveRDS(subset, full_path)
  
  cat(sprintf("\nSaved dataset starting %d with %d records", start_year, nrow(subset)))
}
##########

# Loop over start years
for (start_year in start_years) {
  if (start_year < min_year) {
    cat(sprintf("\nSkipping %d: data begins later (min year: %d)", start_year, min_year))
  } else {
    make_and_save_subset_from_year(start_year)
  }
}

cat("✅ Step 9 complete: RDS datasets produced.")

################################################################################
# Store the program end time and calculate the duration
programStop <- as.POSIXct(Sys.time())
formatted_end_time <- format(programStop, "%Y-%m-%d %H:%M:%S")

# Calculate duration in seconds and decompose
duration_seconds <- as.numeric(difftime(programStop, programStart, 
                                        units = "secs"))
hours <- floor(duration_seconds / 3600)
minutes <- floor((duration_seconds %% 3600) / 60)
seconds <- round(duration_seconds %% 60, 4)

# Build the duration string
duration_string <- paste0(
  if (hours > 0) paste0(hours, " hours, ") else "",
  if (minutes > 0) paste0(minutes, " minutes, ") else "",
  seconds, " seconds"
)

# Construct the summary message
summary_message <- paste0(
  "\n\n*****END OF PROGRAM*****\n",
  "\n📅 Execution ends at: ", formatted_end_time, "\n",
  "\n⏱️ Program run-time: ", duration_string, "\n"
)

# Print summary before sink()
cat(summary_message)

# Close sink() if used
sink()

# Print again after sink() to ensure console visibility
cat(summary_message) 

################################################################################
################################################################################