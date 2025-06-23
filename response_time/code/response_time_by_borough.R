################################################################################

############# Program to examine response time by borough over time ############

################################################################################
# Set the base directory under the working directory base_dir <- getwd()
working_dir <- getwd() 

base_dir <- file.path(working_dir, "datacleaningproject", "nyc311clean",  
                      "response_time" )

code_dir <- file.path(base_dir, "code")

# Define the path for the main data file (CSV file)
data_dir <- file.path(base_dir, "data") 

# Define the shiny app directory 
shiny_dir <- file.path(base_dir, "shiny_apps")
 
# Define the path for the charts
chart_dir <- file.path(base_dir, "charts")

cumulative_dir <- file.path(chart_dir, "cumulative_charts")

distro_dir <- file.path(chart_dir, "distribution_charts")

# Define the path to the directory containing your function scripts
functions_path <- file.path(base_dir, "code", "functions")

######### Commence directing console output to the file ##########
# Define the path for the console output
console_dir <- file.path(base_dir, "console_output")
console_output_file <- file.path(console_dir, 
                                 "response_time_console_output.txt")

################################################################################

options(warn = 2)  # Convert warnings to errors

################################################################################
################################################################################
# Select desired data file
main_data_file <- "6-year_311SR_01-01-2020_thru_06-10-2025_AS_OF_06-11-2025.rds"
#main_data_file <- 
                  "11-year_311SR_01-01-2016_thru_05-31-2025_AS_OF_06-08-2025.rds"

################################################################################
################################################################################
# -------------------------------------------------------------
# 📦 INSTALL AND LOAD REQUIRED PACKAGES
# -------------------------------------------------------------
required_packages <- c(
  
  # Core infrastructure
  "renv",         # Environment management first
  "rlang",        # Core language utilities
  
  # Data handling basics
  "data.table",   # Fast data manipulation
  "arrow",        # Apache Arrow interface
  
  # Specialized utilities
  "lubridate",    # Date handling
  "fasttime",     # Fast datetime parsing
  "stringr",      # String manipulation
  "stringdist",   # String distance calculations
  "scales",       # Scaling functions for visualization
  "sf",           # Simple features for geospatial
  "zoo",          # Time series
  
  # Statistics and quality control
  # "qcc",          # Quality control charts
  # "qicharts2",    # Quality improvement charts
  
  # Visualization and tables 
  "ggplot2",      # Plotting system
  "ggpmisc",      # ggplot2 extensions
  "grid",         # Grid graphics
  "gridExtra",    # Grid graphics extensions
  "gt",           # Grammar of tables
  # For the color palette
  "RColorBrewer",
  "reshape2",
  "viridis",
  
  # Web and API
  "httr",         # HTTP requests
  
  # Development tools
  "styler",       # Code styling
  
  # Shiny and web interfaces
  "shiny",        # Interactive web apps
  "bslib",        # Bootstrap for Shiny
  "DT",           # DataTables for R
  
  # Comprehensive packages (load last to prevent masking)
  "dplyr",        # Data manipulation
  "tidyverse"     # Collection of packages that includes dplyr, ggplot2, etc.
)

# Check and install missing packages
missing_packages <- required_packages[!(required_packages %in% 
                                          installed.packages()[, "Package"])]
if (length(missing_packages)) {
  install.packages(missing_packages)
  cat("📦 Installed missing packages:", paste(missing_packages, 
                                              collapse = ", "), "\n")
}

# Load the required packages
lapply(required_packages, library, character.only = TRUE)

################################################################################
########## Set global options for numeric values ###########
options(scipen = 999) # Set scipen option to a large value.
options(digits = 15) # Set the number of decimal places to 15, the max observed.
options(datatable.print.class = FALSE)  # Remove data class titles on printouts

########## Start program timing ###########
programStart <- as.POSIXct(Sys.time())
formattedStartTime <- format(programStart, "%Y-%m-%d %H:%M:%S")

cat("\nExecution begins at:", formattedStartTime, "\n")

################################################################################
########## Source files in the function directory ##########
# Get all .R files in the "functions" sub-directory
function_files <- list.files(
  functions_path, 
  pattern = "\\.R$", 
  full.names = TRUE)

# Source each file with error handling and message logging (only if files exist)
if (length(function_files) > 0) {
  lapply(function_files, function(file) {
    tryCatch({
      source(file)
      #    message("Successfully sourced: ", file)
    }, error = function(e) {
      message("Error sourcing: ", file, " - ", e$message)
    })
  })
}

################################################################################

#========= Main Execution =========

################################################################################
#sink(console_output_file)

cat("\nExecution begins at:", formattedStartTime)

# Process 311 data
cat("\n\nReading in 311 Service Request data...")
file_path <- file.path(data_dir, main_data_file )
cleaned_data <- readRDS(file_path)

# Ensure cleaned_data is a data.table
setDT(cleaned_data)

################################################################################      
# Vector of columns to keep
columns_to_keep <- c("unique_key",
                     "created_date",
                     "closed_date",
                     "agency",
                     "complaint_type",
                     "status",
                     "borough"
                     )

# Retain only those columns
cleaned_data <- cleaned_data[, ..columns_to_keep]

################################################################################      
# Extract year for grouping
cat("\nCreating year grouping...")

cleaned_data[, `:=` (
  year = as.integer(format(created_date, "%Y"))
#  year_month = format(created_date, "%Y-%m")
)]

################################################################################
# Create the response_time field in days
cleaned_data[, response_time := as.numeric(difftime(closed_date, created_date, 
                                                            units = "days"))]

################################################################################
# Store the original row count for percentage calculations
original_count <- nrow(cleaned_data)
cat("\n\nOriginal dataset has",format(original_count, big.mark = ","), "rows\n")

backup_cleaned_data <- cleaned_data

################################################################################
# ----- Step 1: Remove rows that do not contain valid borough names -----

# Define valid boroughs
valid_boroughs <- c("MANHATTAN", "BRONX", "BROOKLYN", "QUEENS", 
                    "STATEN ISLAND")
# Note: Removing "UNSPECIFIED" as a borough for this analysis

# Process invalid borough entries first
invalid_borough_count <- cleaned_data[!borough %in% valid_boroughs, .N]
invalid_borough_percent <- 100 * invalid_borough_count / original_count

cat("\n\n---- Records with invalid borough values ----")
cat("\nCount:", format(invalid_borough_count, big.mark = ","))
cat("\nPercentage of original dataset:", round(invalid_borough_percent, 2), "%")

# Get more insights about these records with invalid borough values
if(invalid_borough_count > 0) {
  
  # Distribution by invalid borough type
  invalid_by_borough <- cleaned_data[!borough %in% valid_boroughs, .N, 
                                     by = borough][order(-N)]
  # Add percentage and cumulative percentage columns
  invalid_by_borough[, pct := round(N/invalid_borough_count * 100, 2)]
  invalid_by_borough[, cum_pct := cumsum(pct)]
  cat("\n\nInvalid borough values:\n")
  print(as.data.frame(invalid_by_borough), right = FALSE)
  
  # Distribution by complaint type (top 10)
  invalid_by_complaint <- cleaned_data[!borough %in% valid_boroughs, .N, 
                                       by = complaint_type][order(-N)]
  # Add percentage and cumulative percentage columns
  invalid_by_complaint[, pct := round(N/invalid_borough_count * 100, 2)]
  invalid_by_complaint[, cum_pct := cumsum(pct)]
  cat("\nTop complaint types with invalid boroughs:\n")
  print(as.data.frame(head(invalid_by_complaint, 10)), right = FALSE)
  
  # Distribution by agency
  invalid_by_agency <- cleaned_data[!borough %in% valid_boroughs, .N, 
                                    by = agency][order(-N)]
  # Add percentage and cumulative percentage columns
  invalid_by_agency[, pct := round(N/invalid_borough_count * 100, 2)]
  invalid_by_agency[, cum_pct := cumsum(pct)]
  cat("\nInvalid boroughs by agency:\n")
  print(as.data.frame(invalid_by_agency), right = FALSE)
  
  # Distribution by year (of creation date)
  invalid_by_year <- cleaned_data[!borough %in% valid_boroughs, .N, 
                                  by = year][order(year)]
  
  # Add percentage and cumulative percentage columns
  invalid_by_year[, pct := round(N/invalid_borough_count * 100, 2)]
  invalid_by_year[, cum_pct := cumsum(pct)]
  cat("\nInvalid boroughs by year:\n")
  print(as.data.frame(invalid_by_year), right = FALSE)
} 

# Remove records with invalid borough values
cleaned_step1 <- cleaned_data[borough %in% valid_boroughs]

cat("\nRemoved",format(nrow(cleaned_data) - nrow(cleaned_step1), big.mark = ",")
    , "records")

cat("\nAfter removing records with invalid boroughs:", 
    format(nrow(cleaned_step1), big.mark = ","), "rows remain.")

# Update cleaned_data with removed invalid boroughs
cleaned_data <- cleaned_step1

################################################################################
# ---- Step 2: Remove rows with missing closed_date ----

# Process missing closed dates first
missing_closed_count <- cleaned_data[is.na(closed_date), .N]
missing_closed_percent <- 100 * missing_closed_count / original_count

cat("\n\n---- Records with missing closed dates ----")
cat("\nCount:", format(missing_closed_count, big.mark = ","))
cat("\nPercentage of original dataset:", round(missing_closed_percent, 2), "%")

# Get more insights about these records with missing closed dates
if(missing_closed_count > 0) {
  
  # Distribution by complaint type (top 10) with created_date
  missing_by_complaint <- cleaned_data[is.na(closed_date), .N, 
                               by = .(complaint_type, created_date)][order(-N)]
  # Add percentage and cumulative percentage columns
  missing_by_complaint[, pct := round(N/missing_closed_count * 100, 2)]
  missing_by_complaint[, cum_pct := cumsum(pct)]
  cat("\n\nTop complaint types with missing closed dates:\n")
  print(as.data.frame(head(missing_by_complaint, 10)), right = FALSE)
  
  # Distribution by borough
  missing_by_borough <- cleaned_data[is.na(closed_date), .N, 
                                     by = borough][order(-N)]
  # Add percentage and cumulative percentage columns
  missing_by_borough[, pct := round(N/missing_closed_count * 100, 2)]
  missing_by_borough[, cum_pct := cumsum(pct)]
  cat("\nMissing closed dates by borough:\n")
  print(as.data.frame(missing_by_borough), right = FALSE)
  
  # Distribution by agency
  missing_by_agency <- cleaned_data[is.na(closed_date), .N, 
                                    by = agency][order(-N)]
  # Add percentage and cumulative percentage columns
  missing_by_agency[, pct := round(N/missing_closed_count * 100, 2)]
  missing_by_agency[, cum_pct := cumsum(pct)]
  cat("\nMissing closed dates by agency:\n")
  print(as.data.frame(missing_by_agency), right = FALSE)
  
  # Distribution by year (of creation date)
  missing_by_year <- cleaned_data[is.na(closed_date), .N, by = year][order(year)]
  # Add percentage and cumulative percentage columns
  missing_by_year[, pct := round(N/missing_closed_count * 100, 2)]
  missing_by_year[, cum_pct := cumsum(pct)]
  cat("\nMissing closed dates by year:\n")
  print(as.data.frame(missing_by_year), right = FALSE)
} 

# Remove records with missing closed dates
cleaned_step2 <- cleaned_data[!is.na(closed_date)]

cat("\nRemoved", format(nrow(cleaned_data) - nrow(cleaned_step2), big.mark = ",")
                      , "records")
cat("\nAfter removing records with missing closed dates:", 
    format(nrow(cleaned_step2), big.mark = ","), "rows remain.")

# Update cleaned_data with removed missing closed_date(s)
cleaned_data <- cleaned_step2

################################################################################
# ---- Step 3: Remove rows with future closed_date ----

# Extract the AS_OF date from the filename
max_closed_date <- sub(".*AS_OF_([0-9-]+)\\.rds$", "\\1", main_data_file)

# Parse as midnight start of day in UTC
max_closed_date <- as.POSIXct(max_closed_date, format = "%m-%d-%Y", 
                              tz = "UTC")

# Add 1 day and subtract 1 second to get 23:59:59 of the same date
max_closed_date <- max_closed_date + as.difftime(1, units = "days") - 1

future_closed_data <- cleaned_data[closed_date > max_closed_date]
future_closed_count <- nrow(future_closed_data)
future_closed_percent <- 100 * future_closed_count / original_count

cat("\n\n---- Records with future closed dates (beyond", 
    format(max_closed_date, "%Y-%m-%d"), ") ----")
cat("\nCount:", future_closed_count)
cat("\nPercentage of original dataset:", round(future_closed_percent, 2), "%")

if(future_closed_count > 0) {
  
  # Show a sample of 25 records with future closed dates
  cat("\n\nSample records with future closed dates:\n")
  
  # Order by closed_date to see the most extreme cases first
  future_sample <- future_closed_data[order(-closed_date)][1:min(25, 
                                                 future_closed_count)]
  
  # Select only key columns for display
  print(as.data.frame(future_sample[, .(complaint_type, created_date, closed_date, 
                          days_to_close = as.numeric(difftime(closed_date, 
                                                created_date, units = "days")),
                          borough, agency)]), right = FALSE)
  
  # Distribution by complaint type (top 10)
  future_by_complaint <- future_closed_data[, .N, by = complaint_type][order(-N)]
  
  # Add percentage and cumulative percentage columns
  future_by_complaint[, pct := round(N/future_closed_count * 100, 1)]
  future_by_complaint[, cum_pct := cumsum(pct)]
  
  cat("\nTop complaint types with future closed dates:\n")
  print(as.data.frame(head(future_by_complaint, 10)), right = FALSE)
  
  # Distribution by borough
  future_by_borough <- future_closed_data[, .N, by = borough][order(-N)]
  
  # Add percentage and cumulative percentage columns
  future_by_borough[, pct := round(N/future_closed_count * 100, 1)]
  future_by_borough[, cum_pct := cumsum(pct)]
  
  cat("\nFuture closed dates by borough:\n")
  print(future_by_borough)
  
  # Distribution by agency
  future_by_agency <- future_closed_data[, .N, by = agency][order(-N)]
  
  # Add percentage and cumulative percentage columns
  future_by_agency[, pct := round(N/future_closed_count * 100, 1)]
  future_by_agency[, cum_pct := cumsum(pct)]
  
  cat("\nFuture closed dates by agency:\n")
  print(as.data.frame(future_by_agency), right = FALSE)
  
  # Distribution by year (of creation date)
  future_by_year <- future_closed_data[, .N, by = year][order(year)]
  
  # Add percentage and cumulative percentage columns
  future_by_year[, pct := round(N/future_closed_count * 100, 1)]
  future_by_year[, cum_pct := cumsum(pct)]
  
  cat("\nFuture closed dates by year of creation:\n")
  print(future_by_year)
  }

# Remove records with future closed dates
cleaned_step3 <- cleaned_data[closed_date <= max_closed_date]

cat("\nRemoved", nrow(cleaned_data) - nrow(cleaned_step3), "records.")

cat("\nAfter removing records with future closed dates:", 
    format(nrow(cleaned_step3), big.mark = ","), "rows remain.")

# Update cleaned_data with removed missing closed_date(s)
cleaned_data <- cleaned_step3

data_for_closed_day_analysis <- cleaned_step3  # Use in other program.

###############################################################################
#---- Step 4: Remove rows with negative response times ----
negative_count <- cleaned_data[response_time < 0, .N]
negative_percent <- 100 * negative_count / original_count

cat("\n\n---- Records with negative response times ----")
cat("\nCount:", format(negative_count, big.mark = ","))
cat("\nPercentage of original dataset:", round(negative_percent, 2), "%")

# Optional: Examine a few of these problematic records to understand the issue
if(negative_count > 0) {
  
  cat("\n\nSample of records with negative response times:\n")
  # Get a random sample of 10 rows with negative response times
  sample_rows <- cleaned_step1[response_time < 0, 
                               .(complaint_type, borough, 
                                 created_date, closed_date, 
                                 response_time = round(response_time, 1))
  ][sample(.N, min(10, .N))]
  
  # Print the random sample
  print(as.data.frame(sample_rows), right = FALSE)
  
  # Distribution by complaint type (top 10)
  negative_by_complaint <- cleaned_step1[response_time < 0, .N, 
                                         by = complaint_type][order(-N)]
  # Add percentage and cumulative percentage columns                              
  negative_by_complaint[, pct := round(N/negative_count * 100, 1)]
  negative_by_complaint[, cum_pct := cumsum(pct)]
  
  cat("\nTop complaints w/negative response times:\n")
  print(as.data.frame(head(negative_by_complaint, 10)), right = FALSE)
  
  # Distribution by borough
  negative_by_borough <- cleaned_step1[response_time < 0, .N, 
                                       by = borough][order(-N)]
  # Add percentage and cumulative percentage columns
  negative_by_borough[, pct := round(N/negative_count * 100, 1)]
  negative_by_borough[, cum_pct := cumsum(pct)]
  
  cat("\nNegative response times by borough:\n")
  print(as.data.frame(negative_by_borough), right = FALSE)
  
  # Distribution by agency
  negative_by_agency <- cleaned_step1[response_time < 0, .N, 
                                      by = agency][order(-N)]
  # Add percentage and cumulative percentage columns
  negative_by_agency[, pct := round(N/negative_count * 100, 1)]
  negative_by_agency[, cum_pct := cumsum(pct)]
  
  cat("\nNegative response times by agency:\n")
  print(as.data.frame(negative_by_agency), right = FALSE)
}

# Remove records with negative response times
cleaned_step4 <- cleaned_data[response_time >= 0]

cat("\nRemoved", 
    format(nrow(cleaned_data) - nrow(cleaned_step4), big.mark = ","), 
    "records")

cat("\nAfter removing records with negative response times:",
    format(nrow(cleaned_step4), big.mark = ","), "rows remain")

# Update cleaned_data with removed missing closed_date(s)
cleaned_data <- cleaned_step4

################################################################################
# ---- Step 5: Remove rows with zero response times ----
zero_count <- cleaned_data[response_time == 0, .N]
zero_percent <- 100 * zero_count / original_count

cat("\n\n---- Records with zero response times ----")
cat("\nCount:", format(zero_count, big.mark = ","))
cat("\nPercentage of original dataset:", round(zero_percent, 2), "%")

cat("\n\nSample of records with zero response times:\n")
# Get a random sample of 10 rows with zero response times
sample_rows <- cleaned_step2[response_time == 0, 
             .(complaint_type, borough, 
             created_date, closed_date, 
             response_time = round(response_time, 1))][sample(.N, min(10, .N))]

# Print the random sample
print(as.data.frame(sample_rows), right = FALSE)

# Optional: Examine distribution of zero response times
if(zero_count > 0) {
  
  # Distribution by complaint type (top 10)
  zero_by_complaint <- cleaned_step2[response_time == 0, .N, 
                                     by = complaint_type][order(-N)]
  
  # Add percentage and cumulative percentage columns
  zero_by_complaint[, pct := round(N/zero_count * 100, 1)]
  zero_by_complaint[, cum_pct := cumsum(pct)]
  
  cat("\nTop complaint types with zero response times:\n")
  print(as.data.frame(head(zero_by_complaint, 10)), right = FALSE)
  
  # Distribution by borough
  zero_by_borough <- cleaned_step2[response_time == 0, .N, 
                                   by = borough][order(-N)]
  # Add percentage and cumulative percentage columns
  zero_by_borough[, pct := round(N/zero_count * 100, 1)]
  zero_by_borough[, cum_pct := cumsum(pct)]
  
  cat("\nZero response times by borough:\n")
  print(as.data.frame(zero_by_borough), right = FALSE)
  
  # Distribution by agency
  zero_by_agency <- cleaned_step2[response_time == 0, .N, 
                                  by = agency][order(-N)]
  # Add percentage and cumulative percentage columns
  zero_by_agency[, pct := round(N/zero_count * 100, 1)]
  zero_by_agency[, cum_pct := cumsum(pct)]
  
  cat("\nZero response times by agency:\n")
  print(as.data.frame(zero_by_agency), right = FALSE)
  
  # Distribution by year
  zero_by_year <- cleaned_step2[response_time == 0, .N, by = year][order(year)]
  
  # Add percentage and cumulative percentage columns
  zero_by_year[, pct := round(N/zero_count * 100, 1)]
  zero_by_year[, cum_pct := cumsum(pct)]
  
  cat("\nZero response times by year:\n")
  print(zero_by_year)
}

# Remove records with zero response times
cleaned_step5 <- cleaned_data[response_time > 0]

cat("\nRemoved", 
    format(nrow(cleaned_data) - nrow(cleaned_step5), big.mark = ","), 
    "records.")

cat("\nAfter removing records with zero response times:", 
                  format(nrow(cleaned_step5), big.mark = ","), "rows remain")

# Update cleaned_data with removed missing closed_date(s)
cleaned_data <- cleaned_step5

################################################################################
# ---- Step 6: Remove rows with count(complaint_types) < threshold ----

# Set threshold for complaint removal. 
# Remove rows where  total count of complaint_type is less than this threshold.
complaint_type_removal_threshold <- 10

# Calculate total count for each complaint type across all data
complaint_type_counts <- cleaned_data[, .N, by = complaint_type][order(-N)]

# Add percentage column
complaint_type_counts[, pct := round(N/nrow(cleaned_step3) * 100, 2)]
complaint_type_counts[, cum_pct := cumsum(pct)]

# Identify complaint types with counts < 500 and add pct and cum_pct columns
low_count_complaints <- complaint_type_counts[N < 
        complaint_type_removal_threshold][, `:=`(pct = round(N / sum(N) * 100, 1),
         cum_pct = round(cumsum(N) / sum(N) * 100, 1) )]

keep_complaints <- complaint_type_counts[N >= complaint_type_removal_threshold]

low_count_total <- sum(low_count_complaints$N)
low_count_percent <- 100 * low_count_total / original_count

cat("\n\n---- Complaint types with total count <", 
    complaint_type_removal_threshold, " ----")
cat("\nNumber of complaint types with <", complaint_type_removal_threshold,
    "records:", nrow(low_count_complaints))
cat("\nTotal records in low-count complaint types:", format(low_count_total, 
                                                            big.mark = ","))
cat("\nPercentage of original dataset:", round(low_count_percent, 2), "%\n")

if(nrow(low_count_complaints) > 0) {
  
  cat("\nComplaint types being removed (showing first 20):")
  print(as.data.frame(head(low_count_complaints[, .(complaint_type, N, 
                                                    pct, cum_pct)], 20)), right = FALSE)
  
  if(nrow(low_count_complaints) > 20) {
    cat(paste("\n... and", nrow(low_count_complaints) - 20, 
              "more complaint types\n"))
  }
  
  # Show distribution by agency for removed complaint types
  removed_by_agency <- cleaned_step3[complaint_type %in% 
                                       low_count_complaints$complaint_type, 
                                     .N, by = agency][order(-N)]
  removed_by_agency[, pct := round(N/low_count_total * 100, 1)]
  
  cat("\nRemoved records by agency:\n")
  print(as.data.frame(removed_by_agency), right = FALSE)
  
  # Show distribution by borough for removed complaint types
  removed_by_borough <- cleaned_step3[complaint_type %in% 
                                        low_count_complaints$complaint_type, 
                                      .N, by = borough][order(-N)]
  removed_by_borough[, pct := round(N/low_count_total * 100, 1)]
  
  cat("\nRemoved records by borough:\n")
  print(as.data.frame(removed_by_borough), right = FALSE)
  
  # Show distribution by year for removed complaint types
  removed_by_year <- cleaned_step3[complaint_type %in% 
                                     low_count_complaints$complaint_type, 
                                   .N, by = year][order(year)]
  removed_by_year[, pct := round(N/low_count_total * 100, 1)]
  
  cat("\nRemoved records by year:\n")
  print(removed_by_year)
}

# Remove complaint types with counts < complaint_type_removal_threshold
cleaned_step6 <- cleaned_data[complaint_type %in% keep_complaints$complaint_type]

cat("\nRemoved", 
    format(nrow(cleaned_data) - nrow(cleaned_step6), big.mark = ","), 
    "records.")
cat("\nAfter removing complaint types with <", complaint_type_removal_threshold, 
    "records:", format(nrow(cleaned_step6), big.mark = ","), "rows remain\n")

# Update cleaned_data with removed missing closed_date(s)
cleaned_data <- cleaned_step6

################################################################################
# ---- Step 7: Remove Agencies with total row count < threshold ----

# Set threshold for agency removal. 
# Remove rows where the total count of agency is less than this threshold.
agency_removal_threshold <- 10

# Calculate total count for each agency across all data
agency_counts <- cleaned_data[, .N, by = agency][order(-N)]

# Add percentage column
agency_counts[, pct := round(N/nrow(cleaned_step4) * 100, 2)]
agency_counts[, cum_pct := cumsum(pct)]

# Identify agencies with counts < 500
low_count_agencies <- agency_counts[N < agency_removal_threshold]
keep_agencies <- agency_counts[N >= agency_removal_threshold]

low_count_agency_total <- sum(low_count_agencies$N)
low_count_agency_percent <- 100 * low_count_agency_total / original_count

cat("\n\n---- Agencies with total count <", agency_removal_threshold, "----")
cat("\nNumber of agencies with <", agency_removal_threshold, "records:", 
    nrow(low_count_agencies))
cat("\nTotal records in low-count agencies:", format(low_count_agency_total, 
                                                     big.mark = ","))
cat("\nPercentage of original dataset:", round(low_count_agency_percent, 4), 
    "%\n")

if(nrow(low_count_agencies) > 0) { 
  
  cat("Agencies being removed:\n")
  print(low_count_agencies[, .(agency, N, pct)])
  
  cat("\nAgencies being kept:\n")
  print(keep_agencies[, .(agency, N, pct)])
  
  # Show distribution by complaint type for removed agencies
  removed_by_complaint <- cleaned_step4[agency %in% low_count_agencies$agency, 
                                        .N, by = complaint_type][order(-N)]
  removed_by_complaint[, pct := round(N/low_count_agency_total * 100, 1)]
  
  cat("\nTop 10 complaint types in removed agencies:\n")
  print(head(removed_by_complaint, 10))
  
  # Show distribution by borough for removed agencies
  removed_by_borough <- cleaned_step4[agency %in% low_count_agencies$agency, 
                                      .N, by = borough][order(-N)]
  removed_by_borough[, pct := round(N/low_count_agency_total * 100, 1)]
  
  cat("\nRemoved records by borough:\n")
  print(removed_by_borough)
  
  # Show distribution by year for removed agencies
  removed_by_year <- cleaned_step4[agency %in% low_count_agencies$agency, 
                                   .N, by = year][order(year)]
  removed_by_year[, pct := round(N/low_count_agency_total * 100, 1)]
  
  cat("\nRemoved records by year:\n")
  print(removed_by_year)
  
  # Show which complaint types will be lost entirely (if any)
  complaint_types_before <- unique(cleaned_step4$complaint_type)
  complaint_types_after <- unique(cleaned_step4[agency %in% 
                                                  keep_agencies$agency]$complaint_type)
  lost_complaint_types <- setdiff(complaint_types_before, complaint_types_after)
  
  if(length(lost_complaint_types) > 0) {
    cat("\nComplaint types that will be completely lost:\n")
    lost_complaint_stats <- cleaned_step4[complaint_type %in% 
                                            lost_complaint_types, 
                                          .N, by = complaint_type][order(-N)]
    print(lost_complaint_stats)
  } else {
    cat("\nNo complaint types will be completely lost.\n")
  }
}

# Remove agencies with counts < threshold
cleaned_step7 <- cleaned_data[agency %in% keep_agencies$agency]

cat("\nRemoved", 
    format(nrow(cleaned_data) - nrow(cleaned_step7), big.mark = ","), 
    "records.")

cat("\nAfter removing agencies with <", agency_removal_threshold, "records:", 
    format(nrow(cleaned_step7), big.mark = ","), "rows remain.")

# Update cleaned_data with removed missing closed_date(s)
cleaned_data <- cleaned_step7

################################################################################
# ---- Step 8: Remove rows with response_time < threshold ----

# Set threshold for response time removal (in seconds)
response_time_removal_threshold_seconds <-15  # in seconds

# Convert seconds to days
response_time_removal_threshold_days <- 
                        response_time_removal_threshold_seconds / (24 * 60 * 60)

# Count records that will be removed
small_response_time_records <- cleaned_data[response_time < 
                                          response_time_removal_threshold_days]

small_response_time_count <- nrow(small_response_time_records)
small_response_time_percent <- 100 * small_response_time_count / original_count

cat("\n\n---- Rows with response_time <", response_time_removal_threshold_seconds, 
    "seconds (", round(response_time_removal_threshold_days, 6), "hours) ----")

cat("\nNumber of records with response_time <", 
    response_time_removal_threshold_seconds, "seconds:", 
    format(small_response_time_count, big.mark = ","))

cat("\nPercentage of original dataset:", round(small_response_time_percent, 4), 
    "%\n")

if(small_response_time_count > 0) {
  
  # Show distribution of small response times
  response_time_dist <- small_response_time_records[, .N, 
                                                    by = response_time][order(response_time)]
  response_time_dist[, pct := round(N/small_response_time_count * 100, 1)]
  response_time_dist[, cum_pct := cumsum(pct)]
  
  cat("\nDistribution of response times <", 
      response_time_removal_threshold_seconds, "seconds:\n")
  print(response_time_dist)
  
  # Show distribution by agency for removed records
  removed_by_agency <- small_response_time_records[, .N, by = agency][order(-N)]
  removed_by_agency[, pct := round(N/small_response_time_count * 100, 1)]
  
  cat("\nRemoved records by agency:\n")
  print(as.data.frame(head(removed_by_agency, 10)), right = FALSE)
  
  # Show distribution by complaint type for removed records
  removed_by_complaint <- small_response_time_records[, 
                                                      .N, by = complaint_type][order(-N)]
  removed_by_complaint[, pct := round(N/small_response_time_count * 100, 1)]
  
  cat("\nTop 10 complaint types in removed records:\n")
  print(as.data.frame(head(removed_by_complaint, 10)), right = FALSE)
  
  # Show distribution by borough for removed records
  removed_by_borough <- small_response_time_records[, .N, 
                                                    by = borough][order(-N)]
  removed_by_borough[, pct := round(N/small_response_time_count * 100, 1)]
  
  cat("\nRemoved records by borough:\n")
  print(as.data.frame(removed_by_borough), right = FALSE)
  
  # Show distribution by year for removed records
  removed_by_year <- small_response_time_records[, .N, by = year][order(year)]
  removed_by_year[, pct := round(N/small_response_time_count * 100, 1)]
  
  cat("\nRemoved records by year:\n")
  print(removed_by_year)
  
  # Show some examples of the small response times
  cat("\nSample of records being removed (first 20):\n")
  sample_records <- head(small_response_time_records[, .(agency, complaint_type,
                                                         borough,
                                                         response_time,
                                                         created_date,
                                                         closed_date)], 20)
  print(sample_records)
}

# Remove records with small response times
cleaned_step8 <- cleaned_data[response_time >= 
                                          response_time_removal_threshold_days]

cat("\nRemoved",
    format(nrow(cleaned_data) - nrow(cleaned_step8), big.mark = ","), "records.")

cat("\nAfter removing records with response_time <", 
    response_time_removal_threshold_seconds, "seconds:",
    format(nrow(cleaned_step8), big.mark = ","), "rows remain.")

# Update cleaned_data with removed missing closed_date(s)
cleaned_data <- cleaned_step8

################################################################################
# ---- Final Summary of all cleaning operations ----
total_removed <- original_count - nrow(cleaned_data)
total_removed_percent <- 100 * total_removed / original_count

cat("\n\n\n---- Summary of data cleaning operations ----")
cat("\nOriginal dataset size:", format(original_count, big.mark = ","), "rows")

cat("\nFinal dataset size:", format(nrow(cleaned_data), big.mark = ","), "rows")

cat("\nTotal records removed:", format(total_removed, big.mark = ","), 
    "(", round(total_removed_percent, 2), "%)")

cat("\n- Missing closed dates:", format(missing_closed_count, big.mark = ","),
    "(", round(missing_closed_percent, 2), "%)\n")

if(exists("future_closed_count")) {
  cat("- Future closed dates:", format(future_closed_count, big.mark = ","),
      "(", round(future_closed_percent, 2), "%)\n")
}

cat("- Negative response times:", format(negative_count, big.mark = ","),
    "(", round(negative_percent, 2), "%)\n")

cat("- Zero response times:", format(zero_count, big.mark = ","),
    "(", round(zero_percent, 2), "%)\n")

cat("- Low-count complaint types (<", complaint_type_removal_threshold, "):", 
    format(low_count_total, big.mark = ","),
    "(", round(low_count_percent, 2), "%)\n")

cat("- Low-count agencies (<", agency_removal_threshold,"):", 
    format(low_count_agency_total, big.mark = ","),
    "(", round(low_count_agency_percent, 2), "%)\n")

cat("- Small response times (<", response_time_removal_threshold_seconds, 
    "seconds):", format(small_response_time_count, big.mark = ","), "(", 
    round(small_response_time_percent, 2), "%)\n")

# Replace the original data with the fully cleaned version
rm(cleaned_step1, cleaned_step2, cleaned_step3, cleaned_step4, cleaned_step5, 
   cleaned_step6, cleaned_step7, cleaned_step8)  # Clean up temporary variables

# Compute final totals
n_complaint_types <- uniqueN(cleaned_data$complaint_type)
n_boroughs <- uniqueN(cleaned_data$borough)
n_agencies <- uniqueN(cleaned_data$agency)

cat("\nNumber of unique agencies (after filtering):", n_agencies)

# Show final agency distribution
final_agency_distribution <- cleaned_data[, .N, by = agency][order(-N)]
final_agency_distribution[, pct := round(N/nrow(cleaned_data) * 100, 2)]
final_agency_distribution[, cum_pct := cumsum(pct)]

cat("\nFinal agency distribution:\n")
print(as.data.frame(final_agency_distribution), right = FALSE)

# Show final complaint type distribution
final_complaint_distribution <- cleaned_data[, .N, 
                                             by = complaint_type][order(-N)]
final_complaint_distribution[, pct := round(N/nrow(cleaned_data) * 100, 2)]
final_complaint_distribution[, cum_pct := cumsum(pct)]

cat("\nFinal complaint type distribution (top 15):\n")
print(as.data.frame(head(final_complaint_distribution, 15)), right = FALSE) 

################################################################################

# Call R program to produce "removed records" charts and summary
result <- create_before_after_summary(
  before_data = backup_cleaned_data,
  after_data = cleaned_data
)

################################################################################
################################################################################

# Analyze all agencies from 2020 onwards.Produce cumulative % charts.
plots <- create_closed_date_analysis(cleaned_data, 
                                     target_year = 2020)

################################################################################
#################################################################################
# Loop through all agencies and create response time analysis PDFs

# Get list of all agencies
agencies <- sort(unique(cleaned_data$agency))

# Create summary table to track results
summary_results <- data.table(
  agency = character(),
  total_records = integer(),
  valid_response_times = integer(),
  mean_response_days = numeric(),
  median_response_days = numeric(),
  min_response_days = numeric(),
  max_response_days = numeric(),
  analysis_status = character()
)

# Loop through all agencies
for (i in 1:length(agencies)) {

  current_agency <- agencies[i]
  cat("\nProcessing Agency", i, "of", length(agencies), ":", current_agency, 
      "\n")
  
  # # Clean agency name for filename (remove special characters)
  # safe_agency_name <- gsub("[^A-Za-z0-9]", "_", current_agency)
  
  tryCatch({
    
    # Analyze response times for current agency
    result <- analyze_response_times(
      target_agency = current_agency,
      year_filter = NULL,
      max_response_days = NULL
    )

    # Add to summary results
    summary_results <- rbind(summary_results, data.table(
      agency = current_agency,
      total_records = result$data_count,
      valid_response_times = result$summary_stats$count,
      mean_response_days = round(result$summary_stats$mean, 2),
      median_response_days = round(result$summary_stats$median, 2),
      min_response_days = result$summary_stats$min,
      max_response_days = result$summary_stats$max,
      analysis_status = "Success"
    ))
    
  }, error = function(e) {
    
    # Handle errors (e.g., no valid data for agency)
    cat("  ✗ Error processing", current_agency, "\n")
    cat("    Error message:", e$message, "\n")
    cat("    Error class:", class(e), "\n")
    cat("    Call that failed:", deparse(e$call), "\n")
    
    # Check basic data availability for this agency
    agency_check <- cleaned_data[agency == current_agency]
    cat("    Agency record count:", nrow(agency_check), "\n")
    if(nrow(agency_check) > 0) {
      cat("    Records with response_time:", 
                                  sum(!is.na(agency_check$response_time)), "\n")
      if(sum(!is.na(agency_check$response_time)) > 0) {
        cat("    Response time range:", 
            min(agency_check$response_time, na.rm = TRUE), "to",
            max(agency_check$response_time, na.rm = TRUE), "days\n")
      }
    }
    cat("    Full error object:\n")
    print(e)
    cat("\n")
    
    summary_results <<- rbind(summary_results, data.table(
      agency = current_agency,
      total_records = NA_integer_,
      valid_response_times = NA_integer_,
      mean_response_days = NA_real_,
      median_response_days = NA_real_,
      min_response_days = NA_real_,
      max_response_days = NA_real_,
      analysis_status = paste("Error:", e$message)
    ))
  })
}

# Save summary results
summary_file <- file.path(cumulative_dir, "analysis_summary.csv")
fwrite(summary_results, summary_file)

successful_analyses <- summary_results[analysis_status == "Success", .N]
total_agencies <- nrow(summary_results)

cat("Successfully analyzed:", successful_analyses, "out of", total_agencies, 
    "agencies\n")
#cat("Summary file:", summary_file, "\n")

# Display summary statistics
if (successful_analyses > 0) {
  cat("\nTop 5 agencies by median response time:\n")
  top_agencies <- summary_results[analysis_status == 
                          "Success"][order(median_response_days)][1:min(5, .N)]
  print(top_agencies[, .(agency, median_response_days, mean_response_days)])
  
  cat("\nAgencies with errors:\n")
  error_agencies <- summary_results[analysis_status != "Success"]
  if (nrow(error_agencies) > 0) {
    print(error_agencies[, .(agency, analysis_status)])
  } else {
    cat("None\n")
  }
}

################################################################################

# Call function for violin and bar charts showing response_time distribution
 # Run analysis for all three types
comprehensive_results <- run_comprehensive_analysis(
  data = cleaned_data,
  distro_dir = distro_dir,
  analysis_types = c("agency", "borough", "complaint_type"),
#  analysis_types = c("agency", "borough"),
  min_records_threshold = 200000
)

################################################################################
# Pre-compute all possible aggregations for Shiny app, now including agency

# 1. Overall aggregation (no grouping)
# CORRECTED: Since response_time is in days, output columns should be *_days

agg_overall <- cleaned_data[, .(
  count = .N,
  mean_days = mean(response_time, na.rm = TRUE),      
  median_days = median(response_time, na.rm = TRUE),  
  min_days = min(response_time, na.rm = TRUE),       
  max_days = max(response_time, na.rm = TRUE),        
  sd_days = sd(response_time, na.rm = TRUE)           
)]

# 2. By agency
# CORRECTED: Since response_time is in days, output columns should be *_days

agg_agency <- cleaned_data[, .(
  count = .N,
  mean_days = mean(response_time, na.rm = TRUE),     
  median_days = median(response_time, na.rm = TRUE),  
  min_days = min(response_time, na.rm = TRUE),        
  max_days = max(response_time, na.rm = TRUE),        
  sd_days = sd(response_time, na.rm = TRUE)          
), by = agency]

# 3. By complaint_type
agg_complaint <- cleaned_data[, .(
  count = .N,
  mean_days = mean(response_time, na.rm = TRUE),
  median_days = median(response_time, na.rm = TRUE),
  min_days = min(response_time, na.rm = TRUE),
  max_days = max(response_time, na.rm = TRUE),
  sd_days = sd(response_time, na.rm = TRUE)
), by = complaint_type]

# 4. By borough
agg_borough <- cleaned_data[, .(
  count = .N,
  mean_days = mean(response_time, na.rm = TRUE),
  median_days = median(response_time, na.rm = TRUE),
  min_days = min(response_time, na.rm = TRUE),
  max_days = max(response_time, na.rm = TRUE),
  sd_days = sd(response_time, na.rm = TRUE)
), by = borough]

# 5. By year
agg_year <- cleaned_data[, .(
  count = .N,
  mean_days = mean(response_time, na.rm = TRUE),
  median_days = median(response_time, na.rm = TRUE),
  min_days = min(response_time, na.rm = TRUE),
  max_days = max(response_time, na.rm = TRUE),
  sd_days = sd(response_time, na.rm = TRUE)
), by = year]

# 7. By agency + complaint_type
agg_agency_complaint <- cleaned_data[, .(
  count = .N,
  mean_days = mean(response_time, na.rm = TRUE),
  median_days = median(response_time, na.rm = TRUE),
  min_days = min(response_time, na.rm = TRUE),
  max_days = max(response_time, na.rm = TRUE),
  sd_days = sd(response_time, na.rm = TRUE)
), by = .(agency, complaint_type)]

# 8. By agency + borough
agg_agency_borough <- cleaned_data[, .(
  count = .N,
  mean_days = mean(response_time, na.rm = TRUE),
  median_days = median(response_time, na.rm = TRUE),
  min_days = min(response_time, na.rm = TRUE),
  max_days = max(response_time, na.rm = TRUE),
  sd_days = sd(response_time, na.rm = TRUE)
), by = .(agency, borough)]

# 9. By agency + year
agg_agency_year <- cleaned_data[, .(
  count = .N,
  mean_days = mean(response_time, na.rm = TRUE),
  median_days = median(response_time, na.rm = TRUE),
  min_days = min(response_time, na.rm = TRUE),
  max_days = max(response_time, na.rm = TRUE),
  sd_days = sd(response_time, na.rm = TRUE)
), by = .(agency, year)]

# 11. By complaint_type + borough
agg_complaint_borough <- cleaned_data[, .(
  count = .N,
  mean_days = mean(response_time, na.rm = TRUE),
  median_days = median(response_time, na.rm = TRUE),
  min_days = min(response_time, na.rm = TRUE),
  max_days = max(response_time, na.rm = TRUE),
  sd_days = sd(response_time, na.rm = TRUE)
), by = .(complaint_type, borough)]

# 12. By complaint_type + year
agg_complaint_year <- cleaned_data[, .(
  count = .N,
  mean_days = mean(response_time, na.rm = TRUE),
  median_days = median(response_time, na.rm = TRUE),
  min_days = min(response_time, na.rm = TRUE),
  max_days = max(response_time, na.rm = TRUE),
  sd_days = sd(response_time, na.rm = TRUE)
), by = .(complaint_type, year)]

# 14. By borough + year
agg_borough_year <- cleaned_data[, .(
  count = .N,
  mean_days = mean(response_time, na.rm = TRUE),
  median_days = median(response_time, na.rm = TRUE),
  min_days = min(response_time, na.rm = TRUE),
  max_days = max(response_time, na.rm = TRUE),
  sd_days = sd(response_time, na.rm = TRUE)
), by = .(borough, year)]

# 16. By agency + complaint_type + borough
agg_agency_complaint_borough <- cleaned_data[, .(
  count = .N,
  mean_days = mean(response_time, na.rm = TRUE),
  median_days = median(response_time, na.rm = TRUE),
  min_days = min(response_time, na.rm = TRUE),
  max_days = max(response_time, na.rm = TRUE),
  sd_days = sd(response_time, na.rm = TRUE)
), by = .(agency, complaint_type, borough)]

# 17. By agency + complaint_type + year
agg_agency_complaint_year <- cleaned_data[, .(
  count = .N,
  mean_days = mean(response_time, na.rm = TRUE),
  median_days = median(response_time, na.rm = TRUE),
  min_days = min(response_time, na.rm = TRUE),
  max_days = max(response_time, na.rm = TRUE),
  sd_days = sd(response_time, na.rm = TRUE)
), by = .(agency, complaint_type, year)]

# 19. By agency + borough + year
agg_agency_borough_year <- cleaned_data[, .(
  count = .N,
  mean_days = mean(response_time, na.rm = TRUE),
  median_days = median(response_time, na.rm = TRUE),
  min_days = min(response_time, na.rm = TRUE),
  max_days = max(response_time, na.rm = TRUE),
  sd_days = sd(response_time, na.rm = TRUE)
), by = .(agency, borough, year)]

# 21. By complaint_type + borough + year
agg_complaint_borough_year <- cleaned_data[, .(
  count = .N,
  mean_days = mean(response_time, na.rm = TRUE),
  median_days = median(response_time, na.rm = TRUE),
  min_days = min(response_time, na.rm = TRUE),
  max_days = max(response_time, na.rm = TRUE),
  sd_days = sd(response_time, na.rm = TRUE)
), by = .(complaint_type, borough, year)]

# 23. By agency + complaint_type + borough + year
agg_agency_complaint_borough_year <- cleaned_data[, .(
  count = .N,
  mean_days = mean(response_time, na.rm = TRUE),
  median_days = median(response_time, na.rm = TRUE),
  min_days = min(response_time, na.rm = TRUE),
  max_days = max(response_time, na.rm = TRUE),
  sd_days = sd(response_time, na.rm = TRUE)
), by = .(agency, complaint_type, borough, year)]

# Now add dimension identifiers to all tables to allow easier combination
agg_overall[, `:=`(
  dim_agency = "All",
  dim_complaint_type = "All",
  dim_borough = "All",
  dim_year = "All"
)]

agg_agency[, `:=`(
  dim_complaint_type = "All",
  dim_borough = "All",
  dim_year = "All"
)]
setnames(agg_agency, "agency", "dim_agency")

agg_complaint[, `:=`(
  dim_agency = "All",
  dim_borough = "All",
  dim_year = "All"
)]
setnames(agg_complaint, "complaint_type", "dim_complaint_type")

agg_borough[, `:=`(
  dim_agency = "All",
  dim_complaint_type = "All",
  dim_year = "All"
)]
setnames(agg_borough, "borough", "dim_borough")

agg_year[, `:=`(
  dim_agency = "All",
  dim_complaint_type = "All",
  dim_borough = "All"
)]
setnames(agg_year, "year", "dim_year")

agg_agency_complaint[, `:=`(
  dim_borough = "All",
  dim_year = "All"
)]
setnames(agg_agency_complaint, c("agency", "complaint_type"), 
                                          c("dim_agency", "dim_complaint_type"))

agg_agency_borough[, `:=`(
  dim_complaint_type = "All",
  dim_year = "All"
)]
setnames(agg_agency_borough, c("agency", "borough"), 
                                          c("dim_agency", "dim_borough"))
agg_agency_year[, `:=`(
  dim_complaint_type = "All",
  dim_borough = "All"
)]
setnames(agg_agency_year, c("agency", "year"), c("dim_agency", "dim_year"))

agg_complaint_borough[, `:=`(
  dim_agency = "All",
  dim_year = "All"
)]
setnames(agg_complaint_borough, c("complaint_type", "borough"), 
                                        c("dim_complaint_type", "dim_borough"))

agg_complaint_year[, `:=`(
  dim_agency = "All",
  dim_borough = "All"
)]
setnames(agg_complaint_year, c("complaint_type", "year"), 
                                          c("dim_complaint_type", "dim_year"))
agg_borough_year[, `:=`(
  dim_agency = "All",
  dim_complaint_type = "All"
)]
setnames(agg_borough_year, c("borough", "year"), c("dim_borough", "dim_year"))

agg_agency_complaint_borough[, `:=`(
  dim_year = "All"
)]
setnames(agg_agency_complaint_borough, 
         c("agency", "complaint_type", "borough"), 
         c("dim_agency", "dim_complaint_type", "dim_borough"))

agg_agency_complaint_year[, `:=`(
  dim_borough = "All"
)]
setnames(agg_agency_complaint_year, 
         c("agency", "complaint_type", "year"), 
         c("dim_agency", "dim_complaint_type", "dim_year"))

agg_agency_borough_year[, dim_complaint_type := "All"]
setnames(agg_agency_borough_year, 
         c("agency", "borough", "year"), 
         c("dim_agency", "dim_borough", "dim_year"))

agg_complaint_borough_year[, dim_agency := "All"]
setnames(agg_complaint_borough_year, 
         c("complaint_type", "borough", "year"), 
         c("dim_complaint_type", "dim_borough", "dim_year"))

#agg_agency_complaint_borough_year[, dim_year := "All"]
setnames(agg_agency_complaint_borough_year, 
         c("agency", "complaint_type", "borough", "year"), 
         c("dim_agency", "dim_complaint_type", "dim_borough", "dim_year"))

################################################################################
# Create special aggregate categories based on prefix matching
create_prefix_aggregate <- function(base_data, prefix, category_name, 
                                    dimensions) {
  
  # Get all the complaint types that start with the prefix
  matching_complaints <- grep(paste0("^", prefix), 
                              unique(base_data$dim_complaint_type), 
                              value = TRUE, ignore.case = TRUE)
  
  # Skip "All" which isn't a real complaint type
  matching_complaints <- matching_complaints[matching_complaints != "All"]
  
  if (length(matching_complaints) == 0) {
    warning("No complaint types found for prefix: ", prefix)
    return(NULL)
  }
  
  # Find the rows with these complaint types
  results <- list()
  
  # For each required dimension combination, create an aggregation
  for (dim_combo in dimensions) {
    # Filter base data to relevant complaints and dimensions
    filtered_data <- base_data[dim_complaint_type %in% matching_complaints]
    
    # Set all dimensions not in our combo to "All"
    all_dims <- c("dim_agency", "dim_complaint_type", "dim_borough", "dim_year")
    group_dims <- all_dims[all_dims %in% dim_combo]
    
    # Group and aggregate using existing columns from agg_master
    agg_result <- filtered_data[, .(
      count = sum(count),                                        # Sum the counts
      mean_days = weighted.mean(mean_days, count, na.rm = TRUE),     # Weighted mean of existing mean_days
      median_days = weighted.mean(median_days, count, na.rm = TRUE), # Weighted mean of existing median_days  
      min_days = min(min_days, na.rm = TRUE),                        # Min of existing min_days
      max_days = max(max_days, na.rm = TRUE),                        # Max of existing max_days
      sd_days = sqrt(weighted.mean(sd_days^2, count, na.rm = TRUE))  # Weighted SD of existing sd_days
    ), by = group_dims]
    
    # Add "All" for dimensions not in our grouping
    for (dim in all_dims) {
      if (!dim %in% group_dims) {
        agg_result[, (dim) := "All"]
      }
    }
    
    # Set the complaint type to our special category name
    agg_result[, dim_complaint_type := category_name]
    
    # Always set agency to "All" for these special categories
    agg_result[, dim_agency := "All"]
    
    # Generate the aggregation_type name based on which dimensions are not "All"
    agg_type <- paste0(
      "special_",
      ifelse("dim_borough" %in% group_dims, "borough", ""),
      ifelse("dim_borough" %in% group_dims && "dim_year" %in% group_dims, "_", ""),
      ifelse("dim_year" %in% group_dims, "year", ""),
      ifelse(!any(c("dim_borough", "dim_year") %in% group_dims), "overall", "")
    )
    
    agg_result[, aggregation_type := agg_type]
    
    results[[length(results) + 1]] <- agg_result
  }
  
  if (length(results) > 0) {
    return(rbindlist(results, use.names = TRUE, fill = TRUE))
  } else {
    return(NULL)
  }
}

# Define the dimensions we want for our special categories
dimensions_list <- list(
  # Overall - no dimensions
  character(0),
  
  # Single dimensions
  "dim_borough",
  "dim_year",
  
  # Two dimensions
  c("dim_borough", "dim_year")
)

# Combine all aggregations into a single master table
agg_master <- rbindlist(list(
  agg_overall,
  agg_agency,
  agg_complaint,
  agg_borough,
  agg_year,
  agg_agency_complaint,
  agg_agency_borough,
  agg_agency_year,
  agg_complaint_borough,
  agg_complaint_year,
  agg_borough_year,
  agg_agency_complaint_borough,
  agg_agency_complaint_year,
  agg_agency_borough_year,
  agg_complaint_borough_year,
  agg_agency_complaint_borough_year  # Remove the comma here
), use.names = TRUE, fill = TRUE)

# Add aggregation type metadata for easier filtering
agg_master[, aggregation_type := case_when(
  # Agency + Complaint + Borough + Year
  dim_agency != "All" & dim_complaint_type != "All" & 
    dim_borough != "All" & dim_year != "All" ~ "agency_complaint_borough_year",
  
  # Agency + Complaint + Borough
  dim_agency != "All" & dim_complaint_type != "All" & 
    dim_borough != "All" & dim_year == "All" ~ "agency_complaint_borough",
  
  # Agency + Complaint + Year
  dim_agency != "All" & dim_complaint_type != "All" & 
    dim_borough == "All" & dim_year != "All" ~ "agency_complaint_year",
  
  # Agency + Complaint
  dim_agency != "All" & dim_complaint_type != "All" & 
    dim_borough == "All" & dim_year == "All" ~ "agency_complaint",
  
  # Agency + Borough + Year
  dim_agency != "All" & dim_complaint_type == "All" & 
    dim_borough != "All" & dim_year != "All" ~ "agency_borough_year",
  
  # Agency + Borough
  dim_agency != "All" & dim_complaint_type == "All" & 
    dim_borough != "All" & dim_year == "All" ~ "agency_borough",
  
  # Agency + Year
  dim_agency != "All" & dim_complaint_type == "All" & 
    dim_borough == "All" & dim_year != "All" ~ "agency_year",
  
  # Agency only
  dim_agency != "All" & dim_complaint_type == "All" & 
    dim_borough == "All" & dim_year == "All" ~ "agency",
  
  # Complaint + Borough + Year
  dim_agency == "All" & dim_complaint_type != "All" & 
    dim_borough != "All" & dim_year != "All" ~ "complaint_borough_year",
  
  # Complaint + Borough
  dim_agency == "All" & dim_complaint_type != "All" & 
    dim_borough != "All" & dim_year == "All" ~ "complaint_borough",
  
  # Complaint + Year
  dim_agency == "All" & dim_complaint_type != "All" & 
    dim_borough == "All" & dim_year != "All" ~ "complaint_year",
  
  # Complaint only
  dim_agency == "All" & dim_complaint_type != "All" & 
    dim_borough == "All" & dim_year == "All" ~ "complaint",
  
  # Borough + Year
  dim_agency == "All" & dim_complaint_type == "All" & 
    dim_borough != "All" & dim_year != "All" ~ "borough_year",
  
  # Borough only
  dim_agency == "All" & dim_complaint_type == "All" & 
    dim_borough != "All" & dim_year == "All" ~ "borough",
  
  # Year only
  dim_agency == "All" & dim_complaint_type == "All" & 
    dim_borough == "All" & dim_year != "All" ~ "year",
  
  # Overall
  dim_agency == "All" & dim_complaint_type == "All" & 
    dim_borough == "All" & dim_year == "All" ~ "overall",
  
  # Default case
  TRUE ~ "unknown"
)]

# Create the special categories
noise_aggs <- create_prefix_aggregate(
  base_data = agg_master[dim_complaint_type != "All" & 
                           dim_agency == "All"],  # Use base complaint data
  prefix = "NOISE",
  category_name = "NOISE (All Types)",
  dimensions = dimensions_list
)

street_aggs <- create_prefix_aggregate(
  base_data = agg_master[dim_complaint_type != "All" & 
                           dim_agency == "All"],
  prefix = "STREET",
  category_name = "STREET (All Issues)",
  dimensions = dimensions_list
)

homeless_aggs <- create_prefix_aggregate(
  base_data = agg_master[dim_complaint_type != "All" & 
                           dim_agency == "All"],
  prefix = "HOMELESS",
  category_name = "HOMELESS (All Issues)",
  dimensions = dimensions_list
)

# Combine all special aggregates
special_aggs <- rbindlist(
  list(noise_aggs, street_aggs, homeless_aggs),
  use.names = TRUE, fill = TRUE
)

# Add days/weeks columns for the special aggregates already added to agg_master
if (!is.null(special_aggs) && nrow(special_aggs) > 0) {
  # Append special aggregates to the master table
  agg_master <- rbindlist(
    list(agg_master, special_aggs),
    use.names = TRUE, fill = TRUE
  )
}

# Save the master aggregation table to the specified directory
saveRDS(agg_master, file.path(shiny_dir, "nyc_service_requests_aggregated.rds"))
cat("\nApproximate size of the master aggregation table:", 
                            format(object.size(agg_master), units = "MB"), 
                            "\n\n")

# Look at the first few rows to verify
# Round within data.table, then convert
rounded_data <- head(agg_master)[, lapply(.SD, function(x) {
  if(is.numeric(x)) round(x, 0) else x
})]
print(as.data.frame(rounded_data), right = FALSE)

# Define the list of boroughs you want to include
selected_boroughs <- c("BRONX", "MANHATTAN", "STATEN ISLAND", 
                                              "QUEENS", "BROOKLYN")

# Query the pre-computed aggregation table for the selected boroughs by year
borough_time_series <- agg_master[
  dim_agency == "All" &
    dim_complaint_type == "All" &
    dim_borough %in% selected_boroughs &  # Filter for the selected boroughs
    dim_year != "All" 
]

# Query for "All Boroughs" summary data (overall across all boroughs by year)
all_boroughs_time_series <- agg_master[
  dim_agency == "All" &
    dim_complaint_type == "All" &
    dim_borough == "All" &  # This gives us the overall summary
    dim_year != "All" 
]

################################################################################
# Add a borough identifier for the all boroughs data
all_boroughs_time_series[, dim_borough := "All Boroughs"]

# Combine the individual borough data with the all boroughs summary
combined_time_series <- rbind(borough_time_series, all_boroughs_time_series)

# Convert year to numeric for proper ordering
combined_time_series[, year_numeric := as.numeric(dim_year)]

# Sort by borough and year
setorder(combined_time_series, dim_borough, year_numeric)

# Create a color palette with dark color for "All Boroughs"
n_boroughs <- length(selected_boroughs)

borough_colors <- c(
  viridis::viridis(n_boroughs, option = "C", end = 0.9),  # Most colorblind-safe
  "#000000"  # Black for "All Boroughs"
)
names(borough_colors) <- c(selected_boroughs, "All Boroughs")

# Create a chart with "All Boroughs" as a dashed line for even more distinction
line_chart_dashed <- ggplot(combined_time_series, 
                            aes(x = year_numeric, 
                                y = median_days, 
                                color = dim_borough, 
                                group = dim_borough)) +
  geom_line(aes(linewidth = ifelse(dim_borough == "All Boroughs", 1.5, 1),
                linetype = ifelse(dim_borough == "All Boroughs", "dashed", 
                                  "solid"))) +
  geom_point(aes(size = ifelse(dim_borough == "All Boroughs", 4, 3))) +
  
  # Add data labels
  geom_text(aes(label = round(median_days, 1)), 
            vjust = -0.8, hjust = 0.5, size = 3.5) +
  labs(
    title = "Median Response Time by Boroughs By Year",
    subtitle = "Black dashed line shows overall average across all boroughs",
    x = "",
    y = "Median Response Time (days)",
    color = "Borough"
  ) +
  scale_color_manual(values = borough_colors) +
  scale_linewidth_identity() +
  scale_size_identity() +
  scale_linetype_identity() +
  scale_x_continuous(breaks = unique(combined_time_series$year_numeric)) +
  theme_minimal() +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.90, 0.77),
    legend.background = element_rect(fill = "grey99", color = "black", 
                                     linewidth = 0.6),
    legend.margin = margin(12, 12, 12, 12),  # Increase from 6 to 12 or higher
    legend.box.spacing = unit(0.5, "cm"),     # Space around the legend box
    legend.text = element_text(margin = margin(t = 2, b = 2)), 
    # Vertical spacing between legend items
    legend.key.height = unit(1.2, "lines"),  # Height of each legend key
    legend.key.width = unit(1.5, "lines"),   # Width of each legend key
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold"),  # Bold x-axis labels
    axis.text.y = element_text(face = "bold"),  # Bold y-axis labels
    panel.background = element_rect(fill = "grey93"),  # Light grey background
    panel.grid.major = element_line(color = "white", linewidth = 0.75),  
    # White grid lines
    panel.grid.minor = element_blank(),
    plot.subtitle = element_text(size = 10)
  ) +
  guides(
  color = guide_legend(
    override.aes = list(
      linewidth = 1, 
      size = 3, 
      linetype = "solid",
      shape = 16  # This removes the point symbol
    )
  ),
  linewidth = "none",
  size = "none",
  linetype = "none"
)

# Display the alternative chart
print(line_chart_dashed)
Sys.sleep(2)

# Save the alternative chart
ggsave(
  file.path(chart_dir, 
            "median_response_time_boroughs_with_overall_dashed_yearly.pdf"),
  line_chart_dashed, 
  width = 13, 
  height = 8.5, 
  dpi = 300)

################################################################################
# Create individual charts for each borough for clearer view
# Filter out "All Boroughs" for the individual charts since we want separate panels
individual_borough_data <- combined_time_series[dim_borough != "All Boroughs"]

# Create enhanced individual charts that include "All Boroughs" as a reference line
# This shows each borough with the overall average as a gray reference line

# Calculate max value before the ggplot
max_median_value <- max(combined_time_series$median_days, na.rm = TRUE)
y_axis_max <- max_median_value * 1.05

individual_charts_with_all <- ggplot() +
  # First, add the "All Boroughs" line as a reference (gray, behind everything)
  geom_line(data = combined_time_series[dim_borough == "All Boroughs"],
            aes(x = year_numeric, y = median_days, group = 1),
            color = "gray45", linewidth = 1.2, linetype = "dashed") +
  geom_point(data = combined_time_series[dim_borough == "All Boroughs"],
             aes(x = year_numeric, y = median_days),
             color = "gray45", size = 2.5) +
  
  # Then add the individual borough data (colored, on top)
  geom_line(data = individual_borough_data,
            aes(x = year_numeric, y = median_days, group = 1),
            color = "steelblue", linewidth = 1.2) +
  geom_point(data = individual_borough_data,
             aes(x = year_numeric, y = median_days),
             color = "steelblue", size = 3) +
  
  geom_text(data = individual_borough_data,
            aes(x = year_numeric, y = median_days, 
                label = round(median_days, 1)), 
            vjust = -0.8, hjust = 0.5, size = 3.5, color = "steelblue") +
  
  facet_wrap(~ dim_borough, scales = "free_y") +  # Keep this as is
  labs( 
    title = "Median Response Time by Borough with Citywide Average",
    x = "",
    y = "Median Response Time (days)"
  ) +
  scale_x_continuous(breaks = unique(individual_borough_data$year_numeric)) +
  scale_y_continuous(limits = c(0, y_axis_max)) +  # This will override the free_y scaling
  theme_minimal() +
  theme(
    strip.background = element_rect(fill = "lightsteelblue"),
    strip.text = element_text(face = "bold"),
    plot.subtitle = element_text(size = 10),
    axis.text = element_text(face = "bold"),        # Bold for axis tick labels
    axis.title = element_text(face = "bold")       # Bold for axis titles
  )

# Display the enhanced individual charts
print(individual_charts_with_all)
Sys.sleep(2)

# Save the enhanced individual charts
ggsave(
  file.path(chart_dir, 
            "median_response_time_boroughs_with_citywide_reference.pdf"), 
  individual_charts_with_all, 
  width = 13, 
  height = 8.5, 
  dpi = 300)

################################################################################
# Calculate change for all data including "All Boroughs"
# CORRECTED: Change median_hours to median_days since the values are in days

overall_change_with_all <- combined_time_series[, .(
  first_year = year_numeric[1],
  last_year = year_numeric[.N],
  first_value = round(median_days[1], 1),           # CHANGED: median_hours -> median_days
  last_value = round(median_days[.N], 1),           # CHANGED: median_hours -> median_days
  absolute_change = round(median_days[.N] - median_days[1], 1),        # CHANGED: median_hours -> median_days
  percent_change = round((median_days[.N] - median_days[1]) /          # CHANGED: median_hours -> median_days
                           median_days[1] * 100, 1)                       # CHANGED: median_hours -> median_days
), by = dim_borough][order(percent_change)]

cat("\nOverall change in median response time (days):\n")
print(as.data.frame(overall_change_with_all), right = FALSE)

# Create color mapping - highlight "All Boroughs" in black
change_colors <- ifelse(overall_change_with_all$dim_borough == "All Boroughs", 
                        "grey55", "steelblue3")
names(change_colors) <- overall_change_with_all$dim_borough

# Enhanced bar chart showing overall change including "All Boroughs"
overall_chart_with_all <- ggplot(overall_change_with_all, 
                                 aes(x = reorder(dim_borough, percent_change), 
                                     y = percent_change)) +
  geom_col(aes(fill = percent_change > 0), alpha = 0.8) +  
  # Dynamic fill based on positive/negative
  scale_fill_manual(values = c("FALSE" = "steelblue3", "TRUE" = "goldenrod1")) +  
  # Color mapping
  geom_text(aes(label = sprintf("%.0f%%", percent_change)), 
            hjust = 0.5,  # Center the text
            vjust = ifelse(overall_change_with_all$percent_change >= 
                                                                  0, 1.2, -0.2),  
            # Position outside bars
            color = "black", fontface = "bold", size = 3.5) +
  labs(
    title = paste0("Overall Change in Median Response Time (days) (", 
                   overall_change_with_all$first_year[1], "-", 
                   overall_change_with_all$last_year[1], ")"),
    x = "",  # Remove y-axis label
    y = "Percent Change (%)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "grey93"),
    panel.grid.major = element_line(color = "white", linewidth = 0.75),  
    # White grid lines
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(hjust = 0, face = "bold"),  
    # Bold y-axis labels (borough names)
    axis.text.x = element_text(face = "bold"),  
    # Bold x-axis labels (percentages)
    axis.title.x = element_text(face = "bold"),  
    # Bold x-axis title
    plot.title = element_text(face = "bold"),  # Bold title
    plot.margin = margin(t = 20, r = 30, b = 20, l = 10, unit = "pt")
  ) +
  coord_flip(clip = "off") +  # Allow labels to extend beyond plot area
  scale_y_continuous(expand = expansion(mult = c(0.25, 0.25)))  
  # More padding for labels

print(overall_chart_with_all)
Sys.sleep(2)

# Save the enhanced overall change chart
ggsave(
  file.path(chart_dir, "overall_change_with_citywide_average.pdf"), 
  overall_chart_with_all, 
  width = 13, 
  height = 8.5, 
  dpi = 300)

################################################################################
# Create a summary table showing how each borough compares to the citywide average
citywide_change <- overall_change_with_all[dim_borough == 
                                                  "All Boroughs"]$percent_change

comparison_summary <- overall_change_with_all[dim_borough 
                                                        != "All Boroughs"][, .(
  Borough = dim_borough,
  Borough_Change = percent_change,
  Citywide_Change = citywide_change,
  Difference_vs_Citywide = round(percent_change - citywide_change, 1),
  Performance = ifelse(percent_change < citywide_change, 
                                    "Better than average", "Worse than average")
)][order(Borough_Change)]

cat("\nBorough performance vs. citywide average:\n")
print(as.data.frame(comparison_summary), right = FALSE)

################################################################################
# Create a dot plot showing borough vs citywide comparison
comparison_chart <- ggplot(comparison_summary, aes(x = reorder(Borough, 
                                                             Borough_Change))) +
  geom_point(aes(y = Borough_Change, color = "Borough"), size = 4) +
  geom_point(aes(y = Citywide_Change, color = "Citywide Average"), 
                                                        size = 4, shape = 17) +
  geom_segment(aes(y = Borough_Change, yend = Citywide_Change, xend = Borough), 
               color = "gray40", linetype = "dotted", linewidth = 1.25) +
  scale_color_manual(values = c("Borough" = "steelblue", 
                                                "Citywide Average" = "black")) +
  labs(
    title = "Borough Performance vs. Citywide Average",
    subtitle = 
      "Dots = Individual borough change | Triangles = Citywide average change",
    x = "",
    y = "Percent Change (%)",
    color = "Metric"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "grey93"),
    panel.grid.major = element_line(color = "white", linewidth = 0.75),  
    # White grid lines
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(face = "bold"),  # Bold x-axis labels
    axis.text.y = element_text(face = "bold")   # Bold y-axis labels
        ) + 
  coord_flip()

print(comparison_chart)
Sys.sleep(2)

# Save the comparison chart
ggsave(
  file.path(chart_dir, "borough_vs_citywide_comparison.pdf"), 
  comparison_chart, 
  width = 13, 
  height = 8.5, 
  dpi = 300)

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

#sink()

cat("\n\n*****END OF PROGRAM*****\n")
cat("\n📅 Execution ends at:", formatted_end_time, "\n")
cat("\n⏱️ Program run-time:", duration_string, "\n")

################################################################################
################################################################################

