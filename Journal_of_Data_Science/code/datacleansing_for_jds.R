######################################################################### 
######################################################################### 

main_data_file <-  
  "5-year_311SR_01-01-2020_thru_12-31-2024_AS_OF_10-10-2025.rds"
 
# Boolean flag. TRUE to redirect console output to text file
# FALSE to display console output on the screen
enable_sink <- FALSE    

#The "as of" date in "YYYY-MM-DD" format
projection_date <- "2025-10-20"   

#Number of SRs for the year through the projection_date  
projection_SR_count <- 2779203  

# Okabe-Ito palette for colorblind safe 
palette(c("#E69F00", "#56B4E9", "#009E73", "#F0E442", 
          "#0072B2", "#D55E00", "#CC79A7", "#999999"))

#########################################################################
# ------------------------------------------------------ -------
# 📦 INSTALL AND LOAD REQUIRED PACKAGES
# -------------------------------------------------------------
load_required_packages <- function(verbose = TRUE) {
  # Ordered to avoid common masking issues
  required_packages <- c(
    "data.table",      # Load first - commonly masked functions
    "arrow",
    "fasttime",
    "lubridate",
    "here",
    "zoo",
    "ggplot2",         # Core plotting (scales is a dependency, auto-loaded)
    "ggpmisc",
    "ggpattern",
    "ggrastr",
    "qcc",
    "qicharts2",
    "grid",
    "gridExtra",
    "sf",              # Spatial - can mask some dplyr functions
    "stringr",         # Load before dplyr/tidyverse
    "stringdist",
    "dplyr",           # Load before tidyverse
    "tidyverse",       # Load late - masks many functions
    "scales",          # Load AFTER tidyverse to mask purrr::discard and readr::col_factor
    "bslib",
    "shiny",
    "DT",              # Load after dplyr to avoid confusion
    "gt",
    "styler",
    "rlang",
    "renv",
    "remotes"
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

# 
# # 2. Install ggrastr directly from its GitHub repo
# remotes::install_github("VPetukhov/ggrastr")
# 

# -------------------------------------------------------------
# 📁 Set Working Directory for the Project.
# -------------------------------------------------------------
# Set working directory to the location of the initialization script
# setup_paths.R
# Automatically detects if running in project or standalone

# # Check if we're in an RStudio project
# if (requireNamespace("rstudioapi", quietly = TRUE) && 
#     rstudioapi::isAvailable() && 
#     !is.null(rstudioapi::getActiveProject())) {
#   
#   # PROJECT MODE: Use relative paths from project root
#   base_dir <- here::here()  # or getwd() if you don't have 'here' package
#   cat("Running in PROJECT mode\n")
#   cat("Base directory:", base_dir, "\n")
#   
# } else {
  
  # STANDALONE MODE: Use your manual absolute path
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
  cat("Running in STANDALONE mode\n")
  cat("Base directory:", base_dir, "\n")
#}

# Define all paths relative to base_dir (works in both modes)
data_dir      <- file.path(base_dir, "data")
code_dir      <- file.path(base_dir, "code")
chart_dir     <- file.path(base_dir, "charts")
functions_dir <- file.path(base_dir, "code", "functions")
console_dir   <- file.path(base_dir, "console_output")
write_dir     <- file.path(base_dir, "data")


# Create directories if they don't exist
dirs_to_create <- c(data_dir, chart_dir, console_dir)
for (dir in dirs_to_create) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
    cat("Created directory:", dir, "\n")
  }
}

cat("\nDirectory paths set:\n")
cat("  Data:", data_dir, "\n")
cat("  Code:", code_dir, "\n")
cat("  Charts:", chart_dir, "\n")
cat("  Functions:", functions_dir, "\n")
cat("  Console output:", console_dir, "\n")
cat("  Write:", write_dir, "\n")
# Get all .R files in the "functions" sub-directory

function_files <- list.files(functions_dir, pattern = "\\.R$", 
                             full.names = TRUE)

# More robust sourcing with verification
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

#########################################################################
options(scipen = 999) # Set scipen option to a large value.
options(digits = 15) # Set the number of decimal places to 15, the max observed.
options(datatable.print.class = FALSE)

# Extract the date after "AS_OF_"
extracted_date <- sub(".*AS_OF_([0-9-]+).*", "\\1", main_data_file)
as_of_date <- as.POSIXct(
  paste0(extracted_date, " 00:00:00"),
  format = "%m-%d-%Y %H:%M:%S",
  tz = "America/New_York"
)

# Earliest possible genesis date as POSIXct with America/New_York timezone
genesis_date <- as.POSIXct("2003-01-01 00:00:00", tz = "America/New_York")

# Convert to POSIXct format
max_closed_date <- as.POSIXct(extracted_date, format = "%m-%d-%Y", tz = "America/New_York")
#print(paste("Parsed date:", max_closed_date))

# Add time to end of day
max_closed_date <- max_closed_date + (23*3600 + 59*60 + 59)
#print(paste("Final datetime:", max_closed_date))

#########################################################################
programStart <- as.POSIXct(Sys.time())
formattedStartTime <- format(programStart, "%Y-%m-%d %H:%M:%S")
cat("\n***** Program initialization *****")

message("\nExecution begins at:", formattedStartTime)

# Define the console output directory and file name.
console_output_file <- file.path(console_dir, "JDS_datacleaning_console_output.txt")

if (isTRUE(enable_sink)) {
  sink(console_output_file)
}

if (sink.number(type = "output") > 0L) {
  cat("\nExecution begins at:", formattedStartTime)
}

#########################################################################
# Load the USPS zipcode file
message("\nReading the USPS zipcode file.")

USPS_zipcode_file_path <- file.path(data_dir, "USPS_zipcodes.rds")

USPSzipcodes <- readRDS(USPS_zipcode_file_path)
if (!is.data.table(USPSzipcodes)) setDT(USPSzipcodes)  # converts in place

#########################################################################
# Load the main 311 SR data file. Set the read & write paths.
message("\nReading the main 311 SR data file.")

main_data_file_path <- file.path( data_dir, main_data_file)

d311 <- readRDS(main_data_file_path)
if (!is.data.table(d311)) setDT(d311)  # converts in place

num_rows_d311 <- nrow(d311)
num_columns_d311 <- ncol(d311)

# Guard: ensure column exists
stopifnot("unique_key" %in% names(d311))
setindex(d311, unique_key)   # no reorder; speeds joins/subsets on unique_key

#copy_raw_data <- d311

table(lubridate::year(d311$closed_date)) 

 #########################################################################
# Check for unique keys and index
message("\nCreating unique_key index.")

# Basic stats
n_na_keys   <- sum(is.na(d311$unique_key))
n_unique    <- uniqueN(d311$unique_key)
all_unique  <- (n_unique == num_rows_d311) && (n_na_keys == 0L)

#cat("\nAre all 'unique_key' values truly unique (no NA)? ", all_unique, "\n", sep = "")

if (all_unique) {
  # Fast lookups without reordering
  setindex(d311, unique_key)
  # Optional belt-and-suspenders assertion
  stopifnot(anyDuplicated(d311$unique_key) == 0L)
#  cat("Index set on 'unique_key' (secondary index; row order preserved).\n")
  } else {
  # Diagnose duplicates and/or NAs
  if (n_na_keys > 0L) {
    cat("Found ", n_na_keys, " NA unique_key value(s).\n", sep = "")
  }
    
  # Keys that occur more than once
  dup_keys <- d311[, .N, by = unique_key][!is.na(unique_key) & N > 1L][order(-N)]
  
  if (nrow(dup_keys)) {
    # Total duplicate rows = sum(N-1) over dup keys
    total_dup_rows <- dup_keys[, sum(N - 1L)]
    cat("Found ", total_dup_rows, " duplicate row(s) across ",
        nrow(dup_keys), " unique_key value(s).\n", sep = "")
    cat("\nTop duplicate keys (count per key):\n")
    print(dup_keys[1:min(10L, .N)], nrows = min(10L, nrow(dup_keys)))
    # Optional: peek at one offending key’s rows
    eg_key <- dup_keys$unique_key[1L]
    cat("\nExample rows for unique_key = ", eg_key, ":\n", sep = "")
    print(d311[unique_key == eg_key][1:min(.N, 5L)])
  }
  cat("\nResolve NA/duplicate keys before indexing.\n")
  # Do NOT setindex here to avoid masking an issue.
  }

#########################################################################

cat("\n\n**********DATA SUMMARY**********")

options(warn = 2)  

########################################################################
# assume d311 is a data.table and created_date is POSIXct
tz_out  <-  "America/New_York"                    
fmt_ts  <- "%Y-%m-%d %H:%M:%S"
fmt_day <- "%Y-%m-%d"               

# Compute once
rng <- d311[, range(created_date, na.rm = TRUE)]  # POSIXct min,max
if (any(is.infinite(rng))) stop("created_date has no non-missing values")

earliest_date <- rng[1L]
latest_date   <- rng[2L]

# Timestamp strings
earliest_date_formatted <- format(earliest_date, fmt_ts, tz = tz_out)
latest_date_formatted   <- format(latest_date,   fmt_ts, tz = tz_out)

# Date-only strings for titles
earliest_title <- format(earliest_date, fmt_day, tz = tz_out)
latest_title   <- format(latest_date,   fmt_day, tz = tz_out)

#########################################################################
# Plot yearly growth of 311 SRs.
message("\nCreating year plot and statistics.")
yearly_bar_chart <- plot_annual_counts_with_projection(
  DT = d311,
  created_col = "created_date",
  estimate_flag = TRUE,
  estimate_date = projection_date,
  estimate_value = projection_SR_count,
  chart_dir = chart_dir,
  filename = "annual_trend_with_projection_bar_chart.pdf",
  title = "NYC 311 Service Requests by Year",
  include_projection_in_growth = TRUE,
  subtitle = "w/linear trendline & 2025 projection"
)

#########################################################################
# fraction in 0–1 with 4 decimals
sorted_by_agency <- d311[, .(count = .N), by = agency][order(-count)]
sorted_by_agency[, percentage := round(count / sum(count), 4)]
sorted_by_agency[, cumulative_percentage := cumsum(percentage)]

# At the top of your script
options(warn = 2)  # Turn warnings into errors

plot_pareto_combo(
  DT               = d311,
  x_col            = agency,
  title            = "Pareto Analysis by Agency",
  filename         = "SR_by_agency_pareto_combo_chart.pdf",
  chart_dir        = chart_dir,
  width_in         = 13,
  height_in        = 8.5,
  show_labels      = FALSE,
  show_threshold_80 = TRUE,   # whether to draw the 80% reference line
  annotation_size  = 3.5
)

# Display the results
cat("\nRows in the 311 SR dataset:", format(num_rows_d311, big.mark = ","))
cat("\nColumns in the 311 SR dataset:", format(num_columns_d311, big.mark = ","))
cat("\nAgencies represented:", length(unique(d311$agency)))
cat("\n\nData contains SRs from", earliest_date_formatted, 
    "through", latest_date_formatted)

########################################################################

cat("\n\n********** Missing entires by column **********")

#########################################################################
# # Identify date columns (optional; not needed for counting, but keeping for reference)
date_cols <- c("created_date", "closed_date", "due_date", "resolution_action_updated_date")
message("\nDetermining missing values.")

# Per-column NA counts (no giant logical table)
na_counts <- vapply(d311, function(x) sum(is.na(x)), integer(1L))
# Assemble and compute percentages
missingDataPerColumn <- data.table(
  field    = names(na_counts),
  NA_count = unname(na_counts)
)
missingDataPerColumn[, pct_na := round(100 * NA_count / num_rows_d311, 1)]
setorder(missingDataPerColumn, -NA_count)

# Format for display
display_table <- copy(missingDataPerColumn)
display_df <- data.frame(
  field = format(display_table$field, justify = "left"),
  NA_count = format(display_table$NA_count, big.mark = ",", justify = "right"),
  pct_na = format(display_table$pct_na, justify = "right")
)

cat("\nNumber and % NA entries per column:\n")
print(display_df, row.names = FALSE)

# Calculate overall dataset sparsity
total_cells <- as.numeric(num_rows_d311) * ncol(d311)
total_missing <- sum(na_counts)
overall_sparsity_pct <- round(100 * total_missing / total_cells, 2)

cat(sprintf("\nOverall dataset sparsity: %s%% (%s missing values out of %s total cells)\n",
            overall_sparsity_pct,
            format(total_missing, big.mark = ","),
            format(total_cells, big.mark = ",")))

# Fix factor order for plotting
missingDataPerColumn[, field := factor(field, levels = field)]

# First, add a pct_complete column to your data
missingDataPerColumn[, pct_complete := 100 - pct_na]

# Then create the chart with color grouping
create_basic_bar_chart(
  DT          = missingDataPerColumn,
  x_col       = "field",
  y_col       = "pct_complete",  # Changed from pct_na
  title       = "Data Completeness by Field",
  # Color grouping parameters
  use_color_groups = TRUE,
  group_thresholds = c(94, 56),  # Adjust these based on your quality standards
  group_colors     = c("#009E73", "#F0E442", "#D55E00"),  # green, yellow, orange-red
  group_labels     = c("Excellent (≥94%)", "Fair (56-94%)", "Poor (<56%)"),
  legend_position  = "top",
  # Labels
  show_labels = FALSE,
  label_col   = "pct_complete",  # Or keep as "pct_na" if yo u prefer
  label_angle = -70,
  label_size  = 3.5,
  vjust       = 0.5,
  # Theme
  text_size    = 9,
  x_axis_angle = 50,
  x_axis_face  = "bold",
  grid_line_width = 0.5,
  remove_x_title = TRUE,
  remove_y_title = TRUE,
  plot_margin    = c(1, 2, 1, 2),
  # Save
  chart_dir   = chart_dir,
  filename    = "data_completeness_by_field_bar_chart.pdf"
)

#########################################################################
# Determine field usage by Agency. Produce Excel spreadsheet.
# Initialize the list of fields (excluding "agency")
message("\nComputing field usage by Agency.")

# Columns to summarize (exclude the grouping column)
fields_to_summarize <- setdiff(names(d311), "agency")

# Treat NA and "" as missing for character/factor; NA for everything else
has_usable_value <- function(x) {
  ok <- !is.na(x)
  if (is.character(x)) {
    ok <- ok & nzchar(x)
  } else if (is.factor(x)) {
    blank_lvl <- match("", levels(x), nomatch = 0L)
    if (blank_lvl != 0L) ok <- ok & (as.integer(x) != blank_lvl)
  }
  ok
}

# Long format counts: for each field, count usable values by agency
long_counts_by_agency_dt <- rbindlist(
  lapply(fields_to_summarize, function(field_name) {
    usable_row <- has_usable_value(d311[[field_name]])
    d311[usable_row & !is.na(agency), .(count = .N), by = agency][
      , field := field_name][]
  }),
  use.names = TRUE
)

# Wide summary: one row per field, one column per agency (counts)
field_usage_summary_dt <- dcast(
  long_counts_by_agency_dt,
  field ~ agency,
  value.var = "count",
  fill = 0L
)

# Row totals and ordering
agency_cols <- setdiff(names(field_usage_summary_dt), "field")
field_usage_summary_dt[, total := rowSums(.SD), .SDcols = agency_cols]
setorder(field_usage_summary_dt, -total)

# Per-row percentages (0–100, one decimal); create parallel *_pct columns
pct_cols <- paste0(agency_cols, "_pct")
field_usage_summary_dt[
  , (pct_cols) := lapply(.SD, function(x) fifelse(total > 0, 
                                                round(100 * x / total, 1), 0)),
  .SDcols = agency_cols
]

# Console preview
cat("\nField usage by agency (counts):\n")
DT_to_print <- field_usage_summary_dt[, c("field", agency_cols, "total"), with = FALSE]

# Pre-format columns for alignment
display_df <- data.frame(
  field = format(DT_to_print$field, justify = "left"),
  lapply(DT_to_print[, agency_cols, with = FALSE], function(x) format(x, justify = "right")),
  total = format(DT_to_print$total, justify = "right")
)

# Set column names to match original
names(display_df) <- c("field", agency_cols, "total")

print(display_df, row.names = FALSE)
# Save to CSV fast
summary_table_file_path <- file.path(write_dir, "field_usage_summary_table.csv")
fwrite(field_usage_summary_dt, summary_table_file_path)

#cat("\nCSV written to:\n", summary_table_file_path, "\n")

#########################################################################
message("\nRemoving unused fields.")
# Clean-up memory by removing non-utilzied columns

# Keep ONLY the necessary columns
columns_to_keep <- c(
  "unique_key",
  "created_date", 
  "closed_date",
  "agency", 
  "agency_name", 
  "complaint_type",
  "location_type", 
  "incident_zip",
  "incident_address", 
  "street_name", 
  "cross_street_1",
  "cross_street_2", 
  "intersection_street_1", 
  "intersection_street_2",
  "address_type", 
  "landmark",
  "facility_type", 
  "status", 
  "due_date",
  "resolution_action_updated_date", 
  "community_board",
  "bbl", 
  "borough", 
  "x_coordinate_state_plane",
  "y_coordinate_state_plane", 
  "open_data_channel_type", 
  "park_facility_name",
  "park_borough", 
  "vehicle_type", 
  "taxi_company_borough",
  "latitude",
  "longitude", 
  "location"
)

# Remove unnecessary columns to free up memory
d311[, setdiff(names(d311), columns_to_keep) := NULL]

# Make a copy for troubleshooting purposes to avoid re-reading in data
#copy_d311 <- d311

#########################################################################
# Convert coordinate columns to numeric
coord_cols <- c("latitude", "longitude")
d311[, (coord_cols) := lapply(.SD, as.numeric), .SDcols = coord_cols]

#########################################################################
message("\nOrganizing complaint_types.")
cat("\n\n********** COMPLAINT TYPES **********")

total_rows <- nrow(d311)

# One pass: frequency + agency labeling per complaint_type
complaint_summary_dt <- d311[
  , .(
    count = .N,
    unique_agency_count = uniqueN(agency, na.rm = TRUE),
    agency = {
      u <- unique(agency)
      u <- u[!is.na(u)]
      if (length(u) == 1L) u else if (length(u) == 0L) NA_character_ else "MULTIPLE"
    }
  ),
  by = complaint_type
][
  order(-count)
][
  # compute percents from counts to avoid cumulative rounding drift
  , `:=`(
    percent = round(100 * count / total_rows, 2),
    cumulative_percent = round(100 * cumsum(count) / total_rows, 2)
  )
][]

cat("\nThere are", nrow(complaint_summary_dt), "different complaint_type(s).\n")

# ---- Console reports ----
top_n <- 20L
cat("\nTop ", top_n, " complaint_type(s) and responsible agency:\n", sep = "")
top_dt <- complaint_summary_dt[1:min(.N, top_n), .(complaint_type, count, percent, cumulative_percent, agency)]
top_df <- data.frame(
  complaint_type = format(top_dt$complaint_type, justify = "left"),
  count = format(top_dt$count, justify = "right"),
  percent = format(top_dt$percent, justify = "right"),
  cumulative_percent = format(top_dt$cumulative_percent, justify = "right"),
  agency = format(top_dt$agency, justify = "left")
)
print(top_df, row.names = FALSE)

cat("\nBottom ", top_n, " complaint_type(s) and responsible agency:\n", sep = "")
bottom_dt <- tail(complaint_summary_dt[, .(complaint_type, count, agency)], top_n)
bottom_df <- data.frame(
  complaint_type = format(bottom_dt$complaint_type, justify = "left"),
  count = format(bottom_dt$count, justify = "right"),
  agency = format(bottom_dt$agency, justify = "left")
)
print(bottom_df, row.names = FALSE)

cat("\nComplaints with multiple responsible agencies:\n")
multiple_agency_dt <- complaint_summary_dt[agency == "MULTIPLE", .(complaint_type, count, percent)]
multiple_df <- data.frame(
  complaint_type = format(head(multiple_agency_dt, top_n)$complaint_type, justify = "left"),
  count = format(head(multiple_agency_dt, top_n)$count, justify = "right"),
  percent = format(head(multiple_agency_dt, top_n)$percent, justify = "right")
)
print(multiple_df, row.names = FALSE)

# ---- Noise complaints (prefix "NOISE") ----
noise_dt <- complaint_summary_dt[startsWith(complaint_type, "NOISE")]

cat("\nThere are", nrow(noise_dt), "categories of noise complaints:\n")

noise_display_dt <- noise_dt[, .(complaint_type, count, percent)]

noise_df <- data.frame(
  complaint_type = format(noise_display_dt$complaint_type, justify = "left"),
  count = format(noise_display_dt$count, justify = "right"),
  percent = format(noise_display_dt$percent, justify = "right")
)

print(head(noise_df, top_n), row.names = FALSE)

noise_total <- noise_dt[, sum(count)]
noise_pct   <- round(100 * noise_total / total_rows, 1)
cat(
  "\nNoise complaints of all ", nrow(noise_dt), "types number ",
  format(noise_total, big.mark = ","),
  ", constituting ", noise_pct, "% of all SRs.\n", sep = ""
)

# chart
plot_pareto_combo(
  DT              = d311,
  x_col           = complaint_type,
  title           = "Pareto Analysis of Complaint Types",
  filename        = "SR_by_complaint_type_pareto_combo_chart.pdf",
  chart_dir       = chart_dir,
  show_labels     = FALSE,
  top_n            = 20,
  show_threshold_80 = FALSE,   # whether to draw the 80% reference line
  annotation_size = 3
)

#########################################################################
# Determine status of SRs
cat("\n\nSRs by Status (including NA if present)\n")

# Build a labeled status on the fly:
# - NA  -> "NA"
# - ""  -> "(blank)"  (optional; remove that branch if you don't want it)
status_summary_dt <- d311[
  , .(count = .N),
  by = .(status = fcase(
    is.na(status),                "NA",
    nzchar(as.character(status)), as.character(status),
    default = "(blank)"
  ))
][order(-count)]

# Percentages from counts
denom <- status_summary_dt[, sum(count)]
status_summary_dt[, `:=`(
  percentage            = round(100 * count / denom, 2),
  cumulative_percentage = round(100 * cumsum(count) / denom, 2)
)]

# Console print (pretty counts)
to_print <- copy(status_summary_dt)
to_print_df <- data.frame(
  status = format(to_print$status, justify = "left"),
  count = format(to_print$count, big.mark = ",", justify = "right"),
  percent = format(to_print$percent, justify = "right"),
  cumulative_percent = format(to_print$cumulative_percent, justify = "right")
)
print(to_print_df, row.names = FALSE)

#########################################################################

cat("\n\n**********VALIDATING DATA TYPES**********\n")

#########################################################################
# determine if the incident_zip field contain 5 numeric digits
# Find non-compliant ZIP5 values (format-only; no mutation)
message("\nValdiating data types.")

find_noncompliant_zip5 <- function(DT, zip_col = "incident_zip", sample_n = 10L, include_na = FALSE) {
  stopifnot(is.data.table(DT))
  stopifnot("unique_key" %in% names(DT), zip_col %in% names(DT))
  
  x <- trimws(DT[[zip_col]])                 # incident_zip is character
  is_na    <- is.na(x)
  zip_len  <- nchar(x, keepNA = TRUE)
  is_len5  <- zip_len == 5L
  is_digit <- !is_na & grepl("^[0-9]+$", x)
  ok       <- is_len5 & is_digit
  
  # indices to include as "non-compliant"
  bad_idx <- if (include_na) which(!ok | is_na) else which(!is_na & !ok)
  
  # Reason labels (format-only). 'NA' reason only used if include_na = TRUE
  reason <- fcase(
    !is_na & x == "",   "blank",
    !is_na & !is_len5,  "wrong_length",
    !is_na & !is_digit, "non_digit",
    is_na,              "NA",
    default = NA_character_
  )
  
  noncompliant <- DT[bad_idx, .(unique_key, agency, incident_zip = get(zip_col))][
    , `:=`(reason = reason[bad_idx], zip_length = zip_len[bad_idx])
  ]
  setindex(noncompliant, unique_key)         # no reorder
  
  # Summaries
  by_reason <- noncompliant[, .N, by = reason][order(-N)]
  by_agency <- noncompliant[, .N, by = agency][order(-N)]
  
  # Console
  cat("\nNon-compliant ", zip_col, " values (format-only",
      if (!include_na) "; NA excluded", "): ",
      format(nrow(noncompliant), big.mark=","), "\n", sep = "")
  if (nrow(noncompliant)) {
    cat("\nBy reason:\n");  print(by_reason, nrows = nrow(by_reason))
    cat("\nExamples:\n");   print(noncompliant[1:min(.N, sample_n)], nrows = min(.N, sample_n))
    cat("\nBy agency (top 10):\n"); print(by_agency[1:min(10L, .N)])
  }
  
  invisible(list(rows = noncompliant, by_reason = by_reason, by_agency = by_agency))
}

# Call (NAs are NOT counted as invalid):
res <- find_noncompliant_zip5(d311, zip_col = "incident_zip", sample_n = 10L)

#########################################################################
# determine if various fields are numeric values

# Type-only: TRUE if every non-NA token is numeric text (or the vector is numeric already)
# Options let you decide how strict to be without touching range.
are_all_numbers <- function(x, allow_na = TRUE, allow_nonfinite = FALSE, allow_sci = TRUE) {
  if (is.numeric(x)) {
    nf_ok <- allow_nonfinite || all(is.finite(x[!is.na(x)]))
    na_ok <- allow_na || all(!is.na(x))
    return(nf_ok && na_ok)
  }
  s <- trimws(as.character(x))
  pattern <- if (allow_sci)
    "^[+-]?(?:\\d+\\.?\\d*|\\d*\\.\\d+)(?:[eE][+-]?\\d+)?$"  # allow scientific notation
  else
    "^[+-]?(?:\\d+\\.?\\d*|\\d*\\.\\d+)$"
  ok <- nzchar(s) & grepl(pattern, s)
  if (allow_na) all(ok | is.na(s)) else all(ok & !is.na(s))
}

cols_to_check <- c("x_coordinate_state_plane", "y_coordinate_state_plane", "latitude", "longitude")
checks <- sapply(cols_to_check, function(col) are_all_numbers(d311[[col]]))

cat("\n\nType-only numeric checks:\n")
checks_df <- data.frame(
  column = names(checks),
  is_numeric = checks
)
print(checks_df, row.names = FALSE)

#########################################################################

cat("\n\n**********CHECKING FOR ALLOWABLE AND VALID VALUES**********\n")

#########################################################################
# Check lat/long values to see if they are within NYC city limits
message("\nValidating data for allowable and valid values.")

################################################################################
# Check for non-agreement with closed 'status' and 'close_date'

status_res <- report_status_closed_date_exceptions(
  d311,
  status_col    = "status",
  closed_col    = "closed_date",
  closed_values = c("Closed"),
  id_col        = "unique_key",
  n_show        = 10,
  make_charts   = TRUE,
  chart_dir     = file.path(chart_dir),
  x_col_name    = "agency",      # or "complaint_type", etc.
  top_n         = 30,
  flip          = FALSE,
  min_count     = 1
)


################################################################################
# --- NYC bounding box (from NYBBWI metadata) ---
# Source: New York City Borough Boundary — Water Included (NYBBWI) dataset. 
nyc_lat_north <- 40.917691
nyc_lat_south <- 40.477211
nyc_lon_west  <- -74.260380
nyc_lon_east  <- -73.699211

flag_out_of_bounds_latlon <- function(
    DT,
    lat_col = "latitude",
    lon_col = "longitude",
    bounds = list(
      lat = c(nyc_lat_south, nyc_lat_north),   # south, north
      lon = c(nyc_lon_west, nyc_lon_east)      # west, east
    ),
    id_cols = c("unique_key", "agency", "city"),
    sample_n = 20L,
    tolerance_m = 100    # ≈100 m margin
) {
  stopifnot(is.data.table(DT))
  stopifnot(lat_col %in% names(DT), lon_col %in% names(DT))
  
  # Convert tolerance (m) → degrees
  mean_lat <- mean(bounds$lat)
  deg_lat <- tolerance_m / 111000
  deg_lon <- tolerance_m / (111000 * cos(mean_lat * pi / 180))
  
  lat_min <- bounds$lat[1] - deg_lat
  lat_max <- bounds$lat[2] + deg_lat
  lon_min <- bounds$lon[1] - deg_lon
  lon_max <- bounds$lon[2] + deg_lon
  
  # Numeric conversion if needed (no rounding)
  lat_num <- suppressWarnings(as.numeric(DT[[lat_col]]))
  lon_num <- suppressWarnings(as.numeric(DT[[lon_col]]))
  
  # Identify out-of-bounds points
  lat_out_idx <- which(!is.na(lat_num) & !between(lat_num, lat_min, lat_max))
  lon_out_idx <- which(!is.na(lon_num) & !between(lon_num, lon_min, lon_max))
  both_out_idx <- intersect(lat_out_idx, lon_out_idx)
  
  keep_cols <- intersect(c(id_cols, lat_col, lon_col), names(DT))
  bad_lat  <- DT[lat_out_idx, ..keep_cols]
  bad_lon  <- DT[lon_out_idx, ..keep_cols]
  bad_both <- DT[both_out_idx, ..keep_cols]
  
  n_total <- nrow(DT)
  n_lat   <- nrow(bad_lat)
  n_lon   <- nrow(bad_lon)
  n_both  <- nrow(bad_both)
  
  cat("\nOut-of-bounds check (raw coords, ±", tolerance_m, " m buffer):\n", sep = "")
  cat("Latitude out-of-bounds:  ", format(n_lat, big.mark=","), " (",
      round(100 * n_lat / n_total, 4), "%)\n", sep = "")
  if (n_lat) print(bad_lat[1:min(.N, sample_n)], nrows = min(n_lat, sample_n))
  
  cat("\nLongitude out-of-bounds: ", format(n_lon, big.mark=","), " (",
      round(100 * n_lon / n_total, 4), "%)\n", sep = "")
  if (n_lon) print(bad_lon[1:min(.N, sample_n)], nrows = min(n_lon, sample_n))
  
  if (n_both) {
    cat("\nBoth lat & lon out-of-bounds: ", format(n_both, big.mark=","), " (",
        round(100 * n_both / n_total, 4), "%)\n", sep = "")
    print(bad_both[1:min(.N, sample_n)], nrows = min(n_both, sample_n))
  }
  
  invisible(list(bad_lat = bad_lat, bad_lon = bad_lon, bad_both = bad_both))
}

# --- Example call ---
res <- flag_out_of_bounds_latlon(
  d311,
  lat_col = "latitude",
  lon_col = "longitude",
  bounds = list(
    lat = c(nyc_lat_south, nyc_lat_north),
    lon = c(nyc_lon_west, nyc_lon_east)
  ),
  id_cols = c("unique_key", "agency", "city"),
  sample_n = 20L,
  tolerance_m = 100
)

################################################################################
# Check to see if any of the x or y state plane coordinates fall outside 
# the extreme points of New York City.
# Define the latitude and longitude points.
# Build points data frame using those variables
points_df <- data.frame(
  name = c("Northernmost", "Easternmost", "Southernmost", "Westernmost"),
  lat  = c(nyc_lat_north, nyc_lat_south, nyc_lat_south, nyc_lat_north),
  lon  = c(nyc_lon_east,  nyc_lon_east,  nyc_lon_west,  nyc_lon_west)
)

# Convert to an sf object and apply the State Plane projection.
# WGS84 lat/long.
points_sf <- st_as_sf(points_df, coords = c("lon", "lat"), crs = 4326) 
points_sp <- st_transform(points_sf, crs = 2263) # Convert to State Plane

# Change the x/y_coordinate_state_plane fields to "numeric" to enable comparison.
d311$x_coordinate_state_plane <- as.numeric(d311$x_coordinate_state_plane)
d311$y_coordinate_state_plane <- as.numeric(d311$y_coordinate_state_plane)

# Extract the bounding box (xmin, ymin, xmax, ymax) from the sf object
bbox <- st_bbox(points_sp)

# Assign the individual values to variables
xmin <- as.numeric(bbox["xmin"])
xmax <- as.numeric(bbox["xmax"])
ymin <- as.numeric(bbox["ymin"])
ymax <- as.numeric(bbox["ymax"])

# Filter NA values
d311_clean <- d311[!is.na(d311$x_coordinate_state_plane) & 
                     !is.na(d311$y_coordinate_state_plane), ]

# Check for x-coordinate outliers
x_outliers <- d311_clean[
  d311_clean$x_coordinate_state_plane < xmin |
    d311_clean$x_coordinate_state_plane > xmax,
]

# Check for y-coordinate outliers
y_outliers <- d311_clean[
  d311_clean$y_coordinate_state_plane < ymin |
    d311_clean$y_coordinate_state_plane > ymax,
]

# Print status for x-coordinate outliers
if (nrow(x_outliers) == 0) {
  cat("\nAll x_coordinate_state_plane values lie within the boundaries of NYC.")
} else {
  cat("\n\nThere is/are", nrow(x_outliers), 
      "x_coordinate_state_plane values outside the boundaries of NYC.\n")
  cat("\nHere are the first few rows of x-coordinate outliers:\n")
  print(head(x_outliers[, c("unique_key", "agency", "x_coordinate_state_plane")]))
}

# Print status for y-coordinate outliers
if (nrow(y_outliers) == 0) {
  cat("\nAll y_coordinate_state_plane values lie within the boundaries of NYC.")
} else {
  cat("\n\nThere are", nrow(y_outliers), 
      "y_coordinate_state_plane values outside the boundaries of NYC.\n")
  cat("\nHere are the first few rows of y-coordinate outliers:\n")
  print(head(y_outliers[, c("unique_key", "agency", "y_coordinate_state_plane")]))
}

################################################################################
# --- Allowed sets -------------------------------------------------------------

valid_address_types <- c(
  "ADDRESS","BBL","BLOCKFACE","INTERSECTION","PLACENAME","UNRECOGNIZED"
)

valid_statuses <- c(
  "ASSIGNED","CANCEL","CLOSED","IN PROGRESS","OPEN","PENDING","STARTED",
  "UNSPECIFIED"
)

valid_boroughs <- c(
  "BRONX","BROOKLYN","MANHATTAN","QUEENS","STATEN ISLAND","UNSPECIFIED"
)

valid_channels <- c("MOBILE","ONLINE","OTHER","PHONE","UNKNOWN")

valid_vehicle_types <- c(
  "AMBULETTE / PARATRANSIT","CAR","CAR SERVICE","COMMUTER VAN",
  "GREEN TAXI","OTHER","SUV","TRUCK","VAN"
)

# check for allowable values in the 'community_board' field
valid_community_boards <-
  c(
    "01 BRONX", "01 BROOKLYN", "01 MANHATTAN", "01 QUEENS", "01 STATEN ISLAND",
    "02 BRONX", "02 BROOKLYN", "02 MANHATTAN", "02 QUEENS", "02 STATEN ISLAND",
    "03 BRONX", "03 BROOKLYN", "03 MANHATTAN", "03 QUEENS", "03 STATEN ISLAND",
    "04 BRONX", "04 BROOKLYN", "04 MANHATTAN", "04 QUEENS",
    "05 BRONX", "05 BROOKLYN", "05 MANHATTAN", "05 QUEENS",
    "06 BRONX", "06 BROOKLYN", "06 MANHATTAN", "06 QUEENS",
    "07 BRONX", "07 BROOKLYN", "07 MANHATTAN", "07 QUEENS",
    "08 BRONX", "08 BROOKLYN", "08 MANHATTAN", "08 QUEENS",
    "09 BRONX", "09 BROOKLYN", "09 MANHATTAN", "09 QUEENS",
    "10 BRONX", "10 BROOKLYN", "10 MANHATTAN", "10 QUEENS",
    "11 BRONX", "11 BROOKLYN", "11 MANHATTAN", "11 QUEENS",
    "12 BRONX", "12 BROOKLYN", "12 MANHATTAN", "12 QUEENS",
    "13 BROOKLYN", "13 QUEENS",
    "14 BROOKLYN", "14 QUEENS",
    "15 BROOKLYN",
    "16 BROOKLYN",
    "17 BROOKLYN",
    "18 BROOKLYN",
    "UNSPECIFIED BRONX", "UNSPECIFIED BROOKLYN", "UNSPECIFIED MANHATTAN",
    "UNSPECIFIED QUEENS", "UNSPECIFIED STATEN ISLAND",
    "0 UNSPECIFIED"
  )

# Check for invalid zip codes in d311$incident_zip using USPSzipcodesOnly
valid_USPS_zipcodes <- as.list(USPSzipcodes$zip)

# Field to include "agency" in the computed dataset
valid_agencies <- unique(d311$agency)

# --- Centralized spec: field -> allowed values --------------------------------

valid_spec <- list(
  agency                 = valid_agencies,        # Add this line
  address_type           = valid_address_types,
  status                 = valid_statuses,
  borough                = valid_boroughs,
  park_borough           = valid_boroughs,
  taxi_company_borough   = valid_boroughs,
  open_data_channel_type = valid_channels,
  vehicle_type           = valid_vehicle_types,
  community_board        = valid_community_boards,
  incident_zip           = valid_USPS_zipcodes
)

# --- Run all validations in one pass ------------------------------------------

all_validation_results <- lapply(names(valid_spec), function(fld) {
  validate_values(
    x             = d311[[fld]],
    allowed       = valid_spec[[fld]],
    field         = fld,
    ignore_case   = TRUE,
    use_fastmatch = TRUE,
    quiet         = FALSE
  )
})

names(all_validation_results) <- names(valid_spec)

# --- Generate Pareto charts for fields with invalid data ------------------
cat("\nGenerating Pareto charts for fields with invalid data...\n")

for (field_name in names(all_validation_results)) {
  result <- all_validation_results[[field_name]]
  
  # Only create chart if there are invalid values
  if (result$counts$invalid > 0) {
    cat(sprintf("\nCreating Pareto chart for: %s (%d invalid values)\n", 
                field_name, result$counts$invalid))
    
    # Create mask for rows with invalid values for this field
    invalid_mask <- d311[[field_name]] %in% result$invalid_values
    invalid_rows <- d311[invalid_mask]
    
    cat("Number of invalid rows found:", nrow(invalid_rows), "\n")
    
    if (nrow(invalid_rows) > 0) {
      plot_pareto_combo(
        DT = invalid_rows,
        x_col = agency,
        chart_dir = chart_dir,
        filename = paste0("pareto_agency_", field_name, ".pdf"),
        title = sprintf("Agencies with Invalid %s Values", field_name),
        top_n = 30,
        show_labels       = TRUE,
        show_threshold_80 = TRUE,   # whether to draw the 80% reference line
        include_na = TRUE
      )
    }
  } else {
    cat(sprintf("Skipping %s - no invalid values found\n", field_name))
  }
}

# --- Preserve your original per-field variables (for drop-in compatibility) ----

address_type_results          <- all_validation_results[["address_type"]]
statusResults                 <- all_validation_results[["status"]]
boroughResults                <- all_validation_results[["borough"]]
park_boroughResults           <- all_validation_results[["park_borough"]]
taxi_company_boroughResults   <- all_validation_results[["taxi_company_borough"]]
open_data_channelResults      <- all_validation_results[["open_data_channel_type"]]
vehicle_typeResults           <- all_validation_results[["vehicle_type"]]
community_boardResults        <- all_validation_results[["community_board"]]   
incident_zipResults           <- all_validation_results[["incident_zip"]]

# --- Compact summary table (programmatic dashboard) ----------------------------
summary_dt <- data.table::rbindlist(
  lapply(all_validation_results, function(r) {
    data.table::data.table(
      field       = r$field,
      valid       = r$valid,
      invalid     = r$counts$invalid,
      nonblank    = r$counts$nonblank,
      pct_invalid = r$pct_invalid_nonblank
    )
  }),
  use.names = TRUE, fill = TRUE
)[order(-invalid, field)]

cat("\nValidation summary (sorted by invalid desc):\n")
summary_df <- data.frame(
  field = format(summary_dt$field, justify = "left"),
  valid = format(summary_dt$valid, justify = "right"),
  invalid = format(summary_dt$invalid, justify = "right"),
  nonblank = format(summary_dt$nonblank, justify = "right"),
  pct_invalid = format(summary_dt$pct_invalid, justify = "right")
)
print(summary_df, row.names = FALSE)

#########################################################################

cat("\n\n**********CHECKING FOR DATE FIELD ISSUES **********\n")

#########################################################################

# ==============================================================================
# SECTION 0: DURATION CALCULATIONS -- Do this first
# ==============================================================================
# Calculate service request durations between created_date and closed_date
# Uses UTC timezone for consistent temporal calculations

cat("\n=== CALCULATING SR DURATIONS FOR LATER USE ===\n")

d311 <- calculate_durations(d311, "created_date", "closed_date", tz = "America/New_York", in_place = FALSE)

# ==============================================================================
# SECTION 1: CREATED DATE ANALYSIS
# ==============================================================================

cat("\n=== CREATED DATE ANALYSIS ===\n")

# Assuming date_cols is a character vector of column names
# e.g., date_cols <- c("created_date", "closed_date", "resolved_date", "due_date")

for (col in date_cols) {
  cat("\n", rep("=", 60), "\n", sep = "")
  cat("Date Field:", col, "\n")
  cat(rep("=", 60), "\n\n", sep = "")
  
  # Extract year and create frequency table
  years <- lubridate::year(d311[[col]])
  freq_table <- table(years)
  
  # Create data.table for plotting
  summary_dt <- data.table(
    year = as.integer(names(freq_table)),
    count = as.integer(freq_table)
  )
  
  # Special handling for closed_date to prevent timeline plotting
  if (col == "closed_date") {
    summary_dt$year <- factor(summary_dt$year)
  }
  
  # Call plot_barchart with the proper parameters
  plot_barchart(
    DT = summary_dt,
    x_col = "year",
    y_col = "count",
    title = paste("Year Distribution -", col),
    show_labels = TRUE,
    x_label = "",
    y_label = "",
    console_print_title = paste("Year Distribution for", col)
  )
  
  cat("\n")
}

# total rows in d311
total_rows <- num_rows_d311

# helper function to format counts + percentages
fmt_count_pct <- function(n, total) {
  sprintf("%s (%.2f%%)", 
          prettyNum(n, big.mark=","), 
          round(100 * n / total), 2)
}

  # Anomaly checks
  future_created_dates        <- d311[created_date > as_of_date]
  past_created_dates          <- d311[created_date < genesis_date]
  missing_created_dates       <- d311[is.na(created_date)]
  midnight_only_created_dates <- d311[format(created_date, "%H:%M:%S") == "00:00:00"]
  noon_only_created_dates     <- d311[format(created_date, "%H:%M:%S") == "12:00:00"]

  # Summary
  cat("Created_date anomaly check (tz = America/New_York):\n")
  cat("  Future created dates:   ", 
      fmt_count_pct(nrow(future_created_dates), total_rows), "\n")
  cat("  Past created dates:     ", 
      fmt_count_pct(nrow(past_created_dates), total_rows), "\n")
  cat("  Midnight-only created:  ", 
      fmt_count_pct(nrow(midnight_only_created_dates), total_rows), "\n")
   cat("  Noon-only created:      ", 
      fmt_count_pct(nrow(noon_only_created_dates), total_rows), "\n")
  
  # Define anomaly datasets and their labels
  anomaly_list_created <- list(
    list(
      dt = future_created_dates,
      label = "Future Created Dates",
      condition = "Created in Future"
    ),
    list(
      dt = past_created_dates,
      label = paste0("Past Created Dates: before ", 
                     genesis_date,  " -- 311 launch date"),
      condition = "Created Before 311 System launch"
    ),
    list(
      dt = midnight_only_created_dates,
      label = "Midnight Created Dates",
      condition = "Created at Midnight"
    ),
    list(
      dt = noon_only_created_dates,
      label = "Noon Created Dates",
      condition = "Created at Noon"
    )
  )
  
  # Loop through and create charts
  # Simplified loop
  for (anomaly in anomaly_list_created) {
    if (nrow(anomaly$dt) > 0) {
      plot_date_field_analysis(
        DT = anomaly$dt,
        date_col = created_date,
        group_col = agency,
        chart_dir = chart_dir,
        label = anomaly$label,
        condition_text = anomaly$condition,
        min_agency_count = 2,
        top_n = 30
      )
    } else {
      cat(sprintf("\nSkipping %s - no records\n", anomaly$label))
    }
  }
  
  # Handle missing_created_dates separately
  if (nrow(missing_created_dates) > 0) {
    cat("\n=== Missing Created Dates Summary ===\n")
    cat(sprintf("Total records with missing created_date: %s\n", 
                format(nrow(missing_created_dates), big.mark = ",")))
    
    missing_summary <- missing_created_dates[, .N, by = agency][order(-N)]
    cat("\nTop agencies with missing created dates:\n")
    print(head(missing_summary, 10))
  } else {
    cat("\n=== Missing Created Dates Summary ===\n")
    cat("No records with missing created_date found.\n")
  }
  
# ==============================================================================
# SECTION 2: DUE DATE ANALYSIS
# ==============================================================================

# Identify service requests with due_date before created_date
# Logical impossibility indicating data entry errors

cat("\n=== DUE DATE ANALYSIS ===\n")
  
  # Anomaly checks
  future_due_dates        <- d311[due_date > as_of_date]
  past_due_dates          <- d311[due_date < genesis_date]
  missing_due_dates       <- d311[is.na(due_date)]
  midnight_only_due_dates <- d311[format(due_date, "%H:%M:%S") == "00:00:00"]
  noon_only_due_dates     <- d311[format(due_date, "%H:%M:%S") == "12:00:00"]
  
  # Exclude NAs from both columns before the comparison
  # Create the subset with the new column
  due_before_created <- d311[
    !is.na(due_date) & 
      !is.na(created_date) & 
      due_date < created_date
  ]
  
  # Summary
  cat("Due_date anomaly check (tz = America/New_York):\n")
  cat("  Future dates:   ", fmt_count_pct(nrow(future_due_dates), total_rows), "\n")
  cat("  Past dates:     ", fmt_count_pct(nrow(past_due_dates), total_rows), "\n")
  cat("  Missing dates:  ", fmt_count_pct(nrow(missing_due_dates), total_rows), "\n")
  cat("  Midnight-only:  ", fmt_count_pct(nrow(midnight_only_due_dates), total_rows), "\n")
  cat("  Noon-only:      ", fmt_count_pct(nrow(noon_only_due_dates), total_rows), "\n")
  cat("  Due < Created   ", fmt_count_pct(nrow(due_before_created), total_rows), "\n")
  
  # Define anomaly datasets and their labels for due_date
  anomaly_list_due <- list(
    list(
      dt = future_due_dates,
      label = "Future Due Dates",
      condition = "Due in Future"
    ),
    list(
      dt = past_due_dates,
      label = "Past Due Dates",
      condition = "Due Before Genesis"
    ),
    list(
      dt = midnight_only_due_dates,
      label = "Midnight Due Dates",
      
      condition = "Due at Midnight"
    ),
    list(
      dt = noon_only_due_dates,
      label = "Noon Due Dates",
      condition = "Due at Noon"
    ),
    list(
      dt = due_before_created,
      label = "Noon Due Dates before Created Date",
      condition = "Due < Created"
    )
  )
  
  # Loop through and create charts
  for (anomaly in anomaly_list_due) {
    if (nrow(anomaly$dt) > 0) {
      plot_date_field_analysis(
        DT = anomaly$dt,
        date_col = due_date,
        group_col = agency,
        chart_dir = chart_dir,
        label = anomaly$label,
        condition_text = anomaly$condition,
        min_agency_count = 2,
        top_n = 30
      )
    } else {
      cat(sprintf("\nSkipping %s - no records\n", anomaly$label))
    }
  }

    # Handle missing_due_dates separately
  if (nrow(missing_due_dates) > 0) {
    cat("\n=== Missing Due Dates Summary ===\n")
    cat(sprintf("Total records with missing due_date: %s\n", 
                format(nrow(missing_due_dates), big.mark = ",")))
    
    missing_summary <- missing_due_dates[, .N, by = agency][order(-N)]
    cat("\nTop agencies with missing due dates:\n")
    print(head(missing_summary, 10))
  } else {
    cat("\n=== Missing Due Dates Summary ===\n")
    cat("No records with missing due_date found.\n")
  }
 
  res <- report_due_before_created(
    DT = d311,
    chart_dir = chart_dir,
    boxplot_file = "due_before_created_boxplot.pdf",
    pareto_file = "due_before_created_pareto.pdf"
  )
  
# ==============================================================================
# SECTION 3: RESOLUTION UPDATE DATE ANALYSIS
# ==============================================================================
# Check for resolution_action_updated_date occurring before created_date
# Excludes same-day cases where resolution time = 00:00:00 (likely defaults)

cat("\n=== RESOLUTION UPDATE DATE ANALYSIS ===\n")

# Anomaly checks
future_resolution_action_updated_dates        <- d311[resolution_action_updated_date > as_of_date]
past_resolution_action_updated_dates          <- d311[resolution_action_updated_date < genesis_date]
missing_resolution_action_updated_dates       <- d311[is.na(resolution_action_updated_date)]
midnight_only_resolution_action_updated_dates <- d311[format(resolution_action_updated_date, "%H:%M:%S") == "00:00:00"]
noon_only_resolution_action_updated_dates     <- d311[format(resolution_action_updated_date, "%H:%M:%S") == "12:00:00"]  

# Exclude NAs from both columns in the comparison
updates_before_created <- d311[
  !is.na(resolution_action_updated_date) & 
  !is.na(created_date) & 
  resolution_action_updated_date < created_date
]

# Summary
cat("Resolution_action_updated_date anomaly check (tz = America/New_York):\n")
cat("  Future dates:   ", fmt_count_pct(nrow(future_resolution_action_updated_dates), total_rows), "\n")
cat("  Past dates:     ", fmt_count_pct(nrow(past_resolution_action_updated_dates), total_rows), "\n")
cat("  Missing dates:  ", fmt_count_pct(nrow(missing_resolution_action_updated_dates), total_rows), "\n")
cat("  Midnight-only:  ", fmt_count_pct(nrow(midnight_only_resolution_action_updated_dates), total_rows), "\n")
cat("  Noon-only:      ", fmt_count_pct(nrow(noon_only_resolution_action_updated_dates), total_rows), "\n")
cat("  Updates < Created:", fmt_count_pct(nrow(updates_before_created), total_rows), "\n")

# Define anomaly datasets and their labels for resolution_action_updated_date

anomaly_list_resolution <- list(
  list(
    dt = future_resolution_action_updated_dates,
    label = "Future Resolution Action Updated Dates",
    condition = "Resolution Action Updated in Future"
  ),
  list(
    dt = past_resolution_action_updated_dates,
    label = "Past Resolution Action Updated Dates",
    condition = "Resolution Action Updated Before Genesis"
  ),
  list(
    dt = midnight_only_resolution_action_updated_dates,
    label = "Midnight Resolution Action Updated Dates",
    condition = "Resolution Action Updated at Midnight"
  ),
  list(
    dt = noon_only_resolution_action_updated_dates,
    label = "Noon Resolution Action Updated Dates",
    condition = "Resolution Action Updated at Noon"
  ),
  list(
    dt = updates_before_created,
    label = "Resolution Action Updated Dates before Created Date",
    condition = "Resolution Action Updated < Created"
    
  )
)

# Loop through and create charts
for (anomaly in anomaly_list_resolution) {
  if (nrow(anomaly$dt) > 0) {
    plot_date_field_analysis(
      DT = anomaly$dt,
      date_col = resolution_action_updated_date,
      group_col = agency,
      chart_dir = chart_dir,
      label = anomaly$label,
      condition_text = anomaly$condition,
      min_agency_count = 2,
      top_n = 20
    )
  } else {
    cat(sprintf("\nSkipping %s - no records\n", anomaly$label))
  }
}


# Handle missing_resolution_action_updated_dates separately
if (nrow(missing_resolution_action_updated_dates) > 0) {
  cat("\n=== Missing Resolution Action Updated Dates Summary ===\n")
  cat(sprintf("Total records with missing resolution_action_updated_date: %s\n", 
              format(nrow(missing_resolution_action_updated_dates), big.mark = ",")))
  
  missing_summary <- missing_resolution_action_updated_dates[, .N, by = agency][order(-N)]
  cat("\nTop agencies with missing resolution action updated dates:\n")
  print(head(missing_summary, 10))
} else {
  cat("\n=== Missing Resolution Action Updated Dates Summary ===\n")
  cat("No records with missing resolution_action_updated_date found.\n")
}

res <- report_resolution_update_before_created(
  DT = d311,
  created_col = "created_date",
  resolution_col = "resolution_action_updated_date",
  agency_col = "agency",
  unique_key_col = "unique_key",
  chart_dir = chart_dir,
  sample_size = 10,
  make_pareto = TRUE,
  make_boxplot = TRUE
)

# ==============================================================================
# SECTION 4: POST-CLOSED RESOLUTION UPDATE ANALYSIS
# ==============================================================================
# Identify service requests with resolution updates occurring long after closure
# Flags potential process violations or data entry delays

cat("\n=== POST-CLOSED RESOLUTION UPDATE ANALYSIS ===\n")

# Enable interactive debugging on error
options(error = recover)

res <- report_post_closed_updates(
  DT = d311,
  resolution_action_threshold = 30,        # Days after closure
  too_large_threshold = 365 * 6,        # 6 years in days
  chart_dir = chart_dir
)

# ==============================================================================
# SECTION 5: CLOSED DATE ANALYSIS
# ==============================================================================
# Identify service requests with closed dates in the future
# Flags records where closed_date > max(created_date) + 1 day

cat("\n===  CLOSED DATE ANALYSIS ===\n")

# Anomaly checks
future_closed_dates           <- d311[closed_date > as_of_date]
past_closed_dates             <- d311[closed_date < genesis_date]
missing_closed_dates          <- d311[is.na(closed_date)]
midnight_only_closed_dates    <- d311[format(closed_date, "%H:%M:%S") == "00:00:00"]
noon_only_closed_dates        <- d311[format(closed_date, "%H:%M:%S") == "12:00:00"]
closed_before_created         <- d311[closed_date < created_date]

# Summary
# Summary
cat("closed_date anomaly check (tz = America/New_York):\n")
cat("  Future closed dates:   ", 
    fmt_count_pct(nrow(future_closed_dates), total_rows), "\n")
cat("  Past closed dates:     ", 
    fmt_count_pct(nrow(past_closed_dates), total_rows), "\n")
cat("  Missing closed dates:  ", 
    fmt_count_pct(nrow(missing_closed_dates), total_rows), "\n")
cat("  Midnight-only closed:  ", 
    fmt_count_pct(nrow(midnight_only_closed_dates), total_rows), "\n")
cat("  Noon-only closed:      ", 
    fmt_count_pct(nrow(noon_only_closed_dates), total_rows), "\n")
cat("  Closed < Created:      ", 
    fmt_count_pct(nrow(closed_before_created), total_rows), "\n")
# Define anomaly datasets and their labels for closed_date
anomaly_list_closed <- list(
  list(
    dt = future_closed_dates,
    label = "Closed Dates in the Future",
    
    condition = "Closed in Future"
  ),
  list(
    dt = past_closed_dates,
    label = "Closed Dates in the Past",
    
    condition = "Closed Before Created"
  ),
  list(
    dt = midnight_only_closed_dates,
    label = "Midnight Closed Dates",
    
    condition = "Closed at Midnight"
  ),
  list(
    dt = noon_only_closed_dates,
    label = "Noon Closed Dates",
    
    condition = "Closed at Noon"
  ),
  list(
    dt = closed_before_created,
    label = "Closed before Created",
    include_boxplot = TRUE,
    condition = "Closed < Created"
  )
)

# Loop through and create charts
for (anomaly in anomaly_list_resolution) {
  if (nrow(anomaly$dt) > 0) {
    plot_date_field_analysis(
      DT = anomaly$dt,
      date_col = closed_date,
      group_col = agency,
      chart_dir = chart_dir,
      label = anomaly$label,
      condition_text = anomaly$condition,
      min_agency_count = 2,
      top_n = 20
    )
  } else {
    cat(sprintf("\nSkipping %s - no records\n", anomaly$label))
  }
}

# Handle missing_closed_dates separately
if (nrow(missing_closed_dates) > 0) {
  cat("\n=== Missing Closed Dates Summary ===\n")
  cat(sprintf("Total records with missing closed_date: %s\n", 
              format(nrow(missing_closed_dates), big.mark = ",")))
  
  missing_summary <- missing_closed_dates[, .N, by = agency][order(-N)]
  cat("\nTop agencies with missing closed dates:\n")
  print(head(missing_summary, 10))
} else {
  cat("\n=== Missing Closed Dates Summary ===\n")
  cat("No records with missing closed_date found.\n")
}

res <- report_future_closed(
  DT = d311,
  make_plots = TRUE,
  chart_dir = chart_dir,
  max_closed_date = max_closed_date,
  boxplot_file = "future_closed_boxplot.pdf",
  pareto_file = "future_closed_pareto.pdf"
)

# ==============================================================================
# SECTION 6: DAYLIGHT SAVING TIME ANALYSIS
# ==============================================================================
# Analyze potential data quality issues related to DST transitions
# Examines both fall-back and spring-forward periods

cat("\n\n=== DAYLIGHT SAVING TIME ANALYSIS ===\n")

# DST Fall-back analysis (November - clocks fall back)
cat("\n--- DST Fall-back Analysis ---\n")
dst_end_summary <- analyze_dst_fallback(
      DT            = d311,
      chart_dir     = chart_dir,
      show_examples = 10
)

# DST Spring-forward analysis (March - clocks spring forward)
cat("\n--- DST Spring-forward Analysis ---\n")
dst_start_summary <- analyze_dst_springforward(
    DT            = d311,
    chart_dir     = chart_dir,
    show_examples = 10
) 


################################################################################

#zero_screenout <- audit_zero_time_screenout(d311)

# Call with the 2019 file path
result <- summarize_backlog(
  DT = d311,
  data_dir = data_dir
)

################################################################################
# ==============================================================================
# SECTION 6: TEMPORAL PATTERN ANALYSIS
# ==============================================================================
# Analyze datetime patterns for both created_date and closed_date
# Identifies unusual temporal clustering or systematic patterns

cat("\n=== ANALYZING TEMPORAL PATTERNS ===\n")

# 1. Define the columns to iterate over (as strings)
date_cols <- c(
  "resolution_action_updated_date",
  "created_date", 
  "closed_date"
)

# 2. Define the agency column name as a string (to be injected into the function call)
agency_col_string <- "agency"

# 3. Initialize a list to store the results from each run
all_patterns_results <- list()

# 4. Loop through each date column
for (col_name in date_cols) {
  
  label <- gsub("_date|[^A-Za-z]", "", col_name) 
  label <- tools::toTitleCase(label)
  
  cat(sprintf("\n--- %s Date Pattern Analysis ---\n", label))
  
  result_name <- paste0(tolower(label), "_patterns")
  
  # Pass strings directly - NO as.name()
  results <- analyze_datetime_patterns(
    DT = d311,
    datetime_col = col_name,              # Just the string
    chart_dir = chart_dir,
    label = label,
    agency_col = agency_col_string        # Just the string
  )
  
  all_patterns_results[[result_name]] <- results
}

#########################################################################

cat("\n\n********** DURATION ISSUES **********\n")

#########################################################################

message("\nChecking for duration anomalies.")

# ==============================================================================
# SECTION 1: POSITIVE DURATION ANALYSIS
# ==============================================================================
# Analyze service requests with positive (normal) durations
# Creates histogram for visualization of completion time distributions

cat("\n=== ANALYZING POSITIVE DURATIONS ===\n")

# Filter to positive duration records only
positive_data <- d311[duration_days > 0 & !is.na(duration_days)]

# Set histogram display limits for readability
upper_limit <- 30*3    # Maximum days to display (90 days)
lower_limit <- 2       # Minimum days to display

# Create bounded dataset for plotting
limited_positive_data <- positive_data[
  duration_days >= lower_limit & duration_days <= upper_limit
]

# Generate summary statistics
summary(positive_data$duration_day)

# Count records within plotting bounds
n_plotted <- nrow(limited_positive_data)

plot_histogram(
  DT         = positive_data,
  value_col  = "duration_days",
  title      = sprintf("Positive Duration Distribution (<= %s days)", upper_limit),
  x_label    = "Duration (days)",
  add_labels = TRUE,
  chart_dir  = chart_dir,
  filename   = "positive_duration_histogram.pdf",
  bins       = 300,
  alpha      = 0.8,
  outlier_percentile = 1.0,
  add_stats  = TRUE,
  width      = 13,
  height     = 8.5,
  xlim       = c(0, upper_limit)   # <-- NEW ARGUMENT
)

# ==============================================================================
# SECTION 2: NEGATIVE DURATION ANALYSIS
# ==============================================================================
# Analyze service requests with negative durations (data quality issues)
# Negative durations indicate closed_date occurs before created_date

cat("\n=== ANALYZING NEGATIVE DURATIONS ===\n")

# Filter to negative duration records
negative_data <- d311[duration_days < 0 & !is.na(duration_days)]

# Set histogram limits for negative values
upper_limit_neg <- 0      # Less negative (closer to zero)
lower_limit_neg <- -50   # More negative (further from zero)

# Create bounded dataset for plotting
limited_negative_data <- negative_data[
  duration_days >= lower_limit_neg & duration_days <= upper_limit_neg
]

# Generate summary statistics
summary(negative_data$duration_day)

# Count records within plotting bounds
n_plotted_neg <- nrow(limited_negative_data)


plot_histogram(
  DT         = limited_negative_data,
  value_col  = "duration_days",
  
  # Titles and labels
  title      = sprintf("Negative Duration Distribution (%d to %d days)", 
                       lower_limit_neg, upper_limit_neg),
  x_label    = "Duration (days)",
  chart_dir  = chart_dir,
  filename   = "negative_duration_histogram.pdf",
  add_labels = FALSE,        # <-- ADD THIS
  
  # Visual appearance
  bins       = 100,
  fill_color = "#D55E00",
  alpha      = 0.7,
  
  # Bounds
  xlim       = c(lower_limit_neg, upper_limit_neg),
  outlier_percentile = 1.0,   # don’t trim — you’re bounding manually
  
  # Stats & output
  add_stats  = TRUE,
  width      = 13,
  height     = 8.5
)

# ==============================================================================
# SECTION 3: SHORT DURATION ANALYSIS & THRESHOLD DETERMINATION
# ==============================================================================
# Analyze very short durations to identify suspicious patterns
# Determines statistical threshold for flagging anomalously short durations

cat("\n=== ANALYZING SHORT DURATIONS & SETTING THRESHOLDS ===\n")

# Run comprehensive skewed duration analysis
skewed_result <- analyze_skewed_durations(
  DT = d311,
  duration_col = "duration_sec",
  minimum_cutoff_sec = 2,
  upper_cutoff_sec = 60*60*24*2L,  # 2 days in seconds
  print_summary = TRUE,
  create_plots = TRUE,
  chart_dir = chart_dir
)

# Extract the LogNormal 3-standard-deviation threshold
threshold <- skewed_result$thresholds$log_3sd_lower
threshold_numeric <- round(as.numeric(threshold), 0)

# Create detailed histogram with threshold visualization
plot_duration_histogram(
  DT = d311,
  duration_col = "duration_sec",
  bin_width = 1,
  x_label_skip = 2,         # Show every 2nd x-axis label
  x_axis_angle = 45,        # Rotate labels for readability
  max_value = 90,           # Focus on first 90 seconds
  min_value = 2L,
  threshold_numeric = threshold_numeric,
  chart_dir = chart_dir
)

# Display threshold value
cat("LogNormal_3SD threshold:", threshold_numeric, "seconds\n")

# ==============================================================================
# SECTION 4: COMPREHENSIVE DURATION CATEGORY ANALYSIS
# ==============================================================================
# Systematic analysis of all duration categories:
# - Negative (small, large, extreme)
# - Zero durations
# - Near-zero (seconds-level)
# - One-second durations
# - Positive (small, large, extreme)

cat("\n=== COMPREHENSIVE DURATION CATEGORY ANALYSIS ===\n")
 
  duraton_analysis <- analyze_duration_QA(d311, chart_dir = chart_dir)
  
  # ==============================================================================
  # END OF DURATION ANALYSIS
  # ==============================================================================
  #########################################################################
  
  cat("\n\n**********CHECKING FOR DUPLICATE VALUES**********\n")

#########################################################################
message("\nChecking fields for duplicates.")

# Filter complete cases
valid_data <- d311[!is.na(location) & !is.na(latitude) & !is.na(longitude)]
if (nrow(valid_data) == 0) {
  cat("No complete location data found for comparison.\n")
  redundancy_pct <- 0
} else {
  # Use strcapture for safer parsing of "(lat, lon)" strings
  coords <- strcapture(
    pattern = "^\\(([^,]+),\\s*([^)]+)\\)$",
    x = valid_data$location,
    proto = list(lat = numeric(), lon = numeric())
  )
  
  # Combine safely into valid_data
  valid_data[, `:=`(
    location_lat = coords$lat,
    location_lon = coords$lon
  )]
  
  # Drop rows where regex failed
  complete_rows <- valid_data[!is.na(location_lat) & !is.na(location_lon)]
  
  total_rows <- nrow(complete_rows)
  
  if (total_rows == 0) {
    cat("No rows with extractable location coordinates found.\n")
    redundancy_pct <- 0
  } else {
    # Round to 5 decimal places, then allow small variance (±0.00001)
    complete_rows[, lat_match := abs(round(as.numeric(latitude), 6) - 
                               round(as.numeric(location_lat), 6)) <= 0.000001]
    complete_rows[, lon_match := abs(round(as.numeric(longitude), 6) - 
                               round(as.numeric(location_lon), 6)) <= 0.000001]
    complete_rows[, both_match := lat_match & lon_match]
    
    lat_matches  <- sum(complete_rows$lat_match, na.rm = TRUE)
    lon_matches  <- sum(complete_rows$lon_match, na.rm = TRUE)
    exact_matches <- sum(complete_rows$both_match, na.rm = TRUE)
    redundancy_pct <- 100 * exact_matches / total_rows
    
    cat("\nLocation redundancy check (5 decimals ±0.00001):\n")
    cat(sprintf("  Complete rows analyzed: %d\n", total_rows))
    cat(sprintf("  Latitude matches: %d (%.2f%%)\n", lat_matches, 
                100 * lat_matches / total_rows))
    cat(sprintf("  Longitude matches: %d (%.2f%%)\n", lon_matches, 
                100 * lon_matches / total_rows))
    cat(sprintf("  Both match exactly: %d (%.2f%%)\n", exact_matches, 
                redundancy_pct))
    
    if (redundancy_pct > 99) {
      cat("\n  → REDUNDANT: Location column is duplicate of lat/lon columns\n")
    } else {
      cat("  → NOT REDUNDANT: Location column contains different information\n")
      mismatches <- complete_rows[!both_match][1:3]
      if (nrow(mismatches) > 0) {
        cat("\nSample mismatches:\n")
        print(mismatches[, .(latitude, longitude, location, location_lat, 
                             location_lon)])
      }
    }
  }
}

#########################################################################
# check to see if there are any non-matches between 'borough' and 'park_borough'

# reference + duplicates
reference_field <- "borough"
dup_fields <- c("park_borough", "taxi_company_borough")

# (optional) sanity check that columns exist
missing <- setdiff(c(reference_field, dup_fields), names(d311))
if (length(missing)) stop("Missing columns: ", paste(missing, collapse = ", "))

# Run the checks (NA==NA counted as match; text normalized; Pareto by agency)
res_list <- lapply(dup_fields, function(dup) {
  detect_duplicates(
    d311,
    reference_field    = reference_field,
    duplicate_field    = dup,
    trim_ws            = TRUE,
    case_insensitive   = TRUE,
    blank_is_na        = TRUE,
    na_equal           = TRUE,          # <-- count matching NAs as matches
    percent_base       = "all_rows",
    make_pareto        = TRUE,          # <-- Pareto for non-matches
    pareto_group_col   = "agency",
    chart_dir          = chart_dir
  )
})
names(res_list) <- dup_fields

# Roll up your key metric: % duplication (matches) for each duplicate field
dup_summary <- data.table::rbindlist(lapply(seq_along(res_list), function(i) {
  data.table::data.table(
    duplicate_field = names(res_list)[i],
    pct_duplication = res_list[[i]]$stats$pct_matching
  )
}))
dup_summary[order(-pct_duplication)]

#########################################################################

cat("\n\n**********CROSS STREET/INTERSECTION STREET ANALYSYS**********\n")
message("\nCross_street and Intersection_street analysis.")

# Define street pairs to analyze
street_pairs <- list(
  list(street1 = "cross_street_1", street2 = "intersection_street_1"),
  list(street1 = "cross_street_2", street2 = "intersection_street_2"),
  list(street1 = "landmark", street2 = "street_name")
)

# Initialize results storage
all_comparison_results <- list()

# Loop through each street pair
for (i in seq_along(street_pairs)) {
  pair <- street_pairs[[i]]
  
  cat("\n")
  cat(paste(rep("=", 80), collapse = ""), "\n")
  cat(sprintf("STREET PAIR ANALYSIS %d of %d\n", i, length(street_pairs)))
#  cat(paste(rep("=", 80), collapse = ""), "\n")
  
  # Run comparison analysis
  comparison_result <- compare_street_analyses(
    dt = d311,
    street1_col = pair$street1,
    street2_col = pair$street2,
    chart_dir = chart_dir,
    run_enhanced = FALSE
  )
  
  # Store results with descriptive name
  pair_name <- sprintf("%s_vs_%s", pair$street1, pair$street2)
  all_comparison_results[[pair_name]] <- comparison_result
  
#  cat(sprintf("Completed analysis for %s vs %s\n", pair$street1, pair$street2))
}

# Summary across all pairs
cat("\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("SUMMARY ACROSS ALL STREET PAIRS\n")
#cat(paste(rep("=", 80), collapse = ""), "\n")

for (i in seq_along(all_comparison_results)) {
  result <- all_comparison_results[[i]]
  pair_name <- names(all_comparison_results)[i]
  
  cat(sprintf("\n%s:\n", toupper(gsub("_", " ", pair_name))))
  cat(sprintf("  Match rate:     %5.4f%%\n", result$raw_analysis$match_rate * 100))
  cat(sprintf("  Cleaned match rate: %5.4f%%\n", result$cleaned_analysis$match_rate * 100))
  cat(sprintf("  Improvement:        %5.4f%% (%s records)\n", 
              result$comparison$match_rate_improvement * 100,
              scales::comma(result$comparison$match_improvement)))
}

cat(sprintf("\nTotal street pair analyses completed: %d\n", length(all_comparison_results)))

#########################################################################
# Conclude program
# Store the program end time and calculate the duration
programStop <- as.POSIXct(Sys.time())
formatted_end_time <- format(programStop, "%Y-%m-%d %H:%M:%S")

# Calculate the duration of the program (in seconds)
duration_seconds <- as.numeric(difftime(programStop, programStart,  
                                        units = "secs"))

# Convert the duration to a formatted string (hrs, mins, and secs)
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

if (isTRUE(enable_sink)) {
  sink()  # restore normal console output
}

#########################################################################
# Call the end_program function with the formatted end time and duration string
end_program(formatted_end_time, duration_string)

#########################################################################