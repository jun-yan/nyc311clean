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

# -------------------------------------------------------------
# 📁 Set Working Directory for the Project.
# -------------------------------------------------------------
# Set working directory to the location of the initialization script
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

# Define the path for the main data file (RDS file)
data_dir <- file.path(base_dir, "data")

# Define the path for the charts
chart_dir <- file.path(base_dir, "charts", "timeline_charts")

# Define the path to the directory containing your function scripts
functions_dir <- file.path(base_dir, "code", "functions")

# Define the path to the directory containing the console output
console_dir <- file.path(base_dir, "console_output")

# Create the directory for the reduced size file following shrinkage code.
write_dir <- file.path(base_dir, "data")

# Create the directory for the raw data
raw_data_dir <- file.path(data_dir, "raw_data")

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

# -------------------------------------------------------------
# 📊 Read CSV and Compute Yearly Counts
# -------------------------------------------------------------

# Read the CSV file
csv_file <- file.path(raw_data_dir, "2010-2025_created_date.csv")
dt <- fread(csv_file, sep = "\n", header = FALSE, col.names = "raw_line")

# Remove the header row and quotes
dt <- dt[raw_line != '"Created Date"']
dt[, created_date := gsub('^"|"$', '', raw_line)]
dt[, raw_line := NULL]

# Convert to data.table with proper column name
setnames(dt, "created_date", "Created Date")

# Modify column names to snake_case
dt <- modify_column_names(dt)

# Convert created_date to POSIXct datetime format and extract year
# Format: "10/15/2025 02:53:01 AM"
dt[, created_date := mdy_hms(created_date)]
dt[, year := year(created_date)]

# Compute counts by year
yearly_counts <- dt[, .(count = .N), by = year]
setorder(yearly_counts, year)

# Print summary to console
cat("\n=== Yearly Service Request Counts ===\n")
print(yearly_counts)
cat("\nTotal Records:", sum(yearly_counts$count), "\n")

# -------------------------------------------------------------
# 📈 Create Bar Chart with Trendline
# -------------------------------------------------------------

yearly_chart <- plot_barchart(
  DT = yearly_counts,
  x_col = "year",
  y_col = "count",
  
  # Titles and labels
  title = "Yearly SR counts",
  x_label = "Year",
  y_label = "Count",
  
  # Appearance
  fill_color = "#009E73",
  
  # Data labels
  show_labels = TRUE,
  label_vjust = -0.5,
  
  # Trendline
  add_trendline = TRUE,
  trendline_color = "#D55E00",
  trendline_method = "lm",
  trendline_size = 1.2,
  
  # Console output
  console_print_title = "Yearly Service Request Summary",
  show_summary = TRUE,
  
  # Axis customization
  y_axis_labels = scales::comma,
  
  # Save options
  chart_dir = chart_dir,
  filename = "yearly_sr_counts.png"
)

print(yearly_chart)
Sys.sleep(3)
cat("\n✅ Chart created successfully and saved to:", chart_dir, "\n")






