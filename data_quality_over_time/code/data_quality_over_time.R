
#########################################################################
create_condition_plot <- function(data, title, y_label, subtitle = NULL, value_field = "fraction") {
  
  # Make a copy of the data to avoid modifying the original
  data_copy <- data.frame(data)
  
  # Convert year_month to Date if it's not already 
  if(!inherits(data_copy$year_month, "Date")) {
    # Check if it's in YYYY-MM format
    if(is.character(data_copy$year_month) && 
       all(grepl("^\\d{4}-\\d{2}$", data_copy$year_month[!is.na(data_copy$year_month)]))) {
      # Add day to make it a full date
      data_copy$year_month <- as.Date(paste0(data_copy$year_month, "-01"))
    } else {
      # Try direct conversion if it's in another format
      tryCatch({
        data_copy$year_month <- as.Date(data_copy$year_month)
      }, error = function(e) {
        stop("Could not convert year_month to Date format. Please ensure it's in a valid date format.")
      })
    }
  }
  
  # Calculate mean and standard deviation for the three-sigma limits
  process_mean <- mean(data_copy[[value_field]])
  process_sd <- sd(data_copy[[value_field]])
  
  # Calculate the total timespan of the data in days
  total_days <- as.numeric(max(data_copy$year_month) - min(data_copy$year_month))
  
  # Fit linear model (using dynamic field name)
  data_copy$date_numeric <- as.numeric(data_copy$year_month)
  lm_formula <- as.formula(paste0(value_field, " ~ date_numeric"))
  lm_model <- lm(lm_formula, data = data_copy)
  
  # Calculate total percentage change over the entire period
  slope <- coef(lm_model)[2]  # Change per day
  total_change <- slope * total_days
  total_percent_change <- (total_change / process_mean) * 100
  
  # Format the trend label with + or - sign
  trend_label <- ifelse(total_percent_change >= 0, 
                        paste0("+", format(round(total_percent_change, 1), nsmall = 1), "%"),
                        paste0(format(round(total_percent_change, 1), nsmall = 1), "%"))
  
  # Compute a mid-range Y position
  y_trend_position <- median(data_copy[[value_field]], na.rm = TRUE) + 
    (diff(range(data_copy[[value_field]], na.rm = TRUE)) * 0.05)  # Slight offset
  
  # Compute a mid-range X position
  x_trend_position <- median(data_copy$year_month)
  

  # Create the plot (using dynamic mapping)
  p <- ggplot(data_copy, aes(x = year_month, y = .data[[value_field]])) +
    
    # Add the points and line
    geom_line(color = "steelblue2", linewidth = 0.75) +
    geom_point(size = 2.75, color = "steelblue4", shape = 18) +
    
    # Add the mean and sigma lines
    geom_hline(yintercept = process_mean + 3*process_sd, color = "red", linetype = "dotted", linewidth = 0.7) +
    geom_hline(yintercept = max(0, process_mean - 3*process_sd), color = "red", linetype = "dotted", linewidth = 0.7) +
    geom_hline(aes(yintercept = process_mean), linetype = "dashed", color = "darkred", linewidth = 0.8) +
    
    # Add text labels for the sigma lines
    annotate("text", x = data_copy$year_month[2], y = process_mean + 3*process_sd, 
             label = paste("+3σ:", format(round(process_mean + 3*process_sd, 4), nsmall = 4)), 
             vjust = -0.5, hjust = 0, color = "red", size = 3) +
    
    annotate("text", x = data_copy$year_month[2], y = max(0, process_mean - 3*process_sd), 
             label = paste("-3σ:", format(round(max(0, process_mean - 3*process_sd), 4), nsmall = 4)), 
             vjust = 1.5, hjust = 0, color = "red", size = 3) +
    
    # Add linear trend line with custom confidence band
    geom_smooth(method = "lm", 
                se = TRUE, 
                color = "gray45", 
                linewidth = 0.9, 
                fill = "gray81", 
                alpha = 0.3) +
    
    # Add trend annotation (using dynamic calculation for y position)
    # Add trend annotation in the middle
    annotate("text", 
             x = x_trend_position, 
             y = y_trend_position,  
             label = paste("Trend:", trend_label),
             hjust = 0.5, vjust = -1,  
             color = "gray35", fontface = "bold", size = 5) +
    
    # Formatting
    labs(
      title = title,
      subtitle = paste(subtitle, "\n\n"),
      x = "Year-Month",
      y = y_label,
      caption = paste("Mean:", format(round(process_mean, 4), nsmall = 4))
    ) +
    
    scale_x_date(
      date_breaks = "1 year",  # Show tick marks every year
      date_labels = "%Y"       # Format as year only
    ) +
    theme(
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.75),  # Black border around plot area
      axis.text.x = element_text(angle = 10, hjust = 1, color = "black"),  # Ensure labels are visible
      panel.grid.major = element_line(color = "gray88", linewidth = 0.5),  # Adjust grid color for contrast
      panel.grid.minor = element_blank(),
      plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"),  
      panel.background = element_rect(fill = "gray99", color = NA),  # Dark gray for plot panel
      plot.background = element_rect(fill = "gray89", color = NA),   # Dark gray for entire plot background
      plot.caption = element_text(hjust = 1, size = 9, color = "black"),
      axis.title.y = element_text(margin = margin(r = 10, l = 0), color = "black"),
      axis.title.x = element_text(color = "black"),
      axis.text.y = element_text(color = "black"),
      legend.background = element_rect(fill = "gray95"),
      legend.text = element_text(color = "black"),
      legend.title = element_text(color = "black")
    )
    
    scale_y_continuous(labels = function(x) format(round(x, 4), nsmall = 4))
  
  return(list(
    plot = p,
    trend_label = trend_label,
    title = title,
    process_mean = process_mean
  ))
}

#########################################################################
# For analyze_data_conditions function:
analyze_data_conditions <- function(cleaned_data) {
  
  # Calculate the minimum monthly count to use as sample size
  total_per_month <- cleaned_data[, .N, by = year_month]
  min_count <- min(total_per_month$N)
#  print(paste("Using minimum monthly count:", min_count))
  
  # Take random samples of equal size from each month
  sampled_data <- data.table()
  
  # Set seed for reproducibility
  set.seed(42)
  
  # Loop through each month
  for (month in unique(cleaned_data$year_month)) {
    # Get data for this month
    month_data <- cleaned_data[year_month == month]
    
    # Take random sample of size min_count from this month
    month_sample <- month_data[sample(.N, min_count, replace = FALSE)]
    
    # Add to our sampled dataset
    sampled_data <- rbind(sampled_data, month_sample)
  }
  
  # Create indicators for all conditions (in both sampled and full data)
  # First for sampled data
  sampled_data[, midnight_closing := ifelse(!is.na(closed_date), format(closed_date, "%H:%M:%S") == "00:00:00", FALSE)]
  sampled_data[, resolution_before_creation := ifelse(!is.na(resolution_action_updated_date) & !is.na(created_date), 
                                                      resolution_action_updated_date < created_date, FALSE)]
  sampled_data[, status_not_closed_with_closed_date := ifelse(!is.na(closed_date) & !is.na(status), 
                                                              !is.na(closed_date) & status != "CLOSED", FALSE)]
  sampled_data[, status_closed_no_closed_date := ifelse(!is.na(status), 
                                                        status == "CLOSED" & is.na(closed_date), FALSE)]
  sampled_data[, late_resolution_update := ifelse(!is.na(closed_date) & !is.na(resolution_action_updated_date), 
                                                  as.numeric(difftime(resolution_action_updated_date, closed_date, units = "days")) > 90, FALSE)]
  sampled_data[, negative_or_zero_duration := ifelse(!is.na(closed_date) & !is.na(created_date), 
                                                     closed_date <= created_date, FALSE)]
  
  # Now for full data
  cleaned_data[, midnight_closing := ifelse(!is.na(closed_date), format(closed_date, "%H:%M:%S") == "00:00:00", FALSE)]
  cleaned_data[, resolution_before_creation := ifelse(!is.na(resolution_action_updated_date) & !is.na(created_date), 
                                                      resolution_action_updated_date < created_date, FALSE)]
  cleaned_data[, status_not_closed_with_closed_date := ifelse(!is.na(closed_date) & !is.na(status), 
                                                              !is.na(closed_date) & status != "CLOSED", FALSE)]
  cleaned_data[, status_closed_no_closed_date := ifelse(!is.na(status), 
                                                        status == "CLOSED" & is.na(closed_date), FALSE)]
  cleaned_data[, late_resolution_update := ifelse(!is.na(closed_date) & !is.na(resolution_action_updated_date), 
                                                  as.numeric(difftime(resolution_action_updated_date, closed_date, units = "days")) > 90, FALSE)]
  cleaned_data[, negative_or_zero_duration := ifelse(!is.na(closed_date) & !is.na(created_date), 
                                                     closed_date <= created_date, FALSE)]
  
  # Create a list to store all results
  results <- list()
  
  # Define conditions
  conditions <- c(
    "midnight_closing", 
    "resolution_before_creation", 
    "status_not_closed_with_closed_date", 
    "status_closed_no_closed_date", 
    "late_resolution_update", 
    "negative_or_zero_duration"
  )
  
  # Process each condition for both sampled and full data
  for (condition in conditions) {
    # 1. For sampled data (for QCC and QIC)
    sampled_result <- sampled_data[, .(
      count = sum(get(condition), na.rm = TRUE),
      N = .N,
      fraction = sum(get(condition), na.rm = TRUE) / .N
    ), by = year_month]
    
    # 2. For full data (for ggplot)
    full_result <- cleaned_data[, .(
      count = sum(get(condition), na.rm = TRUE),
      N = .N,
      fraction = sum(get(condition), na.rm = TRUE) / .N
    ), by = year_month]
    
    # Make sure the data is in chronological order
    sampled_result <- sampled_result[order(year_month)]
    full_result <- full_result[order(year_month)]
    
    # Convert year_month from character to Date for proper plotting
    sampled_result[, year_month := as.Date(paste0(year_month, "-01"))]
    full_result[, year_month := as.Date(paste0(year_month, "-01"))]
    
    # Store in results list
    results[[paste0(condition, "_sampled")]] <- sampled_result
    results[[condition]] <- full_result
  }
  
  return(results)
}

#########################################################################
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
  "qcc",
  "qicharts2",
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
setwd("C:/Users/David/OneDrive/Documents/datacleaningproject/nyc311clean/data_quality_over_time")

#########################################################################
main_data_file <- "2-year_dataset_AS_OF_02-27-2025.csv"

# Extract the 10-character date after "AS_OF_"
max_closed_date <- sub(".*AS_OF_([0-9-]+)\\.csv$", "\\1", main_data_file)

# Convert to POSIXct format
max_closed_date <- as.POSIXct(max_closed_date, format = "%m-%d-%Y", tz = "America/New_York")
                    + (23*3600 + 59*60 + 59)
message("\nMax closed date:", max_closed_date)

#########################################################################
# Set the base directory under the working directory base_dir <- getwd()
base_dir <- getwd()

# Define the console output directory and file name.
output_dir <- file.path(base_dir, "console_output")
output_file <- file.path(output_dir, "data_qualtiy_over_time_console_output.txt")

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

#####################
# Process 311 data
cat("\nProcessing 311 Service Request data...\n")
main_data_path <- file.path(data_dir, main_data_file)
rds_file <- gsub("\\.csv$", ".rds", main_data_file)
rds_path <- file.path(data_dir, rds_file)

#####################
# Read only the columns you need from raw data
# Read only the columns you need from raw data
raw_data <- fread(main_data_path, 
                  select = c("Created Date", 
                             "Closed Date", 
                             "Incident Zip", 
                             "Status", 
                             "Resolution Action Updated Date", 
                             "Community Board"))

cat("Initial row count:", nrow(raw_data), "\n")

#####################
# Clean column names
cleaned_data <- modify_column_names(raw_data)
cat("Column names cleaned\n")

#####################
# Convert all non-date columns to uppercase
for (col in names(cleaned_data)) {
  if (!(col %in% date_columns) && is.character(cleaned_data[[col]])) {
    cleaned_data[[col]] <- toupper(cleaned_data[[col]])
  }
}

#####################
#Consolidate agency names

# Check if both "agency" and "agency_name" exist in the dataset
required_agency_columns <- c("agency", "agency_name")

if (all(required_agency_columns %in% names(cleaned_data))) {
  cat("Consolidating agency names...\n")
  cleaned_data <- consolidate_agencies(cleaned_data)
  cat("Agency consolidation complete\n")
} else {
  cat("Skipping agency consolidation: Missing required columns -",
      paste(setdiff(required_agency_columns, names(cleaned_data)), collapse=", "), "\n")
}

#####################
# Standardize date formats

# Ensure only existing date columns are processed
valid_date_columns <- intersect(date_columns, names(cleaned_data))

if (length(valid_date_columns) > 0) {
  cat("Standardizing date formats...\n")
  cleaned_data[, (valid_date_columns) := lapply(.SD, function(x) {
    as.POSIXct(x, format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC")
  }), .SDcols = valid_date_columns]
#  cat("Date standardization complete\n")
} else {
  cat("Skipping date standardization: No matching date columns found.\n")
}

#####################  
# Adjust February 29 dates
cat("\nChecking for February 29 dates to adjust...\n")

# Identify which date columns exist in the dataset
valid_date_columns <- intersect(date_columns, names(cleaned_data))

if (length(valid_date_columns) > 0) {
  cleaned_data <- adjust_feb_29_to_28(cleaned_data, valid_date_columns)
  cat("February 29 date adjustments complete\n")
} else {
  cat("Skipping February 29 adjustments: No matching date columns found.\n")
}

#####################
# Ensure cleaned_data is a data.table
setDT(cleaned_data)  
  
# Save processed data
saveRDS(cleaned_data, rds_path)
cat("\nSaved cleaned 311 data to:", rds_path, "\n")
  
##########################################################################  
# Process USPS data
usps_data_file <- "USPS_zipcodes.csv"
  
cat("\nProcessing USPS Zipcode data...\n")
usps_path <- file.path(data_dir, usps_data_file)
usps_rds_file <- gsub("\\.csv$", ".rds", usps_data_file)
  
zipcode_data <- fread(
    usps_path,
    select = "DELIVERY ZIPCODE",
    colClasses = "character"
  )
  
zipcode_data <- unique(zipcode_data)
zipcode_data <- modify_column_names(zipcode_data)
  
# Save the RDS file
usps_rds_file <- gsub("\\.csv$", ".rds", usps_data_file)

if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)
  
# Construct the full file path using the existing variable name
file_path <- file.path(data_dir, usps_rds_file)

# Save the zipcode_data to this location
saveRDS(zipcode_data, file = file_path)

# Print summary
cat("\nUSPS data RDS saved at:", file_path, "\n")

##########################################################################      
# Check if the file exists before loading
if (file.exists(rds_path)) {
  cleaned_data <- readRDS(rds_path)  # Load dataset
  cat("File successfully read:", rds_path, "\n")
} else {
  stop("Error: File not found at path:", rds_path)
}

#labels = function(x) format(round(x, 4), nsmall = 4)

# Extract year-month for grouping
cleaned_data[, year_month := format(created_date, "%Y-%m")]

##########################################################################    
# Analyze all conditions
data_condition_results <- analyze_data_conditions(cleaned_data)

# Define updated subtitles with your preferred descriptions
subtitles <- list(
  midnight_closing = "closed_date occurs exactly at 00:00:00",
  resolution_before_creation = "resolution_action_updated_date is before created_date",
  status_not_closed_with_closed_date = "closed_date exists but status is not CLOSED",
  status_closed_no_closed_date = "status is CLOSED but there is no closed_date",
  late_resolution_update = "resolution_action_updated_date is >90 days after closed_date",
  negative_or_zero_duration = "closed_date is <= created_date (infeasible duration)"
)

# Filter just the original (non-sampled) condition names
original_conditions <- names(data_condition_results)[!grepl("_sampled$", names(data_condition_results))]

# Initialize a list to store summaries
condition_summary <- list()

# Loop through only the original conditions
for (condition in original_conditions) {
  # Get both full and sampled data
  full_data <- data_condition_results[[condition]]
  sampled_data <- data_condition_results[[paste0(condition, "_sampled")]]
  
  cat("\n\n======= CONDITION:", condition, "=======\n")
  
  # 1. ggplot chart (using FULL data)
  result <- create_condition_plot(
    full_data,
    paste("Proportion of", gsub("_", " ", tools::toTitleCase(condition))), 
    "Non-conforming Proportion",
    subtitle = subtitles[[condition]],
    "fraction"
  )
  
  # Extract the plot and other information
  gg_plot <- result$plot
  trend_label <- result$trend_label
  plot_title <- result$title
  process_mean <- result$process_mean
  
  # Print the plot
  print(gg_plot)
  
  # Store or print the captured information
  condition_summary[[condition]] <- list(
    trend = trend_label,
    title = plot_title,
    mean = process_mean,
    "fraction"
  )
  
  # Construct the file path dynamically
  file_path <- paste0(chart_dir, "/", condition, ".pdf")
  
  # Save the last printed plot
  ggsave(file_path, plot = last_plot(), width = 10, height = 7)
  
  # Add a 4 second delay to let plots render completely
  cat("\nWaiting 4 seconds before continuing to next chart...\n")
  Sys.sleep(4)   
  
  # 2. QCC chart (using SAMPLED data)
  # Create a file name using the condition
  file_name <- paste0(chart_dir, "/qcc_p_chart_", condition, ".pdf")
  
  # First create and display the plot in RStudio
  tryCatch({
    qcc_plot <- qcc(sampled_data$count, 
                    sizes = sampled_data$N, 
                    type = "p", 
                    title = paste("QCC p-chart of", gsub("_", " ", tools::toTitleCase(condition))),
                    xlab = "Year-Month",
                    ylab = "Non-conforming Proportion",
                    labels = format(sampled_data$year_month, "%Y-%m"))
  }, error = function(e) {
    cat("Error creating QCC chart:", e$message, "\n")
  })
  
  # Then save the displayed plot to the file
  pdf(file_name, width = 10, height = 7)
  plot(qcc_plot)  # Re-draw the plot to the PDF device
  dev.off()
  
  # Add a 4 second delay to let plots render completely
  cat("\nWaiting 4 seconds before continuing to next chart...\n")
  Sys.sleep(4)   
}

# Keep only the specified columns
cleaned_data <- cleaned_data[, c("created_date", 
                                 "closed_date", 
                                 "incident_zip", 
                                 "status", 
                                 "resolution_action_updated_date", 
                                 "community_board", 
                                 "year_month")]

# Create a data frame from the summaries
summary_df <- data.frame(
  condition = names(condition_summary),
  title = sapply(condition_summary, function(x) x$title),
  trend = sapply(condition_summary, function(x) x$trend),
  mean = sapply(condition_summary, function(x) x$mean)
)

# print(summary_df)
# 
# # Write to CSV
# write.csv(summary_df, paste0(chart_dir, "/condition_summaries.csv"), row.names = FALSE)

##########################################################################    
analyze_invalid_values <- function(data, field_name, valid_values_list, valid_field_name, seed = 42) {
  
  # Step 1: Remove NAs from the specified field
  data_no_na <- data[ !is.na(data[[field_name]]) & data[[field_name]] != "", ]
  
  # Step 2: Add boolean column for valid values
  data_no_na$zip_validity <- data_no_na[[field_name]] %in% valid_values_list[[valid_field_name]]
  # cat("\nRaw data boolean results\n")
  # table(data_no_na$zip_validity)
  
  # Step 3: Find minimum count per year_month to use as sample size
  total_per_month <- data_no_na[, .N, by = year_month]
  min_count <- min(total_per_month$N)
#  print(paste("Using minimum monthly count:", min_count))
  
  # Step 4: Create the sampled dataset
  # Take random samples of equal size from each month
  sampled_data <- data.table()
  
  # Loop through each month
  for (month in unique(data_no_na$year_month)) {
    # Get data for this month
    month_data <- data_no_na[year_month == month]
    
    # Take random sample of size min_count from this month
    month_sample <- month_data[sample(.N, min_count, replace = FALSE)]
    
    # Add to our sampled dataset
    sampled_data <- rbind(sampled_data, month_sample)
  }
  
  sampled_data$zip_validity <- sampled_data[[field_name]] %in% valid_values_list[[valid_field_name]]
#  cat("\n\nSampled data boolean results\n")
#  table(sampled_data$zip_validity)
  
  # Step 5: Count invalid values in full and sampled datasets
  full_summary <- data_no_na[, .(
    total_records = .N,
    invalid_zips = sum(!zip_validity),
    invalid_fraction = sum(zip_validity == FALSE) / .N
  ), by = year_month]
  
  
  # cat("\nFull summary")
  # print(full_summary)
  
  sampled_summary <- sampled_data[, .(
    total_records = .N,
    invalid_zips = sum(!zip_validity),
    invalid_fraction = sum(zip_validity == FALSE) / .N
  ), by = year_month]
  
  # cat("\nSampled summary")
  # print(sampled_summary)
  
  
  # Return all the datasets
  return(list(
    full_summary,
    sampled_summary))
}

#######################
# Read the ZIP codes reference data
valid_zipcodes_path <- file.path(data_dir, usps_rds_file)
valid_zipcodes <- readRDS(valid_zipcodes_path)

# Define valid community boards
valid_community_boards <- c(
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
valid_cb <- data.table(cb = valid_community_boards)

#######################
# Run analyses with consistent seed
zipcode_results <- analyze_invalid_values(cleaned_data, "incident_zip", valid_zipcodes, "delivery_zipcode")

# First, extract the data frames from zipcode_results
zipcode_results_full <- zipcode_results[[1]]  
zipcode_results_sample <- zipcode_results[[2]]

# ZIP CODE CHARTS

# Then, if year_month is not a Date object, convert it by appending "-01"
zipcode_results_full$year_month <- as.Date(paste0(zipcode_results_full$year_month, "-01"))

# Create ggplot for ZIP codes
zip_gg_plot <- create_condition_plot(
  zipcode_results_full,
  "Proportion of Invalid ZIP Codes",
  "Non-conforming Proportion",
  "incident_zip not in USPS database",
  "invalid_fraction"
)

# Extract details from the plot result
zip_trend <- zip_gg_plot$trend_label
zip_mean <- zip_gg_plot$process_mean
zip_title <- zip_gg_plot$title


# Display and save the ggplot
print(gg_plot)

# Access the plot element from the list
file_path <- paste0(chart_dir, "/invalid_zipcodes.pdf")
ggsave(file_path, plot = zip_gg_plot$plot, width = 10, height = 7)

# Delay between charts
cat("\nWaiting 4 seconds between charts...\n")
Sys.sleep(4)

# Create QCC chart for ZIP codes
tryCatch({
  # Convert year_month to Date if it's not already
  if(!inherits(zipcode_results_sample$year_month, "Date")) {
    zipcode_results_sample$year_month <- as.Date(paste0(zipcode_results_sample$year_month, "-01"))
  }
  
  zip_qcc_plot <- qcc(zipcode_results_sample$invalid_zips,  # Count of defects
                      sizes = zipcode_results_sample$total_records,  # Sample sizes
                      type = "p",  # Proportion chart
                      title = "QCC p-chart of Invalid ZIP Codes",
                      xlab = "Year-Month",
                      ylab = "Non-conforming Proportion",
                      labels = format(zipcode_results_sample$year_month, "%Y-%m"))
  
  # Save QCC chart
  pdf(paste0(chart_dir, "/qcc_p_chart_invalid_zipcodes.pdf"), width = 10, height = 7)
  plot(zip_qcc_plot)
  dev.off()
  
  # Add a 4 second delay to let plots render completely
  cat("\nWaiting 4 seconds before continuing to next chart...\n")
  Sys.sleep(4)   
  
}, error = function(e) {
  cat("Error creating ZIP code QCC chart:", e$message, "\n")
})

#######################
community_board_results <- analyze_invalid_values(cleaned_data, "community_board", valid_cb, "cb")

# Extract the data
community_board_full <- community_board_results[[1]]
community_board_sample <- community_board_results[[2]]

# Convert year_month to Date
community_board_full$year_month <- as.Date(paste0(community_board_full$year_month, "-01"))
community_board_sample$year_month <- as.Date(paste0(community_board_sample$year_month, "-01"))

# COMMUNITY BOARD CHARTS
# Create ggplot for community boards
cb_gg_plot <- create_condition_plot(
  community_board_full,
  "Proportion of Invalid Community Boards",
  "Non-conforming Proportion",
  subtitle = "NYC Community Board validation",
  value_field = "invalid_fraction"
)

# Extract details from the plot result
cb_trend <- cb_gg_plot$trend_label
cb_mean <- cb_gg_plot$process_mean
cb_title <- cb_gg_plot$title

# New conditions data frame
new_conditions_df <- data.frame(
  condition = c("invalid_zipcodes", "invalid_community_boards"),
  title = c(zip_title, cb_title),
  trend = c(zip_trend, cb_trend),
  mean = c(zip_mean, cb_mean)
)

# Append new conditions to summary_df
summary_df <- rbind(summary_df, new_conditions_df)

# Print and save the final summary
print(summary_df)
write.csv(summary_df, paste0(chart_dir, "/condition_summaries.csv"), row.names = FALSE)



# Display and save the ggplot

# Access the plot element from the list
file_path <- paste0(chart_dir, "/invalid_community_boards.pdf")
ggsave(file_path, plot = cb_gg_plot$plot, width = 10, height = 7)

print(cb_gg_plot$plot)  # Note the $plot to access the actual plot from the return list

# Delay between charts
cat("\nWaiting 4 seconds between charts...\n")
Sys.sleep(4)

# Create QCC chart for community boards
tryCatch({
  cb_qcc_plot <- qcc(community_board_sample$invalid_zips,  # Use invalid_zips from the sample data
                     sizes = community_board_sample$total_records,  # Use total_records as sizes
                     type = "p", 
                     title = "QCC p-chart of Invalid Community Boards",
                     xlab = "Year-Month",
                     ylab = "Non-conforming Proportion",
                     labels = format(community_board_sample$year_month, "%Y-%m"))
  
  # Save QCC chart
  pdf(paste0(chart_dir, "/qcc_p_chart_invalid_community_boards.pdf"), width = 10, height = 7)
  plot(cb_qcc_plot)
  dev.off()
  
  # Add a 4 second delay to let plots render completely
  cat("\nWaiting 4 seconds before continuing to next chart...\n")
  Sys.sleep(4)   
  
}, error = function(e) {
  cat("Error creating Community Board QCC chart:", e$message, "\n")
})

##########################################################################
# # Compute additional trends and means
# compute_trend_slope <- function(data, value_field) {
#   if (nrow(data) < 2) return(NA)  # Not enough data for trend calculation
#   
#   # Convert year_month to numeric for regression (e.g., number of days since start)
#   data$numeric_date <- as.numeric(as.Date(data$year_month))
#   
#   # Fit a linear model (y = mx + b)
#   model <- lm(get(value_field) ~ numeric_date, data = data)
#   
#   # Extract the slope (m)
#   return(coef(model)[2])
# }
# 
# 
# 
# zip_trend <- compute_trend_slope(zipcode_results_full, "invalid_fraction")  
# cb_trend <- compute_trend_slope(community_board_full, "invalid_fraction")
# 
# zip_mean <- mean(zipcode_results_full$invalid_fraction, na.rm = TRUE)
# cb_mean <- mean(community_board_full$invalid_fraction, na.rm = TRUE)
# 
# # New conditions data frame
# new_conditions_df <- data.frame(
#   condition = c("invalid_zipcodes", "invalid_community_boards"),
#   title = c("Proportion of Invalid ZIP Codes", "Proportion of Invalid Community Boards"),
#   trend = c(zip_trend, cb_trend),
#   mean = c(zip_mean, cb_mean)
# )
# 
# # Append to summary_df
# summary_df <- rbind(summary_df, new_conditions_df)
# 
# # Print summary
# print(summary_df)
# 
# # Write to CSV
# write.csv(summary_df, paste0(chart_dir, "/condition_summaries.csv"), row.names = FALSE)

##########################################################################
# Create a consolidate chart
# Step 1: Create a consolidated data frame
consolidated_data <- data.frame()

# Add each date condition with its identifier
for (condition in names(data_condition_results)[!grepl("_sampled$", names(data_condition_results))]) {
  temp_data <- data_condition_results[[condition]]
  temp_data$condition <- condition
  temp_data$value <- temp_data$fraction  # Ensure consistent column name
  consolidated_data <- rbind(consolidated_data, 
                             temp_data[, c("year_month", "condition", "value")])
}

# Add zip code data
zipcode_data <- data.frame(
  year_month = zipcode_results_full$year_month,
  condition = "invalid_zipcodes",
  value = zipcode_results_full$invalid_fraction
)
consolidated_data <- rbind(consolidated_data, zipcode_data)

# Add community board data
cb_data <- data.frame(
  year_month = community_board_full$year_month,
  condition = "invalid_community_boards",
  value = community_board_full$invalid_fraction
)
consolidated_data <- rbind(consolidated_data, cb_data)

# Step 2: Normalize the data
# Normalize so all metrics start at 1.0, handling zero first values
normalized_data <- consolidated_data %>%
  group_by(condition) %>%
  arrange(year_month) %>%
  mutate(
    first_value = first(value),
    # Handle the special case where first value is zero
    normalized_value = case_when(
      first_value == 0 ~ ifelse(value == 0, 1.0, 1.0 + value),  # Start at 1.0, increase for non-zero values
      TRUE ~ value / first_value  # Standard normalization for non-zero first values
    )
  ) %>%
  ungroup()

# Step 3: Create the consolidated plot
# Filter out NAs before plotting
normalized_data_clean <- normalized_data %>%
  filter(!is.na(normalized_value))

# Create the consolidated plot with a better color palette
combined_plot <- ggplot(normalized_data, aes(x = year_month, y = normalized_value, color = condition)) +
  geom_line(linewidth = 1.2) +  # Slightly thicker lines for better visibility
  geom_point(size = 2.5) +  # Slightly larger points
  labs(
    title = "",
    x = "",
    y = "Normalized Value (1.0 = Initial Value)",
    color = NULL  # Removes the legend title
  ) +
  # Add a horizontal reference line at y=1
  geom_hline(yintercept = 1.0, linetype = "dashed", color = "gray50", alpha = 0.7) +
  theme_minimal() +
  theme(
    legend.position = c(0.5, 0.945),  # Centered at the top inside the plot area
    legend.justification = "center",  # Ensures proper centering
    legend.background = element_rect(fill = alpha("white", 0.6)),  # Semi-transparent white background
    legend.key = element_rect(fill = NA),  # Transparent legend keys
    legend.direction = "horizontal",  # Arranges legend items in a row
    panel.grid.major = element_line(color = "white", linewidth = 0.5),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "gray95"),
    axis.text.x = element_text(angle = 0, hjust = 1) 
  ) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
  # Replace the "Set1" palette with a more contrasting palette
  scale_color_brewer(palette = "Dark2") + 
  guides(color = guide_legend(nrow = 2))  # Force legend to use two rows for better readability

# Display the plot
print(combined_plot)

# Save the plot
ggsave(paste0(chart_dir, "/consolidated_quality_metrics.pdf"), 
       plot = combined_plot, width = 12, height = 8)

# Delay between charts
cat("\nWaiting 4 seconds between charts...\n")
Sys.sleep(4)

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