################################################################################
################################################################################
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
  "zoo",
  "fasttime"
)

# Check and install missing packages
missing_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(missing_packages)) {
  install.packages(missing_packages)
  cat("📦 Installed missing packages:", paste(missing_packages, collapse = ", "), "\n")
}

# Load the required packages
lapply(required_packages, library, character.only = TRUE)

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
base_dir <- file.path(working_dir, "datacleaningproject", "nyc311clean",  
                      "data_quality_over_time" )

# Define the path for the main data file (CSV file)
data_dir <- file.path(base_dir, "data")

# Define the path for the charts
chart_dir <- file.path(base_dir, "charts")

######### Commence directing console output to the file ##########
# Define the path for the console output
console_dir <- file.path(base_dir, "console_output")
console_output_file <- file.path(console_dir, 
                                 "quality_over_time_console_output.txt")

#sink(console_output_file)


cat("\nExecution begins at:", formattedStartTime)

################################################################################
#main_data_file <- "2-year_311SR_01-01-2023_thru_12_31_2024_AS_OF_03-18-2025.csv"
#main_data_file <- "5-year_311SR_01-01-2020_thru_12-31-2024_AS_OF_02-27-2025.csv"
#main_data_file <- "10-year_311SR_01_01_2015_thru_12_31_2014_AS_OF_03-01-2025.csv"
main_data_file <- "15-year_311SR_01-01-2010_thru_12-31-2024_AS_OF_03-17-2025.csv"

# Extract the 10-character date after "AS_OF_"
max_closed_date <- sub(".*AS_OF_([0-9-]+)\\.csv$", "\\1", main_data_file)

# Convert to POSIXct format
max_closed_date <- as.POSIXct(max_closed_date, format = "%m-%d-%Y", 
                              tz = "America/New_York") + (23*3600 + 59*60 + 59)

################################################################################
# Define the path to the directory containing your function scripts
functions_path <- file.path(base_dir, "code", "functions")

########## Source the function files ##########
# Get all .R files in the "functions" sub-directory
function_files <- list.files(functions_path, pattern = "\\.R$", 
                             full.names = TRUE)

# Source each file with error handling and message logging
lapply(function_files, function(file) {
  tryCatch({
    source(file)
#    message("Successfully sourced: ", file)
  }, error = function(e) {
    message("Error sourcing: ", file, " - ", e$message)
  })
})

################################################################################
analyze_invalid_values <- function(data, 
                                   field_name, 
                                   valid_values_list, 
                                   valid_field_name, 
                                   seed = 42) {
  
  # Step 1: Remove NAs from the specified field
  data_no_na <- data[ !is.na(data[[field_name]]) & data[[field_name]] != "", ]
  
  # Step 2: Add boolean column for valid values
  data_no_na$field_validity <- data_no_na[[field_name]] %in% 
    valid_values_list[[valid_field_name]]
  
  # Step 3: Find minimum count per year_month to use as sample size
  total_per_month <- data_no_na[, .N, by = year_month]
  min_count <- min(total_per_month$N)
  
  # # Step 4: Create the sampled dataset
  # sampled_data <- data.table()
  # 
  # for (month in unique(data_no_na$year_month)) {
  #   month_data <- data_no_na[year_month == month]
  #   month_sample <- month_data[sample(.N, min_count, replace = FALSE)]
  #   sampled_data <- rbind(sampled_data, month_sample)
  # }
  # 
  # sampled_data$zip_validity <- sampled_data[[field_name]] %in% 
  #   valid_values_list[[valid_field_name]]
  
  # Step 5: Standardized column names for consistency
  full_summary <- data_no_na[, .(
    count = sum(!field_validity),
    N = .N,
    fraction = sum(!field_validity) / .N
  ), by = year_month]  # year_month is already grouped, no need to repeat
  
  # sampled_summary <- sampled_data[, .(
  #   count = sum(!field_validity),
  #   N = .N,
  #   fraction = sum(!field_validity) / .N
  # ), by = year_month]
  # 
  # Add dataset column to match structure
  full_summary[, dataset := field_name]
#  sampled_summary[, dataset := paste0(field_name, "_sampled")]
  
  return(list(full_summary
#              , sampled_summary)
         ))
}

################################################################################
create_condition_plot <- function(
    data, 
    title = NULL, 
    y_label = NULL, 
    subtitle = NULL, 
    value_field = "fraction") {
  
    setDT(data)  # Convert to data.table
  
    data[, dataset := title]  # Add title as an identifier column
    all_results[[title]] <<- data  # Store in global list

  # Convert year_month to Date if it's not already 
  if(!inherits(data$year_month, "Date")) {
    
    # Check if it's in YYYY-MM format
    if(is.character(data$year_month) && 
       all(grepl("^\\d{4}-\\d{2}$", data$year_month[!is.na(data$year_month)]))) {
      # Add day to make it a full date
      data$year_month <- as.Date(paste0(data$year_month, "-01"))
    } else {
      # Try direct conversion if it's in another format
      tryCatch({
        data$year_month <- as.Date(data$year_month)
      }, error = function(e) {
        stop("Could not convert year_month to Date format.")
      })
    }
  }
  
  # Calculate mean and standard deviation for the three-sigma limits
  process_mean <- mean(data[[value_field]])
  process_sd <- sd(data[[value_field]])
  
  # Calculate the total timespan of the data in days
  total_days <- as.numeric(max(data$year_month) - min(data$year_month))
  
  # Define a list to store results
  all_results <- list()
  
  # Modify `data` by adding a new column for dataset identification
  # Use `title` instead of a hardcoded dataset name
  data[, dataset := title]
  
  # Store in list using `title` as the key
  all_results[[title]] <- data
  
  # Fit linear model (using dynamic field name)
  data$date_numeric <- as.numeric(data$year_month)
  lm_formula <- as.formula(paste0(value_field, " ~ date_numeric"))
  lm_model <- lm(lm_formula, data = data)
  
  # Extract R² value from your existing model
  r_squared <- round(summary(lm_model)$r.squared, 3)
  
  # Calculate total percentage change over the entire period
  slope <- coef(lm_model)[2]  # Change per day
  total_change <- slope * total_days
  total_percent_change <- (total_change / process_mean) * 100
  
  # Format the trend label with + or - sign and include R²
  trend_label <- paste0(format(round(total_percent_change, 1), 
                              nsmall = 1), "% (R²: ", r_squared, ")")
  
  # Compute a mid-range Y position
  # Or alternatively, use a more dynamic approach that accounts for the range
  y_trend_position <- max(data[[value_field]], na.rm = TRUE) - 
    (diff(range(data[[value_field]], na.rm = TRUE)) * 0.1)  # 10% below max
  
  # Compute a mid-range X position
  x_trend_position <- median(data$year_month)
  
  # Create the plot (using dynamic mapping)
  p <- ggplot(data, aes(x = year_month, y = .data[[value_field]])) +
    
    # Add the points and line
    geom_line(color = "steelblue2", linewidth = 0.5) +
    geom_point(size = 2.75, color = "steelblue4", shape = 18) +
    
    # Add the mean and sigma lines
    geom_hline(yintercept = process_mean + 3*process_sd, color = "red", 
               linetype = "dotted", linewidth = 0.7) +
    geom_hline(yintercept = max(0, process_mean - 3*process_sd), color = "red", 
               linetype = "dotted", linewidth = 0.7) +
    geom_hline(aes(yintercept = process_mean), linetype = "dashed", 
               color = "darkred", linewidth = 0.8) +
    
    # Add text labels for the sigma lines
    annotate("text", x = data$year_month[2], y = process_mean + 3*process_sd, 
             label = paste("+3σ:", format(round(process_mean + 3*process_sd, 4), 
                                          nsmall = 4)), 
             vjust = -0.5, hjust = 0, color = "red", size = 3) +
    
    annotate("text", x = data$year_month[2], y = max(0, process_mean - 3*process_sd), 
             label = paste("-3σ:", 
             format(round(max(0, process_mean - 3*process_sd), 4), 
             nsmall = 4)), vjust = 1.5, hjust = 0, color = "red", size = 3) +
    
    # Add linear trend line with custom confidence band
    geom_smooth(method = "lm", 
                se = TRUE, 
                color = "gray45", 
                linewidth = 0.8, 
                fill = "gray81", 
                alpha = 0.3) +
    
    # Add trend annotation (using dynamic calculation for y position)
    annotate("text", 
             x = x_trend_position, 
             y = y_trend_position,  
             label = paste("Trend:", trend_label),
             hjust = 0.5, vjust = -0.25,  
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
      panel.border = element_rect(color = "black", fill = NA, 
                    linewidth = 0.75),  # Black border around plot area
      axis.text.x = element_text(angle = 10, hjust = 1, color = "black"),  
      panel.grid.major = element_line(color = "gray88", linewidth = 0.5),  
      panel.grid.minor = element_blank(),
      plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"),  
      panel.background = element_rect(fill = "gray99", color = NA),  
      plot.background = element_rect(fill = "gray89", color = NA),   
      plot.caption = element_text(hjust = 1, size = 9, color = "black"),
      axis.title.y = element_text(margin = margin(r = 10, l = 0), 
                                  color = "black"),
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

################################################################################
# For analyze_data_conditions function:
analyze_data_conditions <- function(cleaned_data) {
  
  # Calculate the minimum monthly count to use as sample size
  total_per_month <- cleaned_data[, .N, by = year_month]
  min_count <- min(total_per_month$N)

  # # Take random samples of equal size from each month
  # sampled_data <- data.table()
  # 
  # # Set seed for reproducibility
  # set.seed(42)
  
  # Loop through each month
  for (month in unique(cleaned_data$year_month)) {
    # Get data for this month
    month_data <- cleaned_data[year_month == month]
    
    # Take random sample of size min_count from this month
    month_sample <- month_data[sample(.N, min_count, replace = FALSE)]
    
    # # Add to our sampled dataset
    # sampled_data <- rbind(sampled_data, month_sample)
  }
  
  # Create indicators for all conditions (in both sampled and full data)
  # # First for sampled data
  # sampled_data[, midnight_closing := ifelse(!is.na(closed_date), format(closed_date, "%H:%M:%S") == "00:00:00", FALSE)]
  # sampled_data[, resolution_before_creation := ifelse(!is.na(resolution_action_updated_date) & !is.na(created_date), 
  #                                                     resolution_action_updated_date < created_date, FALSE)]
  # sampled_data[, status_not_closed_with_closed_date := ifelse(!is.na(closed_date) & !is.na(status), 
  #                                                             !is.na(closed_date) & status != "CLOSED", FALSE)]
  # sampled_data[, status_closed_no_closed_date := ifelse(!is.na(status), 
  #                                                       status == "CLOSED" & is.na(closed_date), FALSE)]
  # sampled_data[, late_resolution_update := ifelse(!is.na(closed_date) & !is.na(resolution_action_updated_date), 
  #                                                 as.numeric(difftime(resolution_action_updated_date, closed_date, units = "days")) > 90, FALSE)]
  # sampled_data[, negative_or_zero_duration := ifelse(!is.na(closed_date) & !is.na(created_date), 
  #                                                    closed_date <= created_date, FALSE)]
  # 
  # # Now for full data
  cleaned_data[, midnight_closing := ifelse(!is.na(closed_date), format(closed_date, "%H:%M:%S") == "00:00:00", FALSE)]
  cleaned_data[, resolution_before_creation := ifelse(!is.na(resolution_action_updated_date) & !is.na(created_date), 
                                                      resolution_action_updated_date < created_date, FALSE)]
  cleaned_data[, status_not_closed_with_closed_date := ifelse(!is.na(closed_date) & !is.na(status), 
                                                              !is.na(closed_date) & status != "CLOSED", FALSE)]
  cleaned_data[, status_closed_no_closed_date := ifelse(!is.na(status), 
                                                        status == "CLOSED" & is.na(closed_date), FALSE)]
  cleaned_data[, late_resolution_update := ifelse(!is.na(closed_date) & !is.na(resolution_action_updated_date), 
                                                  as.numeric(difftime(resolution_action_updated_date, closed_date, units = "days")) >90, FALSE)]
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
    # # 1. For sampled data (for QCC and QIC)
    # sampled_result <- sampled_data[, .(
    #   count = sum(get(condition), na.rm = TRUE),
    #   N = .N,
    #   fraction = sum(get(condition), na.rm = TRUE) / .N
    # ), by = year_month]
    # 
    # 2. For full data (for ggplot)
    full_result <- cleaned_data[, .(
      count = sum(get(condition), na.rm = TRUE),
      N = .N,
      fraction = sum(get(condition), na.rm = TRUE) / .N
    ), by = year_month]
    
    # Make sure the data is in chronological order
#    sampled_result <- sampled_result[order(year_month)]
    full_result <- full_result[order(year_month)]
    
    # Convert year_month from character to Date for proper plotting
#    sampled_result[, year_month := as.Date(paste0(year_month, "-01"))]
    full_result[, year_month := as.Date(paste0(year_month, "-01"))]
    
    # Store in results list
#    results[[paste0(condition, "_sampled")]] <- sampled_result
    results[[condition]] <- full_result
  }
  
  return(results)
}

################################################################################
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

################################################################################

#========= Main Execution =========

#####################
# Process 311 data
cat("\nReading in 311 Service Request data... \n")
main_data_path <- file.path(data_dir, main_data_file)

rds_file <- gsub("\\.csv$", ".rds", main_data_file)
rds_path <- file.path(data_dir, rds_file)

#####################
# Read only the columns you need from raw data with optimized settings
raw_data <- fread(
  main_data_path, 
  select = c("Unique Key", 
             "Created Date", 
             "Closed Date", 
             "Incident Zip", 
             "Status", 
             "Resolution Action Updated Date", 
             "Community Board"),
  colClasses = c("Created Date" = "character",
                 "Closed Date" = "character",
                 "Resolution Action Updated Date" = "character",
                 "Status" = "character",
                 "Incident Zip" = "character",
                 "Community Board" = "character"),
  nThread = parallel::detectCores() - 1,  # Use all cores except one
  check.names = FALSE,  # Skip name validation
  strip.white = TRUE   # Remove leading/trailing whitespace
#  showProgress = FALSE  # Disable progress bar for slightly more speed
)

cat("\nData set row count:", format(nrow(raw_data), big.mark=","))

#####################
# Clean column names
cleaned_data <- modify_column_names(raw_data)
cat("\nColumn names standardized")

#####################
# Convert only status and community_board columns to uppercase
columns_to_uppercase <- c("status", "community_board")

for (col in columns_to_uppercase) {
  if (is.character(cleaned_data[[col]])) {
    cleaned_data[[col]] <- toupper(cleaned_data[[col]])
}
}
cat("\nColumn names converted to uppercase")

#####################
# Standardize date formats

# Ensure only existing date columns are processed
valid_date_columns <- intersect(date_columns, names(cleaned_data))

if (length(valid_date_columns) > 0) {
  cat("\nStandardizing date formats...")
  cleaned_data[, (valid_date_columns) := lapply(.SD, function(x) {
    as.POSIXct(x, format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC")
  }), .SDcols = valid_date_columns]
#  cat("Date standardization complete\n")
} else {
  cat("Skipping date standardization: No matching date columns found.\n")
}

cat("\nCreated Dates span from", format(min(cleaned_data$created_date), 
    "%Y-%m-%d %H:%M:%S"), "to", format(max(cleaned_data$created_date), 
                                       "%Y-%m-%d %H:%M:%S"))

#####################  
# Adjust February 29 dates inline, only for leap years since 2000
leap_years <- c(2000, 2004, 2008, 2012, 2016, 2020, 2024)
 
 # Identify which date columns exist in the dataset
 valid_date_columns <- intersect(date_columns, names(cleaned_data))
 
 if (length(valid_date_columns) > 0) {
   # Create empty vector to collect all unique keys with Feb 29 dates
   all_feb29_unique_keys <- c()
   
   # Process each date column
   for (col in valid_date_columns) {
     if (col %in% names(cleaned_data)) {

       # Identify rows where the date occurs on February 29 in leap years
       feb_29_indices <- which(!is.na(cleaned_data[[col]]) & 
                              format(cleaned_data[[col]], "%m-%d") == 
                              "02-29" & as.numeric(format(cleaned_data[[col]], 
                              "%Y")) %in% leap_years)
       
       feb29_unique_keys <- cleaned_data$unique_key[feb_29_indices]
       all_feb29_unique_keys <- unique(c(all_feb29_unique_keys, 
                                         feb29_unique_keys))
                        
       num_29Feb_rows <- length(feb_29_indices)
       if (num_29Feb_rows > 0) {

         # Replace February 29 dates with February 28 of the same year
         cleaned_data[[col]][feb_29_indices] <- as.POSIXct(
           paste(
             format(cleaned_data[[col]][feb_29_indices], "%Y-02-28"),
             format(cleaned_data[[col]][feb_29_indices], "%H:%M:%S")
           ),
           tz = "UTC"
         )
         
         cat("\nAdjusted", num_29Feb_rows, "rows in the", col, 
             "column from 29 February to 28 February")
       } else {
         cat("\nNo Feb 29 dates found in leap years for column", col, "\n")
       }
     }
   }
   
   if (length(all_feb29_unique_keys) > 0) {
     cat("\nTotal unique records with February 29 dates:", 
         format(length(all_feb29_unique_keys), big.mark = ","), "\n")
   } else {
     cat("\nNo Feb 29 dates in leap years found, skipping adjustments\n")
   }
 } else {
   cat("Skipping Feb 29 adjustments: No matching date columns found.\n")
 }
 
#####################
# Ensure cleaned_data is a data.table
setDT(cleaned_data)  
  
# Remove from memory to free up RAM
rm(raw_data)
gc()  

#########################################################################
# Extract year from your date columns and create a count by year
yearly_counts <- cleaned_data %>%
  # Extract year from created_date
  mutate(year = as.numeric(format(created_date, "%Y"))) %>%
  # Count records by year
  count(year) %>%
  # Ensure we have year as numeric for the trend line
  mutate(year_numeric = as.numeric(year))

# Add the projected value for 2025
yearly_counts <- yearly_counts %>%
  bind_rows(tibble(year = 2025, n = 3621618, year_numeric = 2025)) %>%
  mutate(status = ifelse(year == 2025, "Projected", "Actual"))

# Fit linear model to calculate trend and R²
lm_model <- lm(n ~ year_numeric, data = yearly_counts)
r_squared <- round(summary(lm_model)$r.squared, 3)

# Calculate total percentage change over the entire period
first_year_count <- yearly_counts$n[1]
last_year_count <- yearly_counts$n[nrow(yearly_counts)]
total_change <- last_year_count - first_year_count
process_mean <- mean(yearly_counts$n)
total_percent_change <- (total_change / process_mean) * 100

# Format the trend label with + or - sign and include R²
trend_label <- ifelse(total_percent_change >= 0, 
                      paste0("+", format(round(total_percent_change, 1), nsmall = 1), 
                             "%   (R²: ", r_squared, ")"),
                      paste0(format(round(total_percent_change, 1), nsmall = 1), 
                             "%   (R²: ", r_squared, ")"))

# Create the bar chart with trend line
growth_chart <- ggplot(yearly_counts, aes(x = year, y = n, fill = status)) +
  # Add bars with different colors for actual vs projected
  geom_col(alpha = 0.7) +
  # Custom fill colors
  scale_fill_manual(values = c("Actual" = "dodgerblue4", "Projected" = "ivory4")) +
  # Add trend line
  geom_smooth(aes(x = year_numeric, y = n), method = "lm", 
              color = "darkgoldenrod1", se = FALSE, linetype = "twodash", linewidth = 1.2) +
  # Add count labels on top of bars
  geom_text(aes(label = paste0(format(round(n/1e6, 2), nsmall = 2))), 
            vjust = -0.5, size = 3.5) +  
  # Add trend annotation
  annotate("text", 
           x = mean(yearly_counts$year), 
           y = max(yearly_counts$n) * 0.85,  
           label = paste("Trend:", trend_label ),
           hjust = 0.95, vjust = -1.9,  
           color = "gray30", fontface = "bold", size = 5.5) +
  # Formatting
  labs(
    title = "SR Count by Year (w/trend)",
    subtitle = NULL,
    x = NULL,
    y = NULL,
    caption = paste("Mean count:", format(round(process_mean), big.mark = ",")),
    fill = "Data Type" # Legend title
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    # Show major grid lines and set them to white
    panel.grid.major.y = element_line(color = "white", linewidth = 0.8),
    panel.grid.major.x = element_line(color = "white", linewidth = 0.8),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 0),
    axis.ticks.length = unit(0.3, "cm"),  # Adjust tick mark length
    panel.background = element_rect(fill = "gray97", color = "gray97")
  ) +
  # Format y-axis with commas for thousands
  scale_y_continuous(labels = scales::comma)

# Print the chart
print(growth_chart)

# Add a 3 second delay to let plots render completely
cat("\nWaiting 3 seconds before continuing to next chart...\n")
Sys.sleep(3)   

file_path <- paste0(chart_dir, "/year_over_year.pdf")

# Save the last printed plot
ggsave(file_path, growth_chart, width = 13, height = 8.5)

################################################################################
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

# Get frequency count of ZIP codes in the incident dataset
zip_frequencies <- table(cleaned_data$incident_zip, useNA = "no")

# Get sorted list of ZIP codes that are both valid and appear in the data
valid_zipcodes <- zipcode_data$delivery_zipcode
common_zipcodes <- names(sort(zip_frequencies[names(zip_frequencies) 
                            %in% valid_zipcodes], decreasing = TRUE))

# Add any valid ZIP codes that don't appear in the incident data
remaining_zipcodes <- setdiff(valid_zipcodes, common_zipcodes)

# Create the optimized zipcode list
zipcode_data <- data.table(
  delivery_zipcode = c(common_zipcodes, remaining_zipcodes)
)

################################################################################      
# Extract year-month for grouping
cleaned_data[, year_month := format(created_date, "%Y-%m")]

################################################################################
# Analyze all conditions
data_condition_results <- analyze_data_conditions(cleaned_data)

# Define updated subtitles with your preferred descriptions
subtitles <- list(
  midnight_closing = "closed_date occurs exactly at 00:00:00",
  resolution_before_creation = 
    "resolution_action_updated_date is before created_date",
  status_not_closed_with_closed_date = 
    "closed_date exists but status is not CLOSED",
  status_closed_no_closed_date = 
    "status is CLOSED but there is no closed_date",
  late_resolution_update = 
    "resolution_action_updated_date is >90 days after closed_date",
  negative_or_zero_duration = 
    "closed_date is <= created_date (infeasible duration)"
)

# Filter just the original (non-sampled) condition names
original_conditions <- names(data_condition_results)[!grepl("_sampled$", 
                              names(data_condition_results))]

# Initialize a list to store summaries
condition_summary <- list()

# Global list
all_results <- list()

# Loop through only the original conditions
for (condition in original_conditions) {
  # Get both full and sampled data
  full_data <- data_condition_results[[condition]]
#  sampled_data <- data_condition_results[[paste0(condition, "_sampled")]]
  
#  cat("\n\n======= CONDITION:", condition, "=======\n")
  
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
  ggsave(file_path, plot = last_plot(), width = 13, height = 8.5)
  
  # Add a 3 second delay to let plots render completely
  cat("\nWaiting 3 seconds before continuing to next chart...\n")
  Sys.sleep(3)   
  
#   # 2. QCC chart (using SAMPLED data)
#   # Create a file name using the condition
#   file_name <- paste0(chart_dir, "/qcc_p_chart_", condition, ".pdf")
# 
#   # Define data and variables explicitly before plotting
#   count_data <- sampled_data$count  # Make sure this is the correct column for all conditions
#   sample_sizes <- sampled_data$N
#   chart_title <- paste("QCC p-chart of", gsub("_", " ", tools::toTitleCase(condition)))
#   x_labels <- format(sampled_data$year_month, "%Y-%m")
# 
#   # First create and display the plot in RStudio
#   tryCatch({
#     qcc_plot <- qcc(count_data,
#                     sizes = sample_sizes,
#                     type = "p",
#                     title = chart_title,
#                     xlab = "Year-Month",
#                     ylab = "Non-conforming Proportion",
#                     labels = x_labels)
#   }, error = function(e) {
#     cat("Error creating QCC chart:", e$message, "\n")
#   })
# 
#   # Then save the displayed plot to the file
#   pdf(file_name, width = 10, height = 7)
#   # Use the explicitly defined title again
#   plot(qcc_plot, title = chart_title)
#   dev.off()
# 
#   # Add a 4 second delay to let plots render completely
# #  cat("\nWaiting 4 seconds before continuing to next chart...\n")
#   Sys.sleep(4)
  
}

# Create a data frame from the summaries
summary_df <- data.frame(
  condition = names(condition_summary),
  title = sapply(condition_summary, function(x) x$title),
  trend = sapply(condition_summary, function(x) x$trend),
  mean = sapply(condition_summary, function(x) x$mean)
)

################################################################################

########## Zip Code Evaluation ##########

################################################################################
# Run analyses with consistent seed
zipcode_results <- analyze_invalid_values(cleaned_data, "incident_zip", 
                                          zipcode_data, "delivery_zipcode")

# First, convert year_month to Date objects
zipcode_results[[1]]$year_month <- 
  as.Date(paste0(zipcode_results[[1]]$year_month, "-01"))
# zipcode_results[[2]]$year_month <- 
#   as.Date(paste0(zipcode_results[[2]]$year_month, "-01"))

# First, extract the data frames from zipcode_results
zipcode_results_full <- zipcode_results[[1]] %>%
  arrange(year_month)

print(head(zipcode_results_full))

# zipcode_results_sample <- zipcode_results[[2]] %>%
#   arrange(year_month)

#######################
# Create ggplot for ZIP codes
zip_gg_plot <- create_condition_plot(
                  data = zipcode_results_full,
                  title = "Proportion of Invalid ZIP Codes",
                  y_label = "Non-conforming Proportion",
                  subtitle = "incident_zip not in USPS database",
                  value_field = "fraction"
)

# Display and save the ggplot
print(zip_gg_plot)

# Access the plot element from the list
file_path <- paste0(chart_dir, "/invalid_zipcodes.pdf")
ggsave(file_path, plot = zip_gg_plot$plot, width = 13, height = 8.5)

# Extract details from the plot result
zip_trend <- zip_gg_plot$trend_label
zip_mean <- zip_gg_plot$process_mean
zip_title <- zip_gg_plot$title

# Delay between charts
cat("\nWaiting 3 seconds between charts...\n")
Sys.sleep(3)

#######################
# # Create QCC chart for ZIP codes
# tryCatch({
# 
#     # Define variables explicitly
#   zip_count_data <- zipcode_results_sample$invalid_zips
#   zip_sample_sizes <- zipcode_results_sample$total_records
#   zip_chart_title <- "QCC p-chart of Invalid ZIP Codes"
#   zip_labels <- format(zipcode_results_sample$year_month, "%Y-%m")
#   
#   zip_qcc_plot <- qcc(zip_count_data,  # Count of defects
#                       sizes = zip_sample_sizes,  # Sample sizes
#                       type = "p",  # Proportion chart
#                       title = zip_chart_title,
#                       xlab = "Year-Month",
#                       ylab = "Non-conforming Proportion",
#                       labels = zip_labels)
#   
#   # Save QCC chart - with explicit title
#   pdf(paste0(chart_dir, "/qcc_p_chart_invalid_zipcodes.pdf"), width = 10, height = 7)
#   plot(zip_qcc_plot, title = zip_chart_title)
#   dev.off()
#   
#   # Add a 4 second delay to let plots render completely
# #  cat("\nWaiting 4 seconds before continuing to next chart...\n")
#   Sys.sleep(4)   
#   
# }, error = function(e) {
#   cat("Error creating ZIP code QCC chart:", e$message, "\n")
# })

################################################################################

########## Community Board Evaluation ##########

################################################################################
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
community_board_results <- analyze_invalid_values(
                              cleaned_data, "community_board", 
                              valid_cb, "cb")

# Extract the data
community_board_full <- community_board_results[[1]]
#community_board_sample <- community_board_results[[2]]

# Convert year_month to Date
community_board_full$year_month <- as.Date(paste0(community_board_full$year_month, "-01"))
#community_board_sample$year_month <- as.Date(paste0(community_board_sample$year_month, "-01"))

# Arrange by year_month from oldest to newest
community_board_full <- community_board_full %>% arrange(year_month)
#community_board_sample <- community_board_sample %>% arrange(year_month)

#######################
# Create ggplot for community boards
cb_gg_plot <- create_condition_plot(
  data = community_board_full,
  title = "Proportion of Invalid Community Boards",
  y_label = "Non-conforming Proportion",
  subtitle = "Community Board does not exist",
  value_field = "fraction"
)

# Display and save the ggplot
file_path <- paste0(chart_dir, "/invalid_community_boards.pdf")
ggsave(file_path, plot = cb_gg_plot$plot, width = 13, height = 8.5)

print(cb_gg_plot$plot)  # Note the $plot to access the plot from the return list

# Delay between charts
cat("\nWaiting 3 seconds between charts...\n")
Sys.sleep(3)

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
write.csv(summary_df, paste0(chart_dir, "/condition_summaries.csv"), row.names = FALSE)

#######################
# # Create QCC chart for community boards
# tryCatch({
#   # Define variables explicitly
#   cb_count_data <- community_board_sample$invalid_zips
#   cb_sample_sizes <- community_board_sample$total_records
#   cb_chart_title <- "QCC p-chart of Invalid Community Boards"
#   cb_labels <- format(community_board_sample$year_month, "%Y-%m")
#   
#   cb_qcc_plot <- qcc(cb_count_data,  # Use invalid_zips from the sample data
#                      sizes = cb_sample_sizes,  # Use total_records as sizes
#                      type = "p", 
#                      title = cb_chart_title,
#                      xlab = "Year-Month",
#                      ylab = "Non-conforming Proportion",
#                      labels = cb_labels)
#   
#   # Save QCC chart - with explicit title
#   pdf(paste0(chart_dir, "/qcc_p_chart_invalid_community_boards.pdf"), width = 10, height = 7)
#   plot(cb_qcc_plot, title = cb_chart_title)
#   dev.off()
#   
#   # Add a 3 second delay to let plots render completely
# #  cat("\nWaiting 3 seconds before continuing to next chart...\n")
#   Sys.sleep(3)   
#   
# }, error = function(e) {
#   cat("Error creating Community Board QCC chart:", e$message, "\n")
# })

################################################################################
# Combine all datasets into one table for use in time_series_analysis program
final_results <- rbindlist(all_results, use.names = TRUE, fill = TRUE)

# Define file path
final_results_file_path <- file.path(data_dir, "condition_plot_results.csv")

# Write to CSV
fwrite(final_results, final_results_file_path)

################################################################################
# Create a consolidated, normalized chart

#######################
# Step 1: Create a consolidated data frame
consolidated_data <- data.frame()

# Add each date condition with its identifier
for (condition in names(data_condition_results)[!grepl("_sampled$", 
                                             names(data_condition_results))]) {
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
  value = zipcode_results_full$fraction
)
consolidated_data <- rbind(consolidated_data, zipcode_data)

# Add community board data
cb_data <- data.frame(
  year_month = community_board_full$year_month,
  condition = "invalid_community_boards",
  value = community_board_full$fraction
)
consolidated_data <- rbind(consolidated_data, cb_data)

#######################
# Step 2: Normalize the data
normalized_data <- consolidated_data %>%
  # Filter for only years 2023-2024
  filter(year_month >= as.Date("2023-01-01") & year_month <= 
           as.Date("2024-12-31")) %>% group_by(condition) %>%
  mutate(
    first_value = first(value),
    normalized_value = (value / first(value))
  ) %>%
  ungroup()

# Cap normalized values to a maximum of 3
normalized_data <- normalized_data %>%
  mutate(normalized_value = pmin(normalized_value, 3))

# Filter out the problematic condition
normalized_data <- normalized_data %>%
  filter(condition != "status_closed_no_closed_date")

#######################
# Step 3: Create the consolidated plot

# Create the dynamic title components
year_range <- normalized_data %>%
  summarize(
    min_year = min(year(year_month)),
    max_year = max(year(year_month))
  )

num_years <- year_range$max_year - year_range$min_year + 1

# Create the consolidated plot with a better color palette
combined_plot <- ggplot(normalized_data, aes(x = year_month, 
                                     y = normalized_value, color = condition)) +
  geom_line(linewidth = 1.2) +  # Slightly thicker lines for better visibility
  geom_point(size = 2.5) +  # Slightly larger points
  labs(
    title = paste0(num_years, "-year view (", 
                   year_range$min_year, "-", year_range$max_year, 
                   ") All Anomalies"),
    x = "Date",
    y = "Normalized Value",
    color = NULL
  ) +
  # Add a horizontal reference line at y=1
  geom_hline(yintercept = 1.0, linetype = "dashed", color = "gray40",  
             linewidth = 0.9, alpha = 0.7) +
  theme_minimal() +
  theme(
    legend.position = "top",  # Use a named position 
    legend.position.inside = c(.354, 0.057),  # parameter for exact positioning
    legend.justification = "left",  # Ensures proper centering
    legend.background = element_rect(fill = alpha("white", 0.6)),  # Semi-trans
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
  guides(color = guide_legend(nrow = 2))  # Force legend to use two rows

# Display the plot
print(combined_plot)

# Save the plot
ggsave(paste0(chart_dir, "/consolidated_quality_metrics.pdf"), 
       plot = combined_plot, width = 13, height = 8.5)

# Delay between charts
cat("\nWaiting 3 seconds between charts...\n") 
Sys.sleep(3)

################################################################################
# Store the program end time and calculate the duration
programStop <- as.POSIXct(Sys.time())
formatted_end_time <- format(programStop, "%Y-%m-%d %H:%M:%S")

# Calculate the duration of the program (in seconds)
duration_seconds <- as.numeric(difftime(programStop, programStart,
                                        units = "secs"))

# Convert the duration to a formatted string (hours, minutes, and seconds)
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
#sink()

cat("\n\n*****END OF PROGRAM*****\n")
cat("\n📅 Execution ends at:", formatted_end_time, "\n")
cat("\n⏱️ Program run-time:", duration_string, "\n")

################################################################################