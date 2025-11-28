# -------------------------------------------------------------
# 📦 INSTALL AND LOAD REQUIRED PACKAGES
# -------------------------------------------------------------
required_packages <- c(
  "ggplot2", 
  "scales", 
  "dplyr", 
  "zoo", 
  "ggpmisc", 
  "lubridate", 
  "data.table",
  "renv", 
  "sf", 
  "stringdist", 
  "styler", 
  "tidyverse", 
  "rlang", 
  "httr"
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
# 📁 Set Working Directory for the Project
# -------------------------------------------------------------
# Set working directory to the location of the initialization script
setwd("C:/Users/David/OneDrive/Documents/datacleaningproject/nyc311clean/code")
# 
# # Source the initialization.R script.
#source("run_this_program_first.R")

#########################################################################
rm(list = ls())

programStart <- as.POSIXct(Sys.time())
formattedStartTime <- format(programStart, "%Y-%m-%d %H:%M:%S")

#########################################################################
# Set path for the data file
main_data_file <- "5-year_311SR_01-01-2020_thru_12-31-2024_AS_OF_09-23-2025.csv"
#main_data_file <- "311_Service_Requests_from_2022-2023_AS_OF_09-15-2024.csv"
#main_data_file <- "smaller_test_data.csv"
#main_data_file <- "extra_small.csv"

#########################################################################
cat("\n***** Program initialization *****")
# Create the sub-directories used during program execution.
# Get the current working directory
working_dir <- getwd()

# Set the base directory under the working directory
base_dir <- working_dir

# Define the path for the main data file (CSV file)
data_file <- file.path(base_dir, "data")

# Define the path for the charts
chart_directory_path <- file.path(base_dir, "charts", "5-year_charts")

# Create the directory for the reduced size file following shrinkage code.
writeFilePath <- file.path(base_dir, "data")

# Define the console output directory and file name.
output_dir <- file.path(base_dir, "console_output")
output_file <- file.path(output_dir, "timeline_console_output.txt")

# Define the path to the directory containing your function scripts
functions_path <- file.path(base_dir, "functions")

# Get all .R files in the "functions" sub-directory
function_files <- list.files(functions_path, pattern = "\\.R$", full.names = TRUE)

# Source each function R file with error handling and message logging
lapply(function_files, function(file) {
  tryCatch({
    source(file)
    #    message("Successfully sourced: ", file)
  }, error = function(e) {
    message("Error sourcing: ", file, " - ", e$message)
  })
})

# Set global parameters
options(scipen = 999) # Set scipen option to a large value.
options(digits = 15) # Set the number of decimal places to 15, the max observed.

# Begin Execution.
cat("\nExecution begins at:", formattedStartTime)

# Start directing console output to the file
sink(output_file)

cat("\nExecution begins at:", formattedStartTime)

#########################################################################
cat("\n\n**********DATA INPUT AND PREPARATION**********\n")

# Specify the columns to read
desired_columns <- c("Created Date", "Closed Date", "Agency")

# Read only the specified columns
main_data_file <- file.path(data_file, main_data_file)
d311 <- as.data.frame(fread(
  main_data_file,
  select = desired_columns,         # Only these columns will be read
  colClasses = "character"          # Ensure all columns are read as character
))

num_rows <- nrow(d311)

#########################################################################
# Preparing data for consistency and normalization

# make columns names user friendly
d311 <- make_column_names_user_friendly(d311)

#d311 <- d311[, c("Created Date", "Closed Date", "Agency")]

d311 <- d311[!is.na(d311$created_date), ]

# Convert character fields to upper case to facilitate comparisons
columns_to_upper <- c("agency")
d311[columns_to_upper] <- lapply(d311[columns_to_upper], toupper)

# Consolidate Agency names
d311 <- consolidate_agencies((d311))

# Convert each date field to POSIXct format in UTC
d311$created_date <- as.POSIXct(d311$created_date, format = "%m/%d/%Y %I:%M:%S %p", tz = "America/New_York")
d311$closed_date <- as.POSIXct(d311$closed_date, format = "%m/%d/%Y %I:%M:%S %p", tz = "America/New_York")

# Call the function on your dataframe d311
date_columns <- c("created_date", "closed_date")
d311 <- adjust_feb_29_to_28(d311, date_columns)

#########################################################################
# Collect macro statistics from the dataset
# Extract the year(s) from the created_date column
years <- year(d311$created_date)
num_years <- unique(years)

cat("\nTotal rows:", format(num_rows, big.mark = ","), "covering", length(num_years), "years")

year_digits <- 5
file_name_prefix <- "5-year"
#########################################################################
# Calculate the earliest and latest dates directly
earliest_date <- min(d311$created_date, na.rm = TRUE)
latest_date <- max(d311$created_date, na.rm = TRUE)

# Print the formatted date range
cat(
  "\nData contains SRs created from",
  format(earliest_date, "%Y-%m-%d %H:%M:%S"),
  "through",
  format(latest_date, "%Y-%m-%d %H:%M:%S")
)

# Convert to yyyy-mm-dd format
earliest_title <- format(earliest_date, "%Y-%m-%d")
latest_title <- format(latest_date, "%Y-%m-%d")

#########################################################################
# Aggregate created_date by second (precise timestamps)
second_level_created_summary <- d311 %>%
  mutate(created_second = floor_date(created_date, "second")) %>%
  group_by(created_second) %>%
  summarise(count = n(), .groups = "drop")

# Aggregate created_date by minute using the second-level aggregation
minute_level_created_summary <- second_level_created_summary %>%
  mutate(created_minute = floor_date(created_second, "minute")) %>%
  group_by(created_minute) %>%
  summarise(count = sum(count), .groups = "drop")

# Aggregate created_date by hour using the minute-level aggregation
hour_level_created_summary <- minute_level_created_summary %>%
  mutate(created_hour = floor_date(created_minute, "hour")) %>%
  group_by(created_hour) %>%
  summarise(count = sum(count), .groups = "drop")

# Aggregate by created_date by hour
created_hour_of_day <- second_level_created_summary %>%
  mutate(created_hour = hour(created_second)) %>%  # Extract the hour from created_second
  group_by(created_hour) %>%
  summarise(count = sum(count), .groups = "drop")  # Sum the counts for each hour

# Aggregate created_date by day using the hour-level aggregation
day_level_summary <- hour_level_created_summary %>%
  mutate(created_day = as.Date(created_hour)) %>%
  group_by(created_day) %>%
  summarise(count = sum(count), .groups = "drop")

# Aggregate created_date by month using the day-level aggregation
monthly_summary <- day_level_summary %>%
  mutate(YearMonth = floor_date(created_day, "month")) %>%
  group_by(YearMonth) %>%
  summarise(count = sum(count), .groups = "drop")

# Aggregate created_date by year using the monthly-level aggregation
yearly_summary <- monthly_summary %>%
  mutate(Year = year(YearMonth)) %>%
  group_by(Year) %>%
  summarise(count = sum(count), .groups = "drop")

# Aggregate created_date by calendar month using the day-level aggregation
calendar_month_summary <- day_level_summary %>%
  mutate(Month = format(created_day, "%B")) %>%
  group_by(Month) %>%
  summarise(count = sum(count), .groups = "drop") %>%
  mutate(Month = factor(Month, levels = month.name))

# Aggregate created_date by day of the week using the day-level aggregation
day_of_week_summary <- day_level_summary %>%
  mutate(day_of_week = weekdays(created_day)) %>%
  group_by(day_of_week) %>%
  summarise(count = sum(count), .groups = "drop") %>%
  mutate(day_of_week = factor(day_of_week,
                              levels =
                                c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
                              labels =
                                c("1-Monday", "2-Tuesday", "3-Wednesday", "4-Thursday", "5-Friday", "6-Saturday", "7-Sunday")
  )) %>%
  arrange(day_of_week)

# Create day-of-the-year summary
day_of_year_summary <- day_level_summary %>%
  mutate(day_of_year = format(created_day, "%m/%d")) %>%
  group_by(day_of_year) %>%
  summarise(count = sum(count), .groups = "drop") %>%
  arrange(as.Date(paste0("2023/", day_of_year), format = "%Y/%m/%d")) %>%
  mutate(day_number = row_number())

# Convert day_of_year to character and order by count
day_of_year_summary$day_of_year <- as.character(day_of_year_summary$day_of_year)
day_of_year_summary <- day_of_year_summary[order(-day_of_year_summary$count), ]

#########################################################################
# Aggregate closed_date by second (precise timestamps) 
second_level_closed_summary <- d311 %>%
  filter(!is.na(closed_date)) %>%
  mutate(closed_second = floor_date(closed_date, "second")) %>%
  group_by(closed_second) %>%
  summarise(count = n(), .groups = "drop")

# Use second_level_closed_summary to aggregate SRs by the minute of the day
minute_level_closed_summary <- second_level_closed_summary %>%
  mutate(closed_minute = floor_date(closed_second, "minute")) %>%  # Round closed_second to minute
  group_by(closed_minute) %>%
  summarise(count = sum(count), .groups = "drop")  # Sum the counts for each minute

# Use second_level_closed_summary to aggregate SRs by the hour of the day
closed_hour_of_day <- second_level_closed_summary %>%
  mutate(closed_hour = hour(closed_second)) %>%  # Extract the hour from closed_second
  group_by(closed_hour) %>%
  summarise(count = sum(count), .groups = "drop")  # Sum the counts for each hour

#########################################################################
# Prepare data for charting
# Add a combined column for day_number and day_of_year
day_of_year_summary <- day_of_year_summary %>%
  mutate(day_info = paste(day_number, "-", day_of_year))
days_to_chart <- day_of_year_summary %>%
   select(count, day_info)

# Step 1: Find the busiest day
busiest_day <- day_level_summary %>%
  arrange(desc(count)) %>%
  slice(1) %>%
  pull(created_day)

# Step 2: Filter entries for the busiest day
entries_on_busiest_day <- d311 %>%
  filter(as.Date(created_date) == busiest_day)

# Step 3: Aggregate by hour for the busiest day
hourly_summary_busiest_day <- entries_on_busiest_day %>%
  mutate(created_hour = hour(created_date)) %>%
  group_by(created_hour) %>%
  summarise(count = n(), .groups = "drop")

# Step 3: Aggregate by minute for the busiest day
minute_summary_busiest_day <- entries_on_busiest_day %>%
  mutate(created_minute = floor_date(created_date, "minute")) %>%
  group_by(created_minute) %>%
  summarise(count = n(), .groups = "drop")

# Step 3: Filter out NA values in closed_date
entries_with_closed_date <- entries_on_busiest_day %>%
  filter(!is.na(closed_date))

# Step 4: Aggregate by minute for closed_date on the busiest day
minute_summary_closed_busiest_day <- entries_with_closed_date %>%
  mutate(closed_minute = floor_date(closed_date, "minute")) %>%
  group_by(closed_minute) %>%
  summarise(count = n(), .groups = "drop")

# Step 5: Aggregate by second for closed_date on the busiest day
second_summary_closed_busiest_day <- entries_with_closed_date %>%
  mutate(closed_second = floor_date(closed_date, "second")) %>%
  group_by(closed_second) %>%
  summarise(count = n(), .groups = "drop")

#########################################################################
# List of data frames to convert
df_list <- list(
  second_level_closed_summary_df = second_level_closed_summary,
  closed_hour_of_day_df = closed_hour_of_day,
  minute_summary_closed_busiest_day_df = minute_summary_closed_busiest_day,
  second_summary_closed_busiest_day_df = second_summary_closed_busiest_day,
  minute_summary_busiest_day_df = minute_summary_busiest_day,
  hourly_summary_busiest_day_df = hourly_summary_busiest_day,
  yearly_df = yearly_summary,
  monthly_df = monthly_summary,
  daily_df = day_level_summary,
  calendar_month_df = calendar_month_summary,
  day_counts_df = day_of_year_summary,
  day_of_week_df = day_of_week_summary,
  created_hour_of_day_df = created_hour_of_day
)

# Apply as.data.frame to each item in the list
df_list <- lapply(df_list, as.data.frame)
list2env(df_list, envir = .GlobalEnv)

#########################################################################
max_year <- yearly_df[which.max(yearly_df$count), ]
min_year <- yearly_df[which.min((yearly_df$count)), ]

# Extract the earliest and latest year counts
earliest_year_count <- yearly_df %>%
  filter(Year == min(Year)) %>%
  select(count) %>%
  pull()

latest_year_count <- yearly_df %>%
  filter(Year == max(Year)) %>%
  select(count) %>%
  pull()

# Compute the percentage growth
percentage_growth <- round(((latest_year_count - earliest_year_count) / earliest_year_count) * 100, 1)

cat("\n\nGrowth over", nrow(yearly_df), "years is", percentage_growth, "%")

cat("\n\nYearly Summary:\n")
print(yearly_df, row.names = FALSE, right = FALSE)

if (nrow(yearly_df) > 2) { # skip if <2 years. Not enough data to be meaningful.
  yearly_df$Year <- as.numeric(yearly_df$Year)
  earliest_year <- min(yearly_df$Year)

  chart_sub_title <- paste("(", earliest_title, "--", latest_title, ") total=", sep = "")

  # Define the extra line
  extra_line <- annotate("text",
    x = as.numeric(min_year$Year), y = max_year$count,
    label = paste0(year_digits, "-yr growth: ", percentage_growth, "%", sep = ""),
    size = 3.7, color = "#661100", vjust = -1, hjust = 0.1
  )
  
  SR_yearly <- base_bar_chart(
    dataset = yearly_df,
    x_col = "Year",                     # Use the numeric column for x-axis
    y_col = "count",                    # y-axis column
    chart_title = NULL,                 # Chart title (can be NULL)
    sub_title = chart_sub_title,        # Subtitle for the chart
    console_print_out_title = "Yearly SR count",  # Console summary title
    add_mean = TRUE,                    # Add mean annotation
    add_median = FALSE,                 # Do not add median annotation
    add_sd = FALSE,                     # Do not add standard deviation annotation
    add_trendline = TRUE,               # Add a trendline to the chart
    add_maximum = FALSE,                # Do not add maximum annotation
    add_minimum = FALSE,                # Do not add minimum annotation
    add_second_maximum = FALSE,         # Do not add second maximum annotation
    extra_line = extra_line,            # Add an optional extra line
    chart_file_name = paste0(file_name_prefix, "-trend_SRs_yearly.pdf"),  # Output file name
    chart_directory = chart_directory_path  # Directory for saving the chart
  )
}
    
#########################################################################
# Chart SRs by month
max_count <- max(monthly_df$count)
total_count <- sum(monthly_df$count)
total_count <- comma(total_count)

# # Convert YearMonth to date format
monthly_df$YearMonth <- as.Date(paste(monthly_df$YearMonth, "01", sep = "-"), format = "%Y-%m-%d")
max_month <- monthly_df[which.max(monthly_df$count), ]
min_month <- monthly_df[which.min(monthly_df$count), ]

# Ensure YearMonth is a Date object
monthly_df$YearMonth <- as.Date(monthly_df$YearMonth)

extra_line <- annotate("text",
  x = min_month$YearMonth, y = max_month$count,
  label = paste0(year_digits, "-yr growth: ", percentage_growth, "%", sep = ""),
  size = 5, color = "#661100", vjust = 1, hjust = 1
)

SR_monthly <- base_bar_chart(
  dataset = monthly_df,
  x_label_every = 180,
  x_col = "YearMonth",                # Use the Date column for x-axis
  y_col = "count",                    # y-axis column
  chart_title = "SRs by Month",       # Chart title
  sub_title = "",                     # Optional subtitle
  console_print_out_title = "Monthly SR Count",  # Console summary title
  add_mean = FALSE,                   # Do not add mean annotation
  add_median = FALSE,                 # Do not add median annotation
  add_sd = FALSE,                     # Do not add standard deviation annotation
  add_trendline = FALSE,              # Do not add trendline
  add_maximum = TRUE,                 # Add maximum annotation
  add_minimum = FALSE,                # Do not add minimum annotation
  add_second_maximum = FALSE,         # Do not add second maximum annotation
  extra_line = NULL,                  # No extra line
  rows_to_print = 24,                 # Print out two year's worth of data
  chart_file_name = "SRs_monthly.pdf",  # Output file name
  chart_directory = chart_directory_path               # Save in current directory
)

#########################################################################
# Ensure created_date is in Date format, then sort by it
daily_df$created_day <- as.Date(daily_df$created_day) # Convert to Date format once
daily_df <- daily_df[order(daily_df$created_day), ] # Sort by created_date

# Find corresponding dates
max_date <- daily_df$created_day[which.max(daily_df$count)]
#min_date <- daily_df$created_day[which.min(daily_df$count)]

earliest_day <- min(daily_df$created_day)
max_count <- max(daily_df$count)

SR_daily <- base_bar_chart(
  dataset = daily_df,
  x_label_every = 180,
  x_col = "created_day",              # Use the categorical or date column for x-axis
  y_col = "count",                    # y-axis column
  chart_title = "Daily SR count",     # Chart title
  sub_title = chart_sub_title,        # Optional subtitle
  console_print_out_title = "Daily SR Count",  # Console summary title
  add_mean = FALSE,                   # Do not add mean annotation
  add_median = TRUE,                  # Add median annotation
  add_sd = TRUE,                      # Add standard deviation annotation
  add_trendline = FALSE,              # Do not add trendline
  add_maximum = TRUE,                 # Add maximum annotation
  add_minimum = FALSE,                # Do not add minimum annotation
  add_second_maximum = FALSE,         # Do not add second maximum annotation
  extra_line = NULL,                  # No extra line
  rows_to_print = 31,                 # Print out 31 days of data (January)
  chart_file_name = paste0(file_name_prefix, "-trend_SRs_daily.pdf"),  # Output file name
  chart_directory = chart_directory_path,  # Directory for saving the chart
  horizontal_adjustment_max = 1.2,    # Adjust maximum label horizontally
  vertical_adjustment_max = 0.8       # Adjust maximum label vertically
)

#########################################################################

#Overall SRs created time-of-day summary (0900, 1000, 1100, 1200, 1300, etc.)
# Find the hour with the maximum count
max_hour_of_the_day <- created_hour_of_day_df[which.max(created_hour_of_day_df$count), ]

# Add annotation for the maximum count
extra_line <- annotate("text",
                       x = max_hour_of_the_day$created_hour, y = max_hour_of_the_day$count,
                       label = paste0("Max: ", format(max_hour_of_the_day$count, big.mark = ","), sep = ""),
                       size = 4, color = "black", vjust = -0.7, hjust = -0.1
)

# Create a new column to format closed_hour as HH:00 for printing
created_hour_of_day_df$created_hour_formatted <- sprintf("%02d:00", as.integer(closed_hour_of_day_df$closed_hour))

SR_created_time_of_day <- base_bar_chart(
  dataset = created_hour_of_day_df,
  x_col = "created_hour",             # Use the numeric column for x-axis
  y_col = "count",                    # y-axis column
  chart_title = "SRs by Hour-of-the-Day",  # Chart title
  sub_title = chart_sub_title,        # Subtitle for the chart
  console_print_out_title = "SRs Created by Hour-of-the-Day (00:00-23:00)",  # Console summary title
  add_mean = TRUE,                    # Add mean annotation
  add_median = FALSE,                 # Do not add median annotation
  add_sd = FALSE,                     # Do not add standard deviation annotation
  add_trendline = FALSE,              # Do not add trendline
  add_maximum = FALSE,                # Do not add maximum annotation
  add_minimum = FALSE,                # Do not add minimum annotation
  add_second_maximum = FALSE,         # Do not add second maximum annotation
  extra_line = NULL,                  # No extra line
  rows_to_print = 24,                 # Print a full day's data (00:00 - 23:00)
  chart_file_name = paste0(file_name_prefix, "-trend_SRs_created_by_hour_of_day.pdf"),  # Output file name
  chart_directory = chart_directory_path,  # Directory for saving the chart
  horizontal_adjustment_max = -1,     # Adjust maximum label horizontally
  vertical_adjustment_max = 1         # Adjust maximum label vertically
)

# Summary of SRs created by hour-of-the-day
cat("\nSummary of created by hour-of-the-day:\n")
# Select and reorder the columns to print
columns_to_print <- c("created_hour_formatted", "count")
#Print the dataframe with formatted closed_hour and counts
print(created_hour_of_day_df[, columns_to_print], row.names = FALSE, right = FALSE)

#########################################################################
# Overall closed time-of-day summary (0900, 1000, 1100, 1200, 1300, etc.)
# Find the hour with the maximum count
max_hour_of_the_day <- closed_hour_of_day_df[which.max(closed_hour_of_day_df$count), ]

# Add annotation for the maximum count (replace max_count with max_hour_of_the_day$count)
extra_line <- annotate("text",
                       x = max_hour_of_the_day$closed_hour, y = max_hour_of_the_day$count,
                       label = paste0("Max: ", format(max_hour_of_the_day$count, big.mark = ","), sep = ""),
                       size = 3.7, color = "black", vjust = -0.7, hjust = -.5
)

# Create a new column to format closed_hour as HH:00 for printing
closed_hour_of_day_df$closed_hour_formatted <- sprintf("%02d:00", as.integer(closed_hour_of_day_df$closed_hour))

SR_closed_time_of_day <- base_bar_chart(
  dataset = closed_hour_of_day_df,
  x_col = "closed_hour",              # Use the numeric column for x-axis
  y_col = "count",                    # y-axis column
  chart_title = "SRs Closed by Hour of Day",  # Chart title
  sub_title = chart_sub_title,        # Subtitle for the chart
  console_print_out_title = "SRs Closed by Hour-of-the-Day (00:00-23:00)",  # Console summary title
  add_mean = TRUE,                    # Add mean annotation
  add_median = FALSE,                 # Do not add median annotation
  add_sd = FALSE,                     # Do not add standard deviation annotation
  add_trendline = FALSE,              # Do not add trendline
  add_maximum = TRUE,                 # Add maximum annotation
  add_minimum = FALSE,                # Do not add minimum annotation
  add_second_maximum = FALSE,         # Do not add second maximum annotation
  extra_line = extra_line,            # Add an optional extra line
  rows_to_print = 24,                 # Print to console full day (00:00 - 23:00)
  chart_file_name = paste0(file_name_prefix, "-trend-SRs_closed_by_hour_of_day.pdf"),  # Output file name
  chart_directory = chart_directory_path,  # Directory for saving the chart
  horizontal_adjustment_max = -1,     # Adjust maximum label horizontally
  vertical_adjustment_max = 1         # Adjust maximum label vertically
)

# Summary of SRs closed by hour-of-the-day
cat("\nSummary of closed by hour-of-the-day:\n")
# Select and reorder the columns to print
columns_to_print <- c("closed_hour_formatted", "count")
#Print the dataframe with formatted closed_hour and counts
print(closed_hour_of_day_df[, columns_to_print], row.names = FALSE, right = FALSE)

#########################################################################
# Overall calendar month summary (Jan, Feb, Mar, etc.)

# Number of days in each month, with February as 28.2 days
days_in_month <- c(
  "January" = 31, "February" = 28.2, "March" = 31, "April" = 30,
  "May" = 31, "June" = 30, "July" = 31, "August" = 31,
  "September" = 30, "October" = 31, "November" = 30, "December" = 31
)

# Add the count_per_day column
calendar_month_df$count_per_day <- round(calendar_month_df$count / days_in_month[calendar_month_df$Month], 0)

# Order the dataframe by Month
calendar_month_df <- calendar_month_df[order(calendar_month_df$Month), ]

cat("\nCalendar Month with total count and count_per_day:\n")
print(calendar_month_df, row.names = FALSE, right = FALSE)

SR_calendar_month <- base_bar_chart( 
  dataset = calendar_month_df,
  x_col = "Month",                    # Use the categorical column for x-axis
  y_col = "count",                    # y-axis column
  chart_title = "SRs by Calendar Month",  # Chart title
  sub_title = chart_sub_title,        # Optional subtitle
  console_print_out_title = "SRs by Calendar Month",  # Console summary title
  add_mean = FALSE,                   # Do not add mean annotation
  add_median = FALSE,                 # Do not add median annotation
  add_sd = FALSE,                     # Do not add standard deviation annotation
  add_trendline = FALSE,              # Do not add trendline
  add_maximum = TRUE,                 # Add maximum annotation
  add_minimum = FALSE,                # Do not add minimum annotation
  add_second_maximum = FALSE,         # Do not add second maximum annotation
  extra_line = NULL,                  # No extra line
  chart_file_name = paste0(file_name_prefix, "-trend_SRs_by_calendar_month.pdf"),  # Output file name
  chart_directory = chart_directory_path  # Directory for saving the chart
)

#########################################################################

SR_day_of_the_year <- base_bar_chart(
  dataset = days_to_chart,
  x_label_every = 10,
  x_col = "day_info",               # Use the numeric column for x-axis
  y_col = "count",                    # y-axis column
  chart_title = "SRs by Day-of-the-Year",  # Chart title
  sub_title = chart_sub_title,        # Subtitle for the chart
  console_print_out_title = "SRs by Day_of_the_year",  # Console summary title
  add_mean = TRUE,                    # Add mean annotation
  add_median = FALSE,                 # Do not add median annotation
  add_sd = FALSE,                     # Do not add standard deviation annotation
  add_trendline = FALSE,              # Do not add a trendline
  add_maximum = TRUE,                 # Add maximum annotation
  add_minimum = FALSE,                # Do not add minimum annotation
  add_second_maximum = TRUE,          # Add second maximum annotation
  extra_line = NULL,                  # No extra line
  rows_to_print = 30,                # Print out a year's worth of data
  chart_file_name = paste0(file_name_prefix, "-trend_SRs_by_day_of_the_year.pdf"),  # Output file name
  chart_directory = chart_directory_path,  # Directory for saving the chart
  horizontal_adjustment_max = 1.2,    # Adjust maximum label horizontally
  vertical_adjustment_max = 1.5       # Adjust maximum label vertically
)

#########################################################################
# Overall day-of-the-week summary

day_of_week_df$day_of_week <- as.character(day_of_week_df$day_of_week)
#max_day_of_the_week <- day_of_week_df[which.max(day_of_week_df$count), ]

# Add a new column 'week_day' with row counts
day_of_week_df <- day_of_week_df %>%
  mutate(week_day = row_number())

# Convert the 'day_of_week' column to a factor
day_of_week_df$day_of_week <- factor(day_of_week_df$day_of_week)

SR_day_of_the_week <- base_bar_chart(
  dataset = day_of_week_df,
  x_col = "day_of_week",              # Use the categorical column for x-axis
  y_col = "count",                    # y-axis column
  chart_title = "SRs by Day-of-the-Week",  # Chart title
  sub_title = chart_sub_title,        # Optional subtitle
  console_print_out_title = "SRs by Day-of-the-Week",  # Console summary title
  add_mean = FALSE,                   # Do not add mean annotation
  add_median = FALSE,                 # Do not add median annotation
  add_sd = FALSE,                     # Do not add standard deviation annotation
  add_trendline = FALSE,              # Do not add trendline
  add_maximum = TRUE,                 # Add maximum annotation
  add_minimum = FALSE,                # Do not add minimum annotation
  add_second_maximum = FALSE,         # Do not add second maximum annotation
  extra_line = NULL,                  # No extra line
  chart_file_name = paste0(file_name_prefix, "-trend_SRs_by_day_of_the_week.pdf"),  # Output file name
  chart_directory = chart_directory_path,  # Directory for saving the chart
  horizontal_adjustment_max = 0.5,    # Adjust maximum label horizontally
  vertical_adjustment_max = 0.2       # Adjust maximum label vertically
)

#########################################################################

# Use second-level aggregated data and filter for top of the hour entries
filtered_by_hour <- second_level_created_summary %>%
  filter(minute(created_second) == 0 & second(created_second) == 0)

# Group the data by the hour
grouped_by_hour <- filtered_by_hour %>%
  mutate(created_hour = hour(created_second)) %>%
  group_by(created_hour) %>%
  summarise(count = sum(count), .groups = "drop")

# Find the hour with the maximum count
max_hour_of_the_day <- grouped_by_hour[which.max(grouped_by_hour$count), ]

# Add a text annotation for the maximum count
extra_line <- annotate("text",
  x = max_hour_of_the_day$created_hour, y = max_hour_of_the_day$count,
  label = paste0("Max: ", format(max_hour_of_the_day$count, big.mark = ",")),
  size = 3.7, color = "black", vjust = -0.4, hjust = 0.1
)

SR_created_by_top_of_hour <- base_bar_chart(
  dataset = grouped_by_hour,
  x_col = "created_hour",             # Use the numeric column for x-axis
  y_col = "count",                    # y-axis column
  chart_title = NULL,                 # Chart title (can be NULL)
  sub_title = chart_sub_title,        # Subtitle for the chart
  console_print_out_title = "SRs Created Exactly on the Hour (HH:00:00)",  # Console summary title
  add_mean = FALSE,                   # Do not add mean annotation
  add_median = TRUE,                  # Add median annotation
  add_sd = TRUE,                      # Add standard deviation annotation
  add_trendline = FALSE,              # Do not add trendline
  add_maximum = FALSE,                # Do not add maximum annotation
  add_minimum = FALSE,                # Do not add minimum annotation
  add_second_maximum = TRUE,          # Add second maximum annotation
  extra_line = extra_line,            # Add an optional extra line
  chart_file_name = paste0(file_name_prefix, "-trend_SRs_created_on_the_hour.pdf"),  # Output file name
  chart_directory = chart_directory_path  # Directory for saving the chart
)

#########################################################################

# Use pre-aggregated data for the minute-level summary
minute_counts_busiest_day <- minute_level_created_summary %>%
  filter(as.Date(created_minute) == as.Date(max_date)) # Filter for the busiest day

# Group the data by hour and minute
minute_counts_busiest_day <- minute_counts_busiest_day %>%
  mutate(hour = hour(created_minute), minute = minute(created_minute)) %>%
  group_by(hour, minute) %>%
  summarise(count = sum(count), .groups = "drop")

# Create a formatted "hour:minute" column
minute_counts_busiest_day <- minute_counts_busiest_day %>%
  mutate(hour_minute = sprintf("%02d:%02d", hour, minute))

# Convert hour_minute to a POSIXct time format
minute_counts_busiest_day$hour_minute <- as.POSIXct(minute_counts_busiest_day$hour_minute, format = "%H:%M")

# Add scale for x-axis (formatting the time by 2-hour intervals)
extra_line <- scale_x_datetime(expand = c(0.025, 0.025), date_labels = "%H:%M", breaks = scales::date_breaks("2 hours"))

SR_created_by_minute_of_busiest_day <- base_bar_chart(
  dataset = minute_counts_busiest_day,
  x_col = "hour_minute",              # Use the datetime column for x-axis
  y_col = "count",                    # y-axis column
  chart_title = paste("SRs Created by Exactly on the Minute of the Busiest Day (HH:MM:00) on", max_date),  # Chart title
  sub_title = chart_sub_title,        # Subtitle for the chart
  console_print_out_title = "SRs Created Exactly on the Minute (HH:MM:00) on Busiest Day",  # Console summary title
  add_mean = FALSE,                   # Do not add mean annotation
  add_median = FALSE,                 # Do not add median annotation
  add_sd = TRUE,                      # Add standard deviation annotation
  add_trendline = FALSE,              # Do not add trendline
  add_maximum = TRUE,                 # Add maximum annotation
  add_minimum = FALSE,                # Do not add minimum annotation
  add_second_maximum = FALSE,         # Do not add second maximum annotation
  extra_line = extra_line,            # Add an optional extra line
  chart_file_name = paste0(file_name_prefix, "-trend_SRs_created_by_minute_of_busiest_day.pdf"),  # Output file name
  chart_directory = chart_directory_path  # Directory for saving the chart
)

#########################################################################
# # Determine the # of SRs closed exactly on hour with minute and seconds == HH:00:00.

# Use second-level closed summary and filter for entries where minute and second are exactly zero
filtered_by_hour <- second_level_closed_summary %>%
  filter(minute(closed_second) == 0 & second(closed_second) == 0)

# Group the data by hour of the closed time
grouped_by_hour <- filtered_by_hour %>%
  mutate(closed_hour = hour(closed_second)) %>%
  group_by(closed_hour) %>%
  summarise(count = sum(count), .groups = "drop")

# Find the hour with the maximum count
max_hour_of_the_day <- grouped_by_hour[which.max(grouped_by_hour$count), ]

# Add annotation for the maximum count
extra_line <- annotate("text",
  x = max_hour_of_the_day$closed_hour, y = max_hour_of_the_day$count,
  label = paste0("Max: ", format(max_hour_of_the_day$count, big.mark = ","), sep = ""),
  size = 3.7, color = "black", vjust = -0.7, hjust = 0.1
)

SR_closed_by_top_of_hour <- base_bar_chart(
  dataset = grouped_by_hour,
  x_col = "closed_hour",              # Use the numeric column for x-axis
  y_col = "count",                    # y-axis column
  chart_title = NULL,                 # Chart title (can be NULL)
  sub_title = chart_sub_title,        # Subtitle for the chart
  console_print_out_title = "SRs Closed Exactly on the Hour (HH:00:00)",  # Console summary title
  add_mean = FALSE,                   # Do not add mean annotation
  add_median = FALSE,                 # Do not add median annotation
  add_sd = TRUE,                      # Add standard deviation annotation
  add_trendline = FALSE,              # Do not add trendline
  add_maximum = FALSE,                # Do not add maximum annotation
  add_minimum = FALSE,                # Do not add minimum annotation
  add_second_maximum = TRUE,          # Add second maximum annotation
  extra_line = extra_line,            # Add an optional extra line
  chart_file_name = paste0(file_name_prefix, "-trend_SRs_closed_on_the_hour.pdf"),  # Output file name
  chart_directory = chart_directory_path  # Directory for saving the chart
)

#########################################################################
# Show minute-by-minute closure of SRs on the busiest day of the year.

# Use pre-aggregated minute-level closed summary and filter for the busiest day
minute_counts_busiest_day <- minute_level_closed_summary %>%
  filter(as.Date(closed_minute) == as.Date(max_date))  # Filter for the busiest day

# Group by hour and minute, count the rows
minute_counts <- minute_counts_busiest_day %>%
  mutate(hour = hour(closed_minute), minute = minute(closed_minute)) %>%
  group_by(hour, minute) %>%
  summarise(count = sum(count), .groups = "drop")

# Add formatted "hour:minute" column
minute_counts <- minute_counts %>%
  mutate(hour_minute = sprintf("%02d:%02d", hour, minute))

# Convert hour_minute to a time format
minute_counts$hour_minute <- as.POSIXct(minute_counts$hour_minute, format = "%H:%M")

extra_line <- scale_x_datetime(expand = c(.03, .03), date_labels = "%H:%M", breaks = "2 hour")

SR_closed_by_minute_of_busiest_day <- base_bar_chart(
  dataset = minute_counts,
  x_col = "hour_minute",              # Use the datetime column for x-axis
  y_col = "count",                    # y-axis column
  chart_title = paste("SRs closed by Exactly on the Minute-of-the-busiest-Day (yy:xx:00) on", max_date),  # Chart title
  sub_title = chart_sub_title,        # Subtitle for the chart
  console_print_out_title = "SRs Closed Exactly on the Minute (HH:MM:00) of the Busiest Day",  # Console summary title
  add_mean = FALSE,                   # Do not add mean annotation
  add_median = FALSE,                 # Do not add median annotation
  add_sd = TRUE,                      # Add standard deviation annotation
  add_trendline = FALSE,              # Do not add trendline
  add_maximum = TRUE,                 # Add maximum annotation
  add_minimum = FALSE,                # Do not add minimum annotation
  add_second_maximum = TRUE,          # Add second maximum annotation
  extra_line = extra_line,            # Add an optional extra line
  chart_file_name = paste0(file_name_prefix, "-trend-SRs_closed_by_minute_of_busiest_day.pdf"),  # Output file name
  chart_directory = chart_directory_path,  # Directory for saving the chart
  horizontal_adjustment_max = -1,     # Adjust maximum label horizontally
  vertical_adjustment_max = 1         # Adjust maximum label vertically
)

#########################################################################
#


########################################################################
# Identify SRs created at midnight and noon using lubridate
# Extract hour, minute, and second components from second-level created summary
hour <- hour(second_level_created_summary$created_second)
minute <- minute(second_level_created_summary$created_second)
second <- second(second_level_created_summary$created_second)

# Identify rows with time exactly at midnight (00:00:00) and noon (12:00:00)
midnight_created_rows <- hour == 0 & minute == 0 & second == 0
noon_created_rows <- hour == 12 & minute == 0 & second == 0

# Get the data for SRs created exactly at midnight or noon
created_at_midnight <- second_level_created_summary[midnight_created_rows, ]
created_at_noon <- second_level_created_summary[noon_created_rows, ]

# Count the number of SRs created at midnight and noon
midnight_created_count <- nrow(created_at_midnight)
noon_created_count <- nrow(created_at_noon)

# Subset the data for SRs created at midnight and noon
created_at_midnight <- d311[midnight_created_rows, c("created_date", "agency")]
created_at_noon <- d311[noon_created_rows, c("created_date", "agency")]

# Process SRs created at midnight
if (midnight_created_count > 0) {
  cat(
    "\n\nThere are",
    format(midnight_created_count, big.mark = ","),
    "SRs that were created at exactly midnight."
  )

  sorted_create_at_midnight <- rank_by_agency(created_at_midnight)

  chart_title <- "SRs created exactly at midnight (00:00:00) by Agency & cumulative percentage"
  chart_file_name <- "SRs_created_at_midnight_by_Agency.pdf"

  create_combo_chart(
    created_at_midnight,
    chart_title,
    chart_file_name,
    console_print_out_title = "SRs created at midnight",
    chart_directory = chart_directory_path
  )
} else {
  cat("\n\nThere are no SRs with a created_date exactly at midnight.\n")
}

# Process SRs created at noon
if (noon_created_count > 0) {
  cat(
    "\n\nThere are",
    format(noon_created_count, big.mark = ","),
    "SRs that were created exactly at noon."
  )

  sorted_create_at_noon <- rank_by_agency(created_at_noon)

  chart_title <- "SRs created exactly at noon (12:00:00) by Agency & cumulative percentage"
  chart_file_name <- "SRs_created_at_noon_by_Agency.pdf"

  create_combo_chart(
    created_at_noon,
    chart_title,
    chart_file_name,
    chart_directory= chart_directory_path,
    console_print_out_title = "SRs created at noon"
  )
} else {
  cat("\n\nThere are no SRs with a created_date exactly at noon.\n")
}

#########################################################################
# Identify SRs closed at midnight and noon

# Remove N/A closed_date(s)
valid_closed_date <- !is.na(d311$closed_date)
valid_closed_data <- d311[valid_closed_date, ]

# Extract hour, minute, and second from the second-level closed summary
hour <- hour(second_level_closed_summary$closed_second)
minute <- minute(second_level_closed_summary$closed_second)
second <- second(second_level_closed_summary$closed_second)

# Identify rows with time exactly at midnight (00:00:00)
midnight_closed_rows <- hour == 0 & minute == 0 & second == 0
noon_closed_rows <- hour == 12 & minute == 0 & second == 0

# Get the data for rows that were closed at midnight or noon
midnight_closed_data <- second_level_closed_summary[midnight_closed_rows, ]
noon_closed_data <- second_level_closed_summary[noon_closed_rows, ]

# Count the number of rows with time exactly at midnight
midnight_closed_count <- sum(midnight_closed_rows)
noon_closed_count <- sum(noon_closed_rows)

midnight_closed_data <- valid_closed_data[midnight_closed_rows, ]
closed_at_midnight <- midnight_closed_data[, c("created_date", "agency")]

noon_closed_data <- valid_closed_data[noon_closed_rows, ]
closed_at_noon <- noon_closed_data[, c("created_date", "agency")]

if (midnight_closed_count > 0) {
  cat(
    "\n\nThere are",
    format(midnight_closed_count, big.mark = ","),
    "SRs that were closed exactly at midnight."
  )

  sorted_closed_at_midnight <- rank_by_agency(closed_at_midnight)

  chart_title <- "SRs closed exactly at midnight (00:00:00) by Agency & cumulative percentage"
  chart_file_name <- "SRs_closed_at_midnight_by_Agency.pdf"
  if (!is.null(sorted_closed_at_midnight)) {
    create_combo_chart(
      closed_at_midnight,
      chart_title,
      chart_file_name,
      chart_directory= chart_directory_path,
      console_print_out_title = "SRs closed at midnight"
    )
  } else {
    cat("\n\nThere are no SRs with a closed_date exactly at midnight (00:00:00).\n")
  }
}

if (noon_closed_count > 0) {
  cat(
    "\n\nThere are",
    format(noon_closed_count, big.mark = ","),
    "SRs that were closed exactly at noon."
  )

  sorted_closed_at_noon <- rank_by_agency(closed_at_noon)

  chart_title <- "SRs closed exactly at noon (12:00:00) by Agency & cumulative percentage"
  chart_file_name <- "SRs_closed_at_noon_by_Agency.pdf"
  if (!is.null(sorted_closed_at_noon)) {
    create_combo_chart(
      closed_at_noon,
      chart_title,
      chart_file_name,
      chart_directory= chart_directory_path,
      console_print_out_title = "SRs closed at noon"
    )
  } else {
    cat("\n\nThere are no SRs with a closed_date exactly at noon (12:00:00).\n")
  }
}

########################################################################
# Identify SRs closed at midnight and noon
# Extract hour, minute, and second components from second-level closed summary
hour <- hour(second_level_closed_summary$closed_second)
minute <- minute(second_level_closed_summary$closed_second)
second <- second(second_level_closed_summary$closed_second)

# Identify rows with time exactly at midnight (00:00:00) and noon (12:00:00)
midnight_closed_rows <- hour == 0 & minute == 0 & second == 0
noon_closed_rows <- hour == 12 & minute == 0 & second == 0

# Count the number of rows with time exactly at midnight and noon
midnight_closed_count <- sum(midnight_closed_rows)
noon_closed_count <- sum(noon_closed_rows)

# Get the data for SRs closed exactly at midnight or noon
midnight_closed_data <- second_level_closed_summary[midnight_closed_rows, ]
noon_closed_data <- second_level_closed_summary[noon_closed_rows, ]

# Subset the data for SRs closed at midnight and noon
closed_at_midnight <- valid_closed_data[midnight_closed_rows, c("created_date", "agency")]
closed_at_noon <- valid_closed_data[noon_closed_rows, c("created_date", "agency")]

# Process SRs closed at midnight
if (midnight_closed_count > 0) {
  cat(
    "\n\nThere are",
    format(midnight_closed_count, big.mark = ","),
    "SRs that were closed exactly at midnight."
  )

  sorted_closed_at_midnight <- rank_by_agency(closed_at_midnight)

  chart_title <- "SRs Closed Exactly at Midnight (00:00:00) by Agency & Cumulative Percentage"
  chart_file_name <- "SRs_closed_at_midnight_by_Agency.pdf"

  create_combo_chart(
    closed_at_midnight,
    chart_title = chart_title,
    chart_file_name = chart_file_name,
    chart_directory = chart_directory_path,
    console_print_out_title = "SRs Closed at Midnight"
  )       
} else {
  cat("\n\nThere are no SRs with a closed_date exactly at midnight (00:00:00).\n")
}

# Process SRs closed at noon
if (noon_closed_count > 0) {
  cat(
    "\n\nThere are",
    format(noon_closed_count, big.mark = ","),
    "SRs that were closed exactly at noon."
  )

  sorted_closed_at_noon <- rank_by_agency(closed_at_noon)

  chart_title <- "SRs Closed Exactly at Noon (12:00:00) by Agency & Cumulative Percentage"
  chart_file_name <- "SRs_closed_at_noon_by_Agency.pdf"

  create_combo_chart(
    closed_at_noon,
    chart_title = chart_title,
    chart_file_name = chart_file_name,
    chart_directory= chart_directory_path,
    console_print_out_title = "SRs Closed at Noon"
  )
} else {
  cat("\n\nThere are no SRs with a closed_date exactly at noon (12:00:00).\n")
}

#########################################################################
# Conclude program
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
# Call the end_program function with the formatted end time and duration string
end_program(formatted_end_time, duration_string)
