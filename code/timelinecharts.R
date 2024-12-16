# install.packages("ggplot2")
# install.packages("scales")
# install.packages("dplyr")
# install.packages('zoo')
# install.packages("ggpmisc")
# install.packages("lubridate")
# install.packages("data.table")
#rm(list = ls(), envir = .GlobalEnv)

library(ggplot2)
library(scales)
library(dplyr)
library(zoo)
library(ggpmisc)
library(lubridate)
library(data.table)

#########################################################################
rm(list = ls())

programStart <- as.POSIXct(Sys.time())
formattedStartTime <- format(programStart, "%Y-%m-%d %H:%M:%S")

#########################################################################

# Set path for the data file
main_data_file <- "311_Service_Requests_from_2022-2023_AS_OF_09-15-2024.csv"
#main_data_file <- "smaller_test_data.csv"
#main_data_file <- "extra_small.csv"

#########################################################################

cat("\n***** Program initialization *****")

setwd("C:\\Users\\David\\OneDrive\\Documents\\datacleaningproject\\nyc311clean")

# Create the sub-directories used during program execution.

# Get the current working directory
working_dir <- getwd()

# Set the base directory under the working directory
base_dir <- file.path(working_dir, "code")

# Define the path for the main data file (CSV file)
data_file <- file.path(base_dir, "data")

# Define the path for the charts
chart_directory_path <- file.path(base_dir, "charts")

# Create the directory for the reduced size file following shrinkage code.
writeFilePath <- file.path(base_dir, "data")

# Define the console output directory and file name.
output_dir <- file.path(base_dir, "console_output")
output_file <- file.path(output_dir, "timeline_console_output.txt")

# Define the path to the directory containing your function scripts
functions_path <- file.path(base_dir, "functions")

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

options(scipen = 999) # Set scipen option to a large value.
options(digits = 15) # Set the number of decimal places to 15, the max observed.

cat("\nExecution begins at:", formattedStartTime)

# Start directing console output to the file
sink(output_file)

cat("\nExecution begins at:", formattedStartTime)

#########################################################################
cat("\n\n**********DATA INPUT AND PREPARATION**********\n")

# Load the main 311 SR data file. Set the read & write paths.
main_data_file <- file.path(data_file, main_data_file)
d311 <- as.data.frame(fread(
  main_data_file,
  colClasses = "character"
))

num_rows <- nrow(d311)

#########################################################################
# Preparing data for consistency and normalization

# make columns names user friendly
d311 <- make_column_names_user_friendly(d311)

d311 <- d311[, c("created_date", "closed_date", "agency")]

d311 <- d311[!is.na(d311$created_date), ]

# Convert character fields to upper case to facilitate comparisons
columns_to_upper <- c("agency")
d311[columns_to_upper] <- lapply(d311[columns_to_upper], toupper)

# Consolidate Agency names
d311 <- consolidate_agencies((d311))

# Convert each date field to POSIXct format in UTC
d311$created_date <- as.POSIXct(d311$created_date, format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC")
d311$closed_date <- as.POSIXct(d311$closed_date, format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC")

# Call the function on your dataframe d311
date_columns <- c("created_date", "closed_date")
d311 <- adjust_feb_29_to_28(d311, date_columns)

#########################################################################
# Collect macro statistics from the dataset
# Extract the year(s) from the created_date column
years <- year(d311$created_date)
num_years <- unique(years)

cat("\nTotal rows:", format(num_rows, big.mark = ","), "covering", length(num_years), "years")

year_digits <- 2
file_name_prefix <- "2-year"
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
days_to_chart <- day_of_year_summary %>%
  select(count, day_number)

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

cat("\nGrowth over", nrow(yearly_df), "years is", percentage_growth, "%")

cat("\nYearly Summary:\n")
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

  SR_yearly <- create_bar_chart_numeric(
    dataset = yearly_df,
    x_col = "Year",
    y_col = "count",
    chart_title = NULL,
    sub_title = chart_sub_title,
    console_print_out_title = "Yearly SR count",
    add_mean = TRUE,
    add_median = FALSE,
    add_sd = FALSE,
    add_trendline = TRUE,
    add_maximum = FALSE,
    add_minimum = FALSE,
    add_second_maximum = FALSE,
    extra_line = extra_line,
    chart_file_name = paste0(file_name_prefix, "-trend_SRs_yearly.pdf"),
    chart_directory = chart_directory_path
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

SR_monthly <- create_bar_chart_categorical(
  dataset = monthly_df,
  x_col = "YearMonth",
  y_col = "count",
  chart_title = "SRs by Month",
  sub_title = chart_sub_title,
  console_print_out_title = "Monthly SR Count",
  add_mean = FALSE,
  add_median = FALSE,
  add_sd = FALSE,
  add_trendline = FALSE,
  add_maximum = TRUE,
  add_minimum = FALSE,
  add_second_maximum = FALSE,
  extra_line = NULL,
  chart_file_name = paste0(file_name_prefix, "-trend_SRs_monthly.pdf"),
  chart_directory = chart_directory_path
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

SR_daily <- create_bar_chart_categorical(
  dataset = daily_df,
  x_col = "created_day",
  y_col = "count",
  chart_title = "Daily SR count",
  sub_title = chart_sub_title,
  console_print_out_title = "Daily SR Count",
  add_mean = FALSE,
  add_median = TRUE,
  add_sd = TRUE,
  add_trendline = FALSE,
  add_maximum = TRUE,
  add_minimum = FALSE,
  add_second_maximum = FALSE,
  extra_line = NULL,
  chart_file_name = paste0(file_name_prefix, "-trend_SRs_daily.pdf"),
  chart_directory = chart_directory_path,
  horizontal_adjustment_max = 1.2,
  vertical_adjustment_max = 0.8
)

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

SR_calendar_month <- create_bar_chart_categorical(
  dataset = calendar_month_df,
  x_col = "Month",
  y_col = "count",
  chart_title = "SRs by Calendar Month",
  sub_title = chart_sub_title,
  console_print_out_title = "SRs by Calendar Month",
  add_mean = FALSE,
  add_median = FALSE,
  add_sd = FALSE,
  add_trendline = FALSE,
  add_maximum = TRUE,
  add_minimum = FALSE,
  add_second_maximum = FALSE,
  extra_line = NULL,
  chart_file_name = paste0(file_name_prefix, "-trend_SRs_by_calendar_month.pdf"),
  chart_directory = chart_directory_path
)

#########################################################################
SR_day_of_the_year <- create_bar_chart_numeric(
  dataset = days_to_chart,
  x_col = "day_number",
  y_col = "count",
  chart_title = "SRs by Day-of-the-Year",
  sub_title = chart_sub_title,
  console_print_out_title = "SRs by Day_of_the_year",
  add_mean = TRUE,
  add_median = FALSE,
  add_sd = FALSE,
  add_trendline = FALSE,
  add_maximum = TRUE,
  add_minimum = FALSE,
  add_second_maximum = TRUE,
  extra_line = NULL,
  chart_file_name = paste0(file_name_prefix, "-trend_SRs_by_day_of_the_year.pdf"),
  chart_directory = chart_directory_path,
  horizontal_adjustment_max = 1.2,
  vertical_adjustment_max = 1.5
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


SR_day_of_the_week <- create_bar_chart_categorical(
  dataset = day_of_week_df,
  x_col = "day_of_week",
  y_col = "count",
  chart_title = "SRs by Day-of-the-Week",
  sub_title = chart_sub_title,
  console_print_out_title = "SRs by Day-of-the-Week",
  add_mean = FALSE,
  add_median = FALSE,
  add_sd = FALSE,
  add_trendline = FALSE,
  add_maximum = TRUE,
  add_minimum = FALSE,
  add_second_maximum = FALSE,
  extra_line = NULL,
  chart_file_name = paste0(file_name_prefix, "-trend_SRs_by_day_of_the_week.pdf"),
  chart_directory = chart_directory_path,
  horizontal_adjustment_max = 0.5,
  vertical_adjustment_max = 0.2
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

# Create the bar chart for SRs created exactly on the hour
SR_created_by_top_of_hour <- create_bar_chart_numeric(
  dataset = grouped_by_hour,
  x_col = "created_hour",
  y_col = "count",
  chart_title = NULL,
  sub_title = chart_sub_title,
  console_print_out_title = "SRs Created Exactly on the Hour (xx:00:00)",
  add_mean = FALSE,
  add_median = TRUE,
  add_sd = TRUE,
  add_trendline = FALSE,
  add_maximum = FALSE,
  add_minimum = FALSE,
  add_second_maximum = TRUE,
  extra_line = extra_line,
  chart_file_name = paste0(file_name_prefix, "-trend_SRs_created_on_the_hour.pdf"),
  chart_directory = chart_directory_path
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

# Create the bar chart for SRs created minute by minute on the busiest day
SR_created_by_minute_of_busiest_day <- create_bar_chart_numeric(
  dataset = minute_counts_busiest_day,
  x_col = "hour_minute",
  y_col = "count",
  chart_title = paste("SRs Created by Exact Minute of the Busiest Day (xx:xx:00) on", max_date),
  sub_title = chart_sub_title,
  console_print_out_title = "SRs Created Exactly on the Minute (yy:xx:00) on Busiest Day",
  add_mean = FALSE,
  add_median = FALSE,
  add_sd = TRUE,
  add_trendline = FALSE,
  add_maximum = TRUE,
  add_minimum = FALSE,
  add_second_maximum = FALSE,
  extra_line = extra_line,
  chart_file_name = paste0(file_name_prefix, "-trend_SRs_created_by_minute_of_busiest_day.pdf"),
  chart_directory= chart_directory_path
)

#########################################################################
# # Determine the # of SRs closed exactly on hour with minute and seconds == 00:00.

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

# Create the bar chart for SRs closed exactly on the hour
SR_closed_by_top_of_hour <- create_bar_chart_numeric(
  dataset = grouped_by_hour,
  x_col = "closed_hour",
  y_col = "count",
  chart_title = NULL,
  sub_title = chart_sub_title,
  console_print_out_title = "SRs Closed Exactly on the Hour (xx:00:00)",
  add_mean = FALSE,
  add_median = FALSE,
  add_sd = TRUE,
  add_trendline = FALSE,
  add_maximum = FALSE,
  add_minimum = FALSE,
  add_second_maximum = TRUE,
  extra_line = extra_line,
  chart_file_name = paste0(file_name_prefix, "-trend_SRs_closed_on_the_hour.pdf"),
  chart_directory= chart_directory_path
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

SR_closed_by_minute_of_busiest_day <- create_bar_chart_numeric(
  dataset = minute_counts,
  x_col = "hour_minute",
  y_col = "count",
  chart_title = paste("SRs closed by Exact Minute-of-the-busiest-Day (yy:xx:00) on", max_date),
  sub_title = chart_sub_title,
  console_print_out_title = "SRs closed by Exact Minute (yy:xx:00) of the Busiest Day",
  add_mean = FALSE,
  add_median = FALSE,
  add_sd = TRUE,
  add_trendline = FALSE,
  add_maximum = TRUE,
  add_minimum = FALSE,
  add_second_maximum = TRUE,
  extra_line = extra_line,
  chart_file_name = paste0(file_name_prefix, "-trend-SRs_closed_by_minute_of_busiest_day.pdf"),
  chart_directory = chart_directory_path,
  horizontal_adjustment_max = -1,
  vertical_adjustment_max = 1
)

# Use pre-aggregated minute-level closed summary and filter for the busiest day
# Filter for the busiest day
minute_counts_busiest_day <- minute_level_closed_summary %>%
  filter(as.Date(closed_minute) == as.Date(max_date))  # Filter for the busiest day

minute_counts_busiest_day <- minute_level_closed_summary %>%
  filter(as.Date(closed_minute) == as.Date(max_date)) # Filter for the busiest day

# Group the data by hour and minute
minute_counts_busiest_day <- minute_counts_busiest_day %>%
  mutate(hour = hour(closed_minute), minute = minute(closed_minute)) %>%
  group_by(hour, minute) %>%
  summarise(count = sum(count), .groups = "drop")

# Create a formatted "hour:minute" column
minute_counts_busiest_day <- minute_counts_busiest_day %>%
  mutate(hour_minute = sprintf("%02d:%02d", hour, minute))

# Convert hour_minute to a POSIXct time format
minute_counts_busiest_day$hour_minute <- as.POSIXct(minute_counts_busiest_day$hour_minute, format = "%H:%M")

# Add scale for x-axis (formatting the time by 2-hour intervals)
extra_line <- scale_x_datetime(expand = c(.03, .03), date_labels = "%H:%M", breaks = scales::date_breaks("2 hours"))

# Create the bar chart for SRs closed minute by minute on the busiest day
SR_closed_by_minute_of_busiest_day <- create_bar_chart_numeric(
  dataset = minute_counts_busiest_day,
  x_col = "hour_minute",
  y_col = "count",
  chart_title = paste("SRs Closed by Exact Minute-of-the-Busiest-Day (xx:xx:00) on", max_date),
  sub_title = chart_sub_title,
  console_print_out_title = "SRs Closed by Exact Minute (xx:xx:00) on Busiest Day",
  add_mean = FALSE,
  add_median = FALSE,
  add_sd = TRUE,
  add_trendline = FALSE,
  add_maximum = TRUE,
  add_minimum = FALSE,
  add_second_maximum = TRUE,
  extra_line = extra_line,
  chart_file_name = paste0(file_name_prefix, "-trend-SRs_closed_by_minute_of_busiest_day.pdf"),
  chart_directory= chart_directory_path,
  horizontal_adjustment_max = -1,
  vertical_adjustment_max = 1
)

#########################################################################
# Overall created time-of-day summary (0900, 1000, 1100, 1200, 1300, etc.)
# Find the hour with the maximum count
max_hour_of_the_day <- created_hour_of_day_df[which.max(created_hour_of_day_df$count), ]

# Add annotation for the maximum count
extra_line <- annotate("text",
  x = max_hour_of_the_day$created_hour, y = max_hour_of_the_day$count,
  label = paste0("Max: ", format(max_hour_of_the_day$count, big.mark = ","), sep = ""),
  size = 4, color = "black", vjust = -0.7, hjust = -0.1
)

# Create the bar chart for SRs created by hour of the day
SR_created_time_of_day <- create_bar_chart_numeric(
  dataset = created_hour_of_day_df,
  x_col = "created_hour",
  y_col = "count",
  chart_title = "SRs created by Hour-of-the-Day",
  sub_title = chart_sub_title,
  console_print_out_title = "SR created by Hour-of-the-Day",
  add_mean = TRUE,
  add_median = FALSE,
  add_sd = FALSE,
  add_trendline = FALSE,
  add_maximum = FALSE,
  add_minimum = FALSE,
  add_second_maximum = FALSE,
  chart_file_name = paste0(file_name_prefix, "-trend_SRs_created_by_hour_of_day.pdf"),
  chart_directory= chart_directory_path,
  horizontal_adjustment_max = -1,
  vertical_adjustment_max = 1
)

# Summary of SRs closed by hour-of-the-day
cat("\nSummary of created by hour-of-the-day:\n")

# Print the dataframe with formatted closed_hour and counts
print(created_hour_of_day_df[, c("created_hour", "count")], row.names = FALSE, right = FALSE)

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

# Create the bar chart for SRs closed by hour of the day
SR_closed_time_of_day <- create_bar_chart_numeric(
  dataset = closed_hour_of_day_df,
  x_col = "closed_hour", # Change this to closed_hour_formatted if you want formatted values in the chart
  y_col = "count",
  chart_title = "SRs Closed by Hour of Day",
  sub_title = chart_sub_title,
  console_print_out_title = "SRs Closed by Hour-of-the-Day",
  add_mean = TRUE,
  add_median = FALSE,
  add_sd = FALSE,
  add_trendline = FALSE,
  add_maximum = TRUE,
  add_minimum = FALSE,
  add_second_maximum = FALSE,
  extra_line = extra_line,
  chart_file_name = paste0(file_name_prefix, "-trend-SRs_closed_by_hour_of_day.pdf"),
  chart_directory= chart_directory_path,
  horizontal_adjustment_max = -1,
  vertical_adjustment_max = 1
)

# Summary of SRs closed by hour-of-the-day
cat("\nSummary of closed by hour-of-the-day:\n")

# Create a new column to format closed_hour as HH:00 for printing
closed_hour_of_day_df$closed_hour_formatted <- sprintf("%02d:00", as.integer(closed_hour_of_day_df$closed_hour))

# Print the dataframe with formatted closed_hour and counts
print(closed_hour_of_day_df[, c("closed_hour_formatted", "count")], row.names = FALSE, right = FALSE)

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
programStop <- as.POSIXct(Sys.time())
duration <- difftime(programStop, programStart, units = "secs")

if (duration > 3600) {
  duration <- duration / 3600 # Convert to hours
  cat("\n\nProgram run-time: ", sprintf("%.2f", duration), "hours\n")
} else if (duration > 60) {
  duration <- duration / 60 # Convert to minutes
  cat("\n\nProgram run-time: ", sprintf("%.2f", duration), "minutes\n")
} else {
  cat("\n\nProgram run-time: ", sprintf("%.2f", duration), "seconds\n")
}

cat("\n *****END OF PROGRAM*****")
#########################################################################
sink()

programStop <- as.POSIXct(Sys.time())
duration <- difftime(programStop, programStart, units = "secs")

if (duration > 3600) {
  duration <- duration / 3600 # Convert to hours
  cat("\n\nProgram run-time: ", sprintf("%.2f", duration), "hours\n")
} else if (duration > 60) {
  duration <- duration / 60 # Convert to minutes
  cat("\n\nProgram run-time: ", sprintf("%.2f", duration), "minutes\n")
} else {
  cat("\n\nProgram run-time: ", sprintf("%.2f", duration), "seconds\n")
}

cat("\n *****END OF PROGRAM*****")
