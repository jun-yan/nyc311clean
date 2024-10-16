# install.packages("ggplot2")
# install.packages("scales")
# install.packages("dplyr")
# install.packages('zoo')
# install.packages("ggpmisc")
# install.packages("lubridate")
rm(list = ls(), envir = .GlobalEnv)

library(ggplot2)
library(scales)
library(dplyr)
library(zoo)
library(ggpmisc)
library(lubridate)

programStart <- as.POSIXct(Sys.time())
formattedStartTime <- format(programStart, "%Y-%m-%d %H:%M:%S")
cat("\nExecution begins at:", formattedStartTime)

#########################################################################

# Set path for the data file
#main_data_file <- "10-year 2014-2023.csv"
main_data_file <- "311_Service_Requests_from_2022-2023_AS_OF_09-15-2024.csv"

#########################################################################

cat("\n***** Program initialization *****")

setwd("C:/Users/david/OneDrive/Documents/datacleaningproject/nyc311clean/code")

# Define the path to the directory containing your function scripts
functions_path <- "functions"

# Source all .R files in the directory
files <- list.files(functions_path, pattern = "\\.R$", full.names = TRUE)

# Source each file
lapply(files, source)

data1File <- file.path("..", "..", "data", main_data_file)

# Extract the first two digits for file naming purposes
year_digits <- substr(main_data_file, 1, 2)

if (year_digits != "10") { year_digits <- "2"} 
  # Set path for the chart directory

chart_prefix <- paste0(year_digits, "-year")
chart_directory_path <- file.path("..", "..", "charts", "2022-2023 study", chart_prefix)

file_name_prefix <- chart_prefix #to name individual chart files

# Set scipen option to a large value to prevent scientific notation for numbers
options(scipen = 10)

sink(paste0("../../console_output/", year_digits, "-yr timeline_console_output.txt"))

cat("\nExecution begins at:", formattedStartTime)

#########################################################################
cat("\n\n**********DATA INPUT AND PREPARATION**********\n")

# Load the main 311 SR data file. Set the read & write paths.
d311 <-
  read.csv(data1File,
    header = TRUE,
    colClasses = rep("character", ncol(read.csv(data1File)))
  )

num_rows <- nrow(d311)

#########################################################################
# Preparing data for consistency and normalization

# make columns names user friendly
d311 <- make_column_names_user_friendly(d311)

d311 <- d311[, c("created_date", "closed_date", "agency")]

d311 <- d311[!is.na(d311$created_date), ]

# Convert character fields to upper case to facilitate comparisons
columns_to_upper <- c( "agency" )
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

#########################################################################
# Calculate the earliest and latest dates directly
earliest_date <- min(d311$created_date, na.rm = TRUE)
latest_date <- max(d311$created_date, na.rm = TRUE)

# Print the formatted date range
cat("\nData contains SRs created from", 
    format(earliest_date, "%Y-%m-%d %H:%M:%S"), 
    "through", 
    format(latest_date, "%Y-%m-%d %H:%M:%S"))

# Convert to yyyy-mm-dd format
earliest_title<- format(earliest_date, "%Y-%m-%d")
latest_title<- format(latest_date, "%Y-%m-%d")

chart_sub_title <- paste("(", earliest_title, "--", latest_title, ") total=", sep = "")

#########################################################################
# Sort by Agency and rank
sorted_by_agency <- rank_by_agency(d311)

cat("\n\nRanking by Agency\n")
print_threshold <- nrow(sorted_by_agency)
sorted_by_agency <- as.data.frame(sorted_by_agency)
print(head(sorted_by_agency, print_threshold), row.names = FALSE, right = FALSE)

chart_title <- "Complaints by Agency"
chart_file_name <- "Complains_by_Agency.pdf"

create_combo_chart(
  dataset = d311,
  chart_title = chart_title,
  chart_file_name = chart_file_name, 
  console_print_out_title = "Summary of complaints by Agency"
)
 
# ########################################################################
# Aggregate at the yearly level
yearly_summary <- d311 %>%
  mutate(Year = year(created_date)) %>%
  group_by(Year) %>%
  summarise(count = n(), .groups = 'drop')

# Aggregate at the monthly level
monthly_summary <- d311 %>%
  mutate(YearMonth = floor_date(created_date, "month")) %>%
  group_by(YearMonth) %>%
  summarise(count = n(), .groups = 'drop')

# Aggregate at the daily level
daily_summary <- d311 %>%
  mutate(created_date = as.Date(created_date)) %>%
  group_by(created_date) %>%
  summarise(count = n(), .groups = 'drop')

########################################################################
# Aggregate by calendar month
calendar_month_summary <- d311 %>%
  mutate(Month = format(created_date, "%B")) %>%
  group_by(Month) %>%
  summarise(count = n(), .groups = 'drop') %>%
  mutate(Month = factor(Month, levels = month.name))

# Find the maximum count
max_rows_calendar_month <- max(calendar_month_summary$count, na.rm = TRUE)

#########################################################################
# Extract day of the year and aggregate counts
day_counts <- d311 %>%
  mutate(day_of_year = format(created_date, "%m/%d")) %>%
  group_by(day_of_year) %>%
  summarise(count = n(), .groups = 'drop')

#########################################################################
day_of_week <- d311 %>%
  mutate(day_of_week = weekdays(created_date)) %>%
  group_by(day_of_week) %>%
  summarize(count = n(), .groups = 'drop') %>%
  mutate(day_of_week = factor(day_of_week, levels = 
                                c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
                              labels = 
                                c("1-Monday", "2-Tuesday", "3-Wednesday", "4-Thursday", "5-Friday", "6-Saturday", "7-Sunday"))) %>%
arrange(day_of_week)

#########################################################################
# Extract created_hour and closed_hour in one step
d311 <- d311 %>%
  mutate(
    created_hour = hour(created_date),
    closed_hour = hour(closed_date)
  )

# Aggregate the rows by created_hour, excluding NA values
created_counts <- d311 %>%
  filter(!is.na(created_hour)) %>%
  group_by(created_hour) %>%
  summarise(count = n(), .groups = 'drop')

# Aggregate the rows by closed_hour, excluding NA values
closed_counts <- d311 %>%
  filter(!is.na(closed_hour)) %>%
  group_by(closed_hour) %>%
  summarise(count = n(), .groups = 'drop')

#########################################################################
# List of data frames to convert
df_list <- list(
  yearly_df = yearly_summary,
  monthly_df = monthly_summary,
  daily_df = daily_summary,
  calendar_month_df = calendar_month_summary,
  day_counts_df = day_counts,
  day_of_week_df = day_of_week,
  created_hour_of_day_df = created_counts,
  closed_hour_of_day_df = closed_counts
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

if (nrow(yearly_df) > 1) {  #skip if only 1-2 years. Not enough data to be meaningful.
  yearly_df$Year <- as.numeric(yearly_df$Year)
  earliest_year <- min(yearly_df$Year)

  chart_sub_title <- paste("(", earliest_title, "--", latest_title, ") total=", sep = "")

  # Define the extra line
  extra_line <- annotate("text",
    x = as.numeric(min_year$Year), y = max_year$count,
    label = paste0(year_digits, "-yr growth: ", percentage_growth, "%", sep = ""),
    size = 3.7, color = "#661100", vjust = -1, hjust = 0.1 )

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
    chart_file_name = paste0(file_name_prefix, "-trend_SRs_yearly.pdf")
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

mxmonth <- max_month$YearMonth
mimonth <- min_month$YearMonth

earliest_YearMonth <- min(monthly_df$YearMonth)

start_date <- min(monthly_df$YearMonth)
end_date <- max(monthly_df$YearMonth)

# Ensure YearMonth is a Date object
monthly_df$YearMonth <- as.Date(monthly_df$YearMonth)

extra_line <- annotate("text",
  x = min_month$YearMonth, y = max_month$count,
  label = paste0(year_digits, "-yr growth: ", percentage_growth, "%", sep = ""),
  size = 5, color = "#661100", vjust = 1, hjust = 1 )

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
  chart_file_name = paste0(file_name_prefix, "-trend_SRs_monthly.pdf")
)

#########################################################################
# Ensure created_date is in Date format, then sort by it
daily_df$created_date <- as.Date(daily_df$created_date)  # Convert to Date format once
daily_df <- daily_df[order(daily_df$created_date), ]  # Sort by created_date

# Find corresponding dates
max_date <- daily_df$created_date[which.max(daily_df$count)]
min_date <- daily_df$created_date[which.min(daily_df$count)]

earliest_day <- min(daily_df$created_date)
max_count <- max(daily_df$count)

extra_line2 <- annotate("text",
  x = earliest_day, y = max_count,
  label = paste0(year_digits, "-yr growth: ", percentage_growth, "%", sep = ""),
  size = 3.7, color = "#E69F00", vjust = 0.6, hjust = -2
)

SR_daily <- create_bar_chart_categorical(
  dataset = daily_df,
  x_col = "created_date",
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
  chart_file_name = paste0(file_name_prefix, "-trend_SRs_by_calendar_month.pdf")
)

#########################################################################
# Overall day-of-the-year summary (Jan 1st, Jan 2nd, Jan 3rd, etc.)

# Assuming day_counts_df is already sorted by date
day_counts_df <- day_counts_df %>%
  arrange(as.Date(paste0("2020/", day_of_year), format = "%Y/%m/%d")) %>%
  mutate(day_number = row_number())

# Convert day_of_year to character
day_counts_df$day_of_year <- as.character(day_counts_df$day_of_year)

day_counts_df <- day_counts_df[order(-day_counts_df$count), ]

days_to_chart <- day_counts_df %>%
  select(count, day_number)

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
  horizontal_adjustment_max = 1.2,
  vertical_adjustment_max = 1.5
)

#########################################################################
# Overall day-of-the-week summary

day_of_week_df$day_of_week <- as.character(day_of_week_df$day_of_week)
max_day_of_the_week <- day_of_week_df[which.max(day_of_week_df$count), ]

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
  horizontal_adjustment_max = 0.5,
  vertical_adjustment_max = 0.2
)

#########################################################################
# Determine the # of SRs created exactly on the hour with 00 minutes and 00 seconds.
# Looking for an anomaly at midnight and noon.

# Filter rows where the created_date has seconds = "00" and minutes = "00"
filtered_by_hour <- d311 %>%
  filter(minute(created_date) == 0 & second(created_date) == 0)

grouped_by_hour <- filtered_by_hour %>%
  group_by(hour = hour(created_date)) %>%
  summarise(count = n())

# total_count <- sum(grouped_by_hour$count)
max_hour_of_the_day <- grouped_by_hour[which.max(grouped_by_hour$count), ]

extra_line <- annotate("text",
  x = max_hour_of_the_day$hour, y = max_hour_of_the_day$count,
  label = paste0("Max: ", format(max_hour_of_the_day$count, big.mark = ",")),
  size = 3.7, color = "black", vjust = -0.4, hjust = 0.1
)

SR_created_by_top_of_hour <- create_bar_chart_numeric(
  dataset = grouped_by_hour,
  x_col = "hour",
  y_col = "count",
  chart_title = "SRs created Exactly on the Hour (xx:00:00))",
  sub_title = chart_sub_title,
  console_print_out_title = "SRs created Exactly on the Hour (xx:00:00)",
  add_mean = FALSE,
  add_median = TRUE,
  add_sd = TRUE,
  add_trendline = FALSE,
  add_maximum = FALSE,
  add_minimum = FALSE,
  add_second_maximum = TRUE,
  extra_line = extra_line,
  chart_file_name = paste0(file_name_prefix, "-trend_SRs_created_on_the_hour.pdf")
)

#########################################################################
# Show minute-by-minute creation of SRs on the busiest day of the year.
# Ensure created_date is in the correct time zone
d311 <- d311 %>%
  mutate(created_date = with_tz(created_date, tzone = "UTC"))

# Filter the data for the desired date and seconds value "00"
date_to_filter <- max_date

date_filtered <- d311 %>%
  filter(
    as_date(created_date, tz = "UTC") == ymd(date_to_filter, tz = "UTC"),
    second(created_date) == 0
  )

# Group by hour and minute, count the rows
minute_counts <- date_filtered %>%
  group_by(hour = hour(created_date), minute = minute(created_date)) %>%
  summarise(count = n(), .groups = "drop")

minute_counts <- minute_counts %>%
  mutate(hour_minute = sprintf("%02d:%02d", hour, minute))

# Convert hour_minute to a time format
minute_counts$hour_minute <- as.POSIXct(minute_counts$hour_minute, format = "%H:%M")
#earliest_hour_minute <- min(minute_counts$hour_minute)

extra_line <- scale_x_datetime(expand = c(0.025, 0.025), date_labels = "%H:%M", breaks = "2 hour")

SR_created_by_minute_of_busiest_day <- create_bar_chart_numeric(
  dataset = minute_counts,
  x_col = "hour_minute",
  y_col = "count",
  chart_title = paste("SRs created by Exact Minute-of-the-busiest-Day (xx:xx:00) on", max_date),
  sub_title = chart_sub_title,
  console_print_out_title = "SRs created Exactly on the Minute (yy:xx:00) on busiest Day",
  add_mean = FALSE,
  add_median = FALSE,
  add_sd = TRUE,
  add_trendline = FALSE,
  add_maximum = TRUE,
  add_minimum = FALSE,
  add_second_maximum = FALSE,
  extra_line = extra_line,
  chart_file_name = paste0(file_name_prefix, "-trend_SRs_created_by_minute_of_busiest_day.pdf")
)

#########################################################################
# Determine the # of SRs closed exactly on hour with minute and seconds == 00:00.
# Looking for an anomaly at midnight and noon.

# Filter rows where the created_date has seconds = "00" and minutes = "00"
filtered_by_hour <- d311 %>%
  filter(!is.na(closed_date) & minute(closed_date) == 0 & second(closed_date) == 0)

grouped_by_hour <- filtered_by_hour %>%
  group_by(hour = hour(closed_date)) %>%
  summarise(count = n())

#total_count <- sum(grouped_by_hour$count)

max_hour_of_the_day <- grouped_by_hour[which.max(grouped_by_hour$count), ]

extra_line <- annotate("text",
  x = max_hour_of_the_day$hour, y = max_hour_of_the_day$count,
  label = paste0("Max: ", format(max_count, big.mark = ","), sep = ""),
  size = 3.7, color = "black", vjust = -0.7, hjust = 0.1
)

SR_closed_by_top_of_hour <- create_bar_chart_numeric(
  dataset = grouped_by_hour,
  x_col = "hour",
  y_col = "count",
  chart_title = "SRs closed Exactly on the Hour (xx:00:00)",
  sub_title = chart_sub_title,
  console_print_out_title = "SRs closed exactly on the Hour (xx:00:00) on busiest Day",
  add_mean = FALSE,
  add_median = FALSE,
  add_sd = TRUE,
  add_trendline = FALSE,
  add_maximum = FALSE,
  add_minimum = FALSE,
  add_second_maximum = TRUE,
  extra_line = extra_line,
  chart_file_name = paste0(file_name_prefix, "-trend_SRs_closed_on_the_hour.pdf")
)

#########################################################################
# Show minute-by-minute closure of SRs on the busiest day of the year.
# Ensure closed_date is in the correct time zone
d311 <- d311 %>%
  mutate(closed_date = with_tz(closed_date, tzone = "UTC"))

# Filter the data for the desired date and seconds value "00"
date_to_filter <- max_date

date_filtered <- d311 %>%
  filter(
    as_date(closed_date, tz = "UTC") == ymd(date_to_filter, tz = "UTC"),
    second(closed_date) == 0 & !is.na(closed_date)
  )

# Group by hour and minute, count the rows
minute_counts <- date_filtered %>%
  group_by(hour = hour(closed_date), minute = minute(closed_date)) %>%
  summarise(count = n(), .groups = "drop")

total_count <- sum(minute_counts$count)

# Assuming 'minute_counts' is your data frame with 'hour' and 'minute' columns
minute_counts <- minute_counts %>%
  mutate(hour_minute = sprintf("%02d:%02d", hour, minute))

# Convert hour_minute to a time format
minute_counts$hour_minute <- as.POSIXct(minute_counts$hour_minute, format = "%H:%M")
#earliest_hour_minute <- min(minute_counts$hour_minute)

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
  horizontal_adjustment_max = -1,
  vertical_adjustment_max = 1
)

#########################################################################
# Overall created time-of-day summary (0900, 1000, 1100, 1200, 1300, etc.)

max_hour_of_the_day <- created_hour_of_day_df[which.max(created_hour_of_day_df$count), ]

extra_line <- annotate("text",
  x = max_hour_of_the_day$created_hour, y = max_hour_of_the_day$count,
  label = paste0("Max: ", format(max_count, big.mark = ","), sep = ""),
  size = 4, color = "black", vjust = -0.7, hjust = -0.1
)


SR_created_time_of_day <- create_bar_chart_numeric(
  dataset = created_hour_of_day_df,
  x_col = "created_hour",
  y_col = "count",
  chart_title = "SRs created by hour-of-the-day",
  sub_title = chart_sub_title,
  console_print_out_title = "SR created by hour-of-the-day",
  add_mean = TRUE,
  add_median = FALSE,
  add_sd = FALSE,
  add_trendline = FALSE,
  add_maximum = FALSE,
  add_minimum = FALSE,
  add_second_maximum = FALSE,
  chart_file_name = paste0(file_name_prefix, "-trend_SRs_created_by_hour_of_day.pdf"),
  horizontal_adjustment_max = -1,
  vertical_adjustment_max = 1
)

#########################################################################
# Overall closed time-of-day summary (0900, 1000, 1100, 1200, 1300, etc.)
max_hour_of_the_day <- closed_hour_of_day_df[which.max(closed_hour_of_day_df$count), ]

extra_line <- annotate("text",
  x = max_hour_of_the_day$closed_hour, y = max_hour_of_the_day$count,
  label = paste0("Max: ", format(max_count, big.mark = ","), sep = ""),
  size = 3.7, color = "black", vjust = -0.7, hjust = -.5
)

SR_closed_time_of_day <- create_bar_chart_numeric(
  dataset = closed_hour_of_day_df,
  x_col = "closed_hour",
  y_col = "count",
  chart_title = "SRs closed by Hour of Day",
  sub_title = chart_sub_title,
  console_print_out_title = "SRs closed by Hour-of-the-Day",
  add_mean = TRUE,
  add_median = FALSE,
  add_sd = FALSE,
  add_trendline = FALSE,
  add_maximum = TRUE,
  add_minimum = FALSE,
  add_second_maximum = FALSE,
  extra_line = extra_line,
  chart_file_name = paste0(file_name_prefix, "-trend-SRs_closed_by_hour_of_day.pdf"),
  horizontal_adjustment_max = -1,
  vertical_adjustment_max = 1
)

cat("\nSummary of closed by hour-of-the-day:\n")

# Create a new column to format closed_hour as HH:00
closed_hour_of_day_df$closed_hour_formatted <- sprintf("%02d:00", as.integer(closed_hour_of_day_df$closed_hour))
# Print the dataframe with formatted closed_hour and counts
print(closed_hour_of_day_df[, c("closed_hour_formatted", "count")], row.names = FALSE, right = FALSE)

########################################################################
# Identify SRs created at midnight and noon

hour <- as.numeric(format(d311$created_date, "%H"))
minute <- as.numeric(format(d311$created_date, "%M"))
second <- as.numeric(format(d311$created_date, "%S"))

# Identify rows with time exactly at midnight (00:00:00)
midnight_created_rows <- hour == 0 & minute == 0 & second == 0
noon_created_rows <- hour == 12 & minute == 0 & second == 0

# Count the number of rows with time exactly at midnight
midnight_created_count <- sum(midnight_created_rows)
noon_created_count <- sum(noon_created_rows)

midnight_created_data <- d311[midnight_created_rows, ]
created_at_midnight <- midnight_created_data[, c("created_date", "agency")]

noon_created_data <- d311[noon_created_rows, ]
created_at_noon <- noon_created_data[, c("created_date", "agency")]

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
    console_print_out_title = "SRs created at midnight"
  )
} else {
  cat("\n\nThere are no SRs with a created_date exactly at midnight.\n")
}

if (noon_created_count > 0) {
  cat(
    "\n\nThere",
    format(noon_created_count, big.mark = ","),
    "SRs that were created exactly at noon."
  )
  
  sorted_create_at_noon <- rank_by_agency(created_at_noon)
  
  chart_title <- "SRs created exactly at noon (12:00:00) by Agency & cumulative percentage"
  chart_file_name <- "SRs_created_at_noon_by_Agency.pdf"
  if (!is.null(sorted_create_at_noon)) {
    create_combo_chart(
      created_at_noon,
      chart_title,
      chart_file_name,
      console_print_out_title = "SRs created at noon"
    )
  } else {
    cat("\n\nThere are no SRs with a created_date exactly at noon.\n")
  }
}

#########################################################################
# Identify SRs closed at midnight and noon

# Remove N/A closed_date(s)
valid_closed_date <- !is.na(d311$closed_date)
valid_closed_data <- d311[valid_closed_date, ]

# Extract hour, minute, and second components of closed_date for valid rows
hour <- as.numeric(format(d311$closed_date[valid_closed_date], "%H"))
minute <- as.numeric(format(d311$closed_date[valid_closed_date], "%M"))
second <- as.numeric(format(d311$closed_date[valid_closed_date], "%S"))

# Identify rows with time exactly at midnight (00:00:00)
midnight_closed_rows <- hour == 0 & minute == 0 & second == 0
noon_closed_rows <- hour == 12 & minute == 0 & second == 0

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
      console_print_out_title = "SRs closed at noon"
    )
  } else {
    cat("\n\nThere are no SRs with a closed_date exactly at noon (12:00:00).\n")
  }
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

