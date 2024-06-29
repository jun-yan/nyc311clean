# install.packages("ggplot2")
# install.packages("scales")
# install.packages("dplyr")
# install.packages('zoo')
# install.packages("ggpmisc")
# install.packages("lubridate")

library(ggplot2)
library(scales)
library(dplyr)
library(zoo)
library(ggpmisc)
library(lubridate)

sink("timeline_console_output.txt")
main_data_file <- "10-year 2014-2023.csv"
data1File <- file.path("..", "..", "data", main_data_file)

#Define the chart directory
chart_directory_path <- file.path("..", "..", "charts", "2022-2023 study", "10-year trends")

file_name_prefix <- sub(" .*", "", main_data_file)

programStart <- as.POSIXct(Sys.time())
formattedStartTime <- format(programStart, "%Y-%m-%d %H:%M:%S")
cat("\nExecution begins at:", formattedStartTime)

#########################################################################
# Create the bar chart
create_bar_chart <- function(
    dataset,
    x_col,
    y_col,
    chart_title,
    sub_title,
    x_axis_title,
    y_axis_title,
    print_out_title,
    add_mean = FALSE,
    add_median = FALSE,
    add_sd = FALSE,
    add_trendline = FALSE,
    add_maximum = FALSE,
    add_minimum = FALSE,
    add_second_maximum = FALSE,
    extra_line = NULL,
    chart_file_name) 
{
  # Find the row with the maximum count
  max_row <- dataset[which.max(dataset[[y_col]]), ]
  y_max_count <- max_row[[y_col]]
  y_total_count <- sum(dataset[[y_col]], na.rm = TRUE)

  # Find the row with the minimum count
  min_row <- dataset[which.min(dataset[[y_col]]), ]
  y_min_count <- round(min_row[[y_col]], 0)

  result <- calculate_values(y_max_count)
  starting_value <- result$starting_value
  increment <- result$increment

  # Calculate mean, median, and standard deviation if requested
  y_mean_value <- round( mean(dataset[[y_col]]), 0 )
  y_median_value <- round( median(dataset[[y_col]]), 0 )
  y_sd_value <- round( sd(dataset[[y_col]]), 0)

  # Print the date and count for maximum value
  cat("\n\n***", print_out_title, "SRs***")

  # Change the first letter to lower case for print out purposes
  print_out_title <- substr(print_out_title, 1, 1) %>%
    tolower() %>% paste(., substr(print_out_title, 2, nchar(print_out_title)), sep = "")

  cat( "\n", paste("Maximum", print_out_title, ":"), as.character(max_row[[x_col]]), "  ",
    paste("Maximum", print_out_title, "count:"), format(y_max_count, big.mark = ",")
  )

  # Print the date and count for minimum value
  cat( "\n", paste("Minimum", print_out_title, ":"), as.character(min_row[[x_col]]), "  ",
    paste("Minimum", print_out_title, "count: "), format(y_min_count, big.mark = ","), "\n"
  )

  # Print the date and count for minimum value
  cat( "\n")
  cat( paste("Average ", print_out_title, ":", sep = ""), format(y_mean_value, big.mark = ","), "  ",
    paste("\nMedian ", print_out_title, ":", sep = ""), format(y_median_value, big.mark = ","),
    paste("\nStd Dev (\u03C3) ", print_out_title, ":", sep = ""), format(y_median_value, big.mark = ",")
  )

  # Create the bar chart
  bar_chart <- ggplot(dataset, aes(x = .data[[x_col]], y = .data[[y_col]])) +
    geom_bar(stat = "identity", fill = "#117733") +
    theme(
      axis.title.x = element_text(vjust = 0, size = 11),
      axis.title.y = element_text(vjust = 1, size = 11),
      plot.title = element_text(hjust = 0.5, size = 13),
      plot.subtitle = element_text(size = 9),
      panel.background = element_rect(fill = "gray95", color = "gray95"),
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, face = "bold"),
      axis.text.y = element_text(face = "bold")
    ) +
    geom_hline( yintercept = seq(starting_value, y_max_count, by = increment),
                linetype = "dotted", color = "gray40", linewidth = 0.3) +
    ggtitle( chart_title, subtitle = paste(sub_title, format(y_total_count, big.mark = ","), sep = "") ) +
    labs(x = x_axis_title, y = NULL)

    if (add_maximum) {
      bar_chart <- bar_chart +
      annotate("text", x = max_row[[x_col]], y = y_max_count,
             label = paste0("Max: ", format(y_max_count, big.mark = ",")), 
             size = 3.7, color = "black", vjust = -0.4, hjust = 0.5)
    }
  
    if (add_second_maximum) {
      # Order the data frame by the count column in descending order
      ordered_by_count <- dataset[order(dataset[[y_col]], decreasing = TRUE), ]
      # Select the second row
      second_max <- ordered_by_count[2, ]
      
      bar_chart <- bar_chart +
      annotate("text", x = second_max[[x_col]], y = second_max[[y_col]],
               label = paste0("2^nd~'Highest: ", format(second_max[[y_col]], big.mark = ","), "'"), , 
               size = 3.7, color = "black", vjust = -0.4, hjust = 0.05, parse = TRUE)
    }
  
    if (add_minimum) { 
      bar_chart <- bar_chart + 
      annotate("text", x = min_row[[x_col]], y = y_min_count,
             label = paste0("Min: ", format(y_min_count, big.mark = ",")),
             size = 3.7, color = "black", vjust = -0.4, hjust = 0.5 )
    }

    if (!is.null(starting_value) && !is.null(y_max_count) && !is.null(increment)) {
    bar_chart <- bar_chart +
      geom_hline(yintercept = seq(starting_value, y_max_count, by = increment), 
                 linetype = "dotted", color = "gray40")
    }

    if (add_mean) {
      bar_chart <- bar_chart +
        geom_hline(yintercept = y_mean_value, linetype = "twodash", color = "black", linewidth = 0.6) +
          annotate("text", x = min(dataset[[x_col]]), y = y_mean_value, 
               label = paste0("Average: ", format(round(y_mean_value, 0), big.mark = ",")), 
                              size = 4, color = "black", hjust = -0.5, vjust = -0.75)
    }

    if (add_median) {
      bar_chart <- bar_chart +
        geom_hline(yintercept = y_median_value, linetype = "twodash", color = "black", linewidth = 0.6) +
        annotate("text", x = min(dataset[[x_col]]), y = y_median_value, 
               label = paste0("Median: ", format(round(y_median_value,0), big.mark = ",")), 
                              size = 4, color = "black",hjust = -0.5, vjust = -0.75)
    }

    if (add_sd) {
    bar_chart <- bar_chart +
      geom_hline(yintercept = round(y_mean_value + 3*y_sd_value, 0), linetype = "longdash", 
                 color = "black", linewidth = 0.3) +
      annotate("text", x = min(dataset[[x_col]]), y = y_mean_value + 3*y_sd_value, 
        label = "+3 sigma", size = 3.5, color = "black", hjust = -0.5, vjust = -0.75)
    }

    if(add_trendline) {
    bar_chart <- bar_chart +
      stat_poly_eq( color = "#D55E00" ) +
      geom_smooth(method = "lm", span = 1, se = FALSE, color = "#D55E00", 
                  linetype = "dashed", linewidth = 0.9)
    }

    if (!is.null(extra_line)) {
    bar_chart <- bar_chart + extra_line
    }

  # Print the bar chart
  suppressMessages(print(bar_chart))
  chart_path <- file.path(chart_directory_path, chart_file_name)
  suppressMessages(ggsave(chart_path, plot = bar_chart, width = 10, height = 8))
}

#########################################################################
calculate_values <- function(max_count) {
  # Set scipen option to a large value
  options(scipen = 999)
  thresholds <- c(10, 
                  100, 
                  250, 
                  500, 
                  1000, 
                  2500, 
                  5000, 
                  10000,
                  15000,
                  25000, 
                  50000,
                  100000, 
                  500000, 
                  1000000, 
                  2500000, 
                  5000000, 
                  10000000,
                  50000000, 
                  100000000)
  
  values <- matrix(c(0, 1, 1,
                     0, 20, 1,
                     50, 50, 1,
                     100, 100, 1,
                     100, 200, 1,
                     250, 250, 1,
                     1000, 1000, 1,
                     1000, 2000, 1000,
                     2500, 2500, 1000,
                     5000,5000,1000,
                     5000, 10000, 100,
                     10000, 20000, 1000,
                     100000, 100000, 1000,
                     100000, 200000, 1000,
                     250000, 250000, 1000,
                     500000, 500000, 1000,
                     1000000, 1000000, 1000000,
                     5000000, 5000000, 1000000,
                     10000000, 20000000, 1000000), ncol = 3, byrow = TRUE)
  
  colnames(values) <- c("starting_value", "increment", "scaling_factor")
  
  index <- findInterval(max_count, thresholds)
  index <- index +1
  
  # Use format() to prevent scientific notation
  result <- lapply(values[index, ], format, scientific = FALSE)
  
  # Return the calculated values as a named list
  result <- as.list(values[index, ])
  return(result)
}

#########################################################################
cat("\n\n**********DATA INPUT AND PREPARATION**********\n")

# Load the main 311 SR data file. Set the read & write paths.
d311 <-
  read.csv(data1File,
           header = TRUE,
           colClasses = rep("character", ncol(read.csv(data1File)))
  )

colnames(d311) <- gsub("Created.Date", "created_date", colnames(d311))
colnames(d311) <- gsub("Closed.Date", "closed_date", colnames(d311))

# Filter out rows with NA values in the created_date column
d311 <- d311[!is.na(d311$created_date),]

#########################################################################
cat("\n\n**********DATA SUMMARY**********\n")

# Convert character date-time strings to datetime objects with America/New_York timezone
d311$created_date <- as.POSIXct(d311$created_date, format = "%m/%d/%Y %I:%M:%S %p", tz = "America/New_York")
d311$closed_date <- as.POSIXct(d311$closed_date, format = "%m/%d/%Y %I:%M:%S %p", tz = "America/New_York")

d311 <- d311[!is.na(d311$created_date),]
num_years <- unique(d311$Year)
numrows <- nrow(d311)

cat("\nTotal rows:", format(numrows, big.mark = ","), "covering", length(num_years),"years")

#########################################################################
earliest_date <- min(d311$created_date, na.rm = TRUE)
earliest_date_formatted <- format(earliest_date, format = "%Y-%m-%d %H:%M:%S")

latest_date <- max(d311$created_date, na.rm = TRUE)
latest_date_formatted <- format(latest_date, format = "%Y-%m-%d %H:%M:%S")

earliest_title <- format(as.Date(earliest_date_formatted), format = "%Y-%m-%d")
latest_title <- format(as.Date(latest_date_formatted), format = "%Y-%m-%d")

cat("\nData contains SRs created from", earliest_date_formatted, "through", latest_date_formatted)

#########################################################################
# Aggregate at the yearly level
yearly_summary <- d311 %>%
  mutate(Year = format(created_date, "%Y")) %>%
  group_by(Year) %>%
  summarise(count = n())

max_rows_yearly <- max(yearly_summary$count)
yearly_summary <- na.omit(yearly_summary)
cat("\nYearly Summary complete.")

#########################################################################
# Aggregate at the monthly level
monthly_summary <- d311 %>%
  mutate(YearMonth = format(created_date, "%Y-%m")) %>%
  group_by(YearMonth) %>%
  summarise(count = n())
  
max_rows_monthly <- max(monthly_summary$count)
monthly_summary <- na.omit(monthly_summary)
cat("\nMonthly Summary complete.")

########################################################################                       
# Aggregate at the daily level
daily_summary <- d311 %>%
  mutate(created_date = date(created_date)) %>%  # Extract date part from datetime_col
  group_by(created_date) %>%                     # Group by the extracted date
  summarise(count = n())                 # Summarize by counting the number of entries per day

max_rows_daily <- max(daily_summary$count)
daily_summary <- na.omit(daily_summary)
cat("\nDaily Summary complete.")

########################################################################                       
# Aggregate by calendar month
calendar_month_summary <- d311 %>%
  group_by(Month = format(created_date, "%B")) %>%
  summarise(count = n())

max_rows_calendar_month <- max(calendar_month_summary$count)
calendar_month_summary <- na.omit(calendar_month_summary)

# Convert Month to factor with custom order
calendar_month_summary$Month <- factor(
  calendar_month_summary$Month,
  levels = month.name
)

# Order the data by month
calendar_month_summary <- calendar_month_summary[order(calendar_month_summary$Month), ]
cat("\nCalendar Month Summary complete.")

#########################################################################
#Aggregate by day of the year, e.g. Jan 1st, July 4th, Dec 25th, etc.
# Extract day of the year
d311$day_of_year <- format(d311$created_date, "%m/%d")

# Count occurrences of each day of the year
day_counts <- d311 %>%
  group_by(day_of_year) %>%
  summarise(count = n())
cat("\nDay of Year Summary complete.")

#########################################################################
# Aggregate by day of the week
day_of_week <- d311 %>%
  mutate(day_of_week = weekdays(created_date)) %>%
  group_by(day_of_week) %>%
  summarize(count = n()) %>%
  arrange(match(day_of_week, c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>%
  mutate(day_of_week = recode(day_of_week,
                          "Monday" = "1-Monday",
                          "Tuesday" = "2-Tuesday",
                          "Wednesday" = "3-Wednesday",
                          "Thursday" = "4-Thursday",
                          "Friday" = "5-Friday",
                          "Saturday" = "6-Saturday",
                          "Sunday" = "7-Sunday"
  ))
cat("\nDay of Week Summary complete.")

#########################################################################
# Aggregate by hour of the day
# Extract hour part from created_date and closed_date
d311$created_hour <- hour(d311$created_date)
d311$closed_hour <- hour(d311$closed_date)

# Aggregate the rows by closed_hour, excluding NA values
created_counts <- d311 %>%
  filter(!is.na(created_hour)) %>%
  group_by(created_hour) %>%
  summarise(count = n())

# Aggregate the rows by closed_hour, excluding NA values
closed_counts <- d311 %>%
  filter(!is.na(closed_hour)) %>%
  group_by(closed_hour) %>%
  summarise(count = n())

cat("\nHourly Summaries complete.")

#########################################################################
yearly_df <- as.data.frame(yearly_summary)

monthly_df <- as.data.frame(monthly_summary)

daily_df <- as.data.frame(daily_summary)

calendar_month_df <- as.data.frame(calendar_month_summary)

day_counts_df <- as.data.frame(day_counts)

day_of_week_df <- as.data.frame(day_of_week)

created_hour_of_day_df <- as.data.frame((created_counts))

closed_hour_of_day_df <- as.data.frame((closed_counts))

cat("\nDataframes complete.")

#########################################################################
# Overall yearly summary
mean_count <- round(mean(yearly_df$count), 0)
median_count <- round(median(yearly_df$count), 0)
standard_deviation <- round(sd(yearly_df$count), 0)

cat("\n\nAverage yearly count:", format(mean_count, big.mark = ","))
cat("\nStandard deviation:", format(standard_deviation, big.mark = ","))
cat("\nMedian:", format(median_count, big.mark = ","))

max_year <- yearly_df[which.max(yearly_df$count), ]
max_count <- max_year$count
cat("\nYear with maximum count:",max_year$Year, "with", format(max_count, big.mark = ","), "SRs")

min_year <- yearly_df[which.min((yearly_df$count)),]
cat("\nYear with minimum count:", min_year$Year, 
    "with", format(min_year$count, big.mark = ","), "SRs\n")

# Extract the earliest and latest year counts
earliest_year_count <- yearly_df %>% filter(Year == min(Year)) %>% select(count) %>% pull()
latest_year_count <- yearly_df %>% filter(Year == max(Year)) %>% select(count) %>% pull()

# Compute the percentage growth
percentage_growth <- round(((latest_year_count - earliest_year_count) / earliest_year_count) * 100, 1)

cat("\nGrowth over", nrow(yearly_df), "years is", percentage_growth, "%" )

cat("\nYearly Summary:\n")
print(yearly_df, row.names = FALSE, right = FALSE)

yearly_df$Year <- as.numeric(yearly_df$Year)
earliest_year <- min(yearly_df$Year)

chart_sub_title <- paste("(", earliest_title, "--", latest_title, ") total=", sep = "")

# Define the extra line
extra_line <- annotate("text", x = as.numeric(max_year$Year), y = max_year$count,
                       label = paste0("10-yr growth: ", percentage_growth, "%", sep = ""), 
                       size = 3.7, color = "#D55E00", vjust = -2, hjust = 5.75)
SR_yearly <- create_bar_chart(
  yearly_df,
  x_col = "Year",
  y_col = "count",
  chart_title = "Yearly SR count (w/trendline)",
  sub_title = chart_sub_title,
  x_axis_title = NULL,
  y_axis_title = NULL,
  print_out_title = "Yearly",
  add_mean = TRUE,
  add_median = FALSE,
  add_sd = FALSE,
  add_trendline = TRUE,
  add_maximum = FALSE,
  add_minimum = FALSE,
  add_second_maximum = FALSE,
  extra_line = extra_line,
  chart_file_name = paste0(file_name_prefix, "-yearly.pdf")
  )

#########################################################################
# Overall monthly summary
mean_count <- round(mean(monthly_df$count), 0)
median_count <- round(median(monthly_df$count), 0)
standard_deviation <- round(sd(monthly_df$count), 0)

cat("\nAverage monthly:", format(mean_count, big.mark = ","), 
    "  Standard deviation of", format(standard_deviation, big.mark = ","))

cat("\nMedian:", format(median_count, big.mark = ","))

# Append '-01' to the YearMonth to create a full date string
monthly_df$YearMonth <- paste0(monthly_df$YearMonth, "-01")

# Convert the YearMonth column to Date format
 monthly_df$YearMonth <- as.Date(monthly_df$YearMonth)
 
# Format the YearMonth column to just show year and month
 monthly_df$YearMonth <- format(monthly_df$YearMonth, "%Y-%m")

max_month <- monthly_df[which.max(monthly_df$count), ]
cat("\nMonth with maximum count:", max_month$YearMonth, 
    "with", format(max_month$count, big.mark = ","), "SRs")

min_month <- monthly_df[which.min((monthly_df$count)),]
cat("\nMonth with minimum count:", min_month$YearMonth, 
    "with", format(min_month$count, big.mark = ","), "SRs\n")

monthly_df <- monthly_df[order(-monthly_df$count),]
cat("\nMonthly Summary (top 10):\n")
print(head(monthly_df, 10), row.names = FALSE, right = FALSE)

monthly_df <- monthly_df[order(monthly_df$count),]
cat("\nMonthly Summary (bottom 10):\n")
print(head(monthly_df, 10), row.names = FALSE, right = FALSE)

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

extra_line1 <- scale_x_date(labels = date_format("%Y-%m"), 
                  breaks = seq(min(monthly_df$YearMonth), max(monthly_df$YearMonth), by = "6 months"), 
                                                 expand = c(0, 0))  

extra_line2 <- annotate("text", x = max_month$YearMonth, y = max_month$count,
                        label = paste0("10-yr growth: ", percentage_growth, "%", sep = ""), 
                        size = 3.7, color = "#D55E00", vjust = -1, hjust = 4)

SR_monthly <- create_bar_chart(
  monthly_df,
  x_col = "YearMonth",
  y_col = "count",
  chart_title = "Monthly SR count",
  sub_title = chart_sub_title,
  x_axis_title = NULL,
  y_axis_title = "count",
  print_out_title = "Monthly",
  add_mean = TRUE,
  add_median = FALSE,
  add_sd = FALSE,
  add_trendline = TRUE,
  add_maximum = TRUE,
  add_minimum = FALSE,
  add_second_maximum = FALSE,
  extra_line = extra_line2, 
  chart_file_name = paste0(file_name_prefix, "-monthly.pdf")
)

#########################################################################
# Overall daily summary
mean_count <- round(mean(daily_df$count), 0)
median_count <- round(median(daily_df$count), 0)
standard_deviation <- round(sd(daily_df$count), 0)

cat("\nAverage daily count:", format(mean_count, big.mark = ","), 
    "  Standard deviation:", format(standard_deviation, big.mark = ",")) 

cat("\nMedian:", format(median_count, big.mark = ","))

daily_df$created_date <- as.character(daily_df$created_date)

# Convert created_date to Date format
daily_df$created_date <- as.Date(daily_df$created_date)

# Sort the dataframe by count in descending order
sorted_df <- daily_df[order(-daily_df$count), ]

# Select the top ten rows
top_ten <- sorted_df[1:10, ]

# Print the top ten counts
cat("\n\nTop ten counts by date:\n")
for (i in 1:nrow(top_ten)) {
  cat( format(top_ten$created_date[i], "%Y-%m-%d"), "  ", format(top_ten$count[i], big.mark = ","), "  ", i, "\n")
}

# Sort the dataframe by count in ascending order
sorted_df <- daily_df[order(daily_df$count), ]

# Select the bottom ten rows
bottom_ten <- head(sorted_df, 10)

# Print the bottom ten counts
cat("\nBottom ten counts by date:\n")
for (i in 1:nrow(bottom_ten)) {
  cat( format(bottom_ten$created_date[i], "%Y-%m-%d"), "   ", format(bottom_ten$count[i], big.mark = ","), "    ", i, "\n")
}

# Find corresponding dates
max_date <- daily_df$created_date[which.max(daily_df$count)]
min_date <- daily_df$created_date[which.min(daily_df$count)]

daily_df <- daily_df[order(daily_df$created_date), ]
daily_df$created_date <- as.Date(daily_df$created_date)

extra_line <- scale_x_date(expand = c(0, 0), labels = scales::date_format("%Y-%m"), 
                           breaks = scales::date_breaks("6 months"))
SR_daily <- create_bar_chart(
  daily_df,
  x_col = "created_date",
  y_col = "count",
  chart_title = "Daily SR count (w/trendline)",
  sub_title = chart_sub_title,
  x_axis_title = NULL,
  y_axis_title = "count",
  print_out_title = "Daily",
  add_mean = FALSE,
  add_median = TRUE,
  add_sd = TRUE,
  add_trendline = FALSE,
  add_maximum = TRUE,
  add_minimum = FALSE,
  add_second_maximum = FALSE,
  extra_line = extra_line,
  chart_file_name = paste0(file_name_prefix, "-trend-daily.pdf")
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

mean_count <- round(mean(grouped_by_hour$count), 0)
median_count <- round(median(grouped_by_hour$count), 0)
standard_deviation <- round(sd(grouped_by_hour$count), 0)

total_count <- sum(grouped_by_hour$count)

cat("\nAverage top-of-the-hour 'created' count:", format(mean_count, big.mark = ","), 
    "  Standard deviation:", format(standard_deviation, big.mark = ","))
cat("\nMedian:", format(median_count, big.mark = ","))

max_hour_of_the_day <- grouped_by_hour[which.max(grouped_by_hour$count), ]
max_hour <- as.character(max_hour_of_the_day$hour)
max_count <- max_hour_of_the_day$count

cat("\nTop of the hour with maximum 'created' count is hour #:", max_hour, 
    "with", format(max_count, big.mark = ","), "SRs")

min_hour_of_the_day <- grouped_by_hour[which.min(grouped_by_hour$count), ]
min_hour_of_the_day$hour <- as.character(min_hour_of_the_day$hour)
cat("\nTop of the Hour with the minimum 'created' count is hour #:", min_hour_of_the_day$hour, 
    "with", format(min_hour_of_the_day$count, big.mark = ","), "SRs\n")

extra_line <- annotate("text", x = 0, y = median_count, 
                       label = paste0("Median: ", round(median_count, 0)), 
                       size = 3.5, hjust = -0.5, vjust = -0.75)
SR_created_by_top_of_hour <- create_bar_chart(
  grouped_by_hour,
  x_col = "hour",
  y_col = "count",
  chart_title = "SRs 'created' Exactly on the Hour (zero minutes/zero seconds)",
  sub_title = chart_sub_title,
  x_axis_title = "hour-of-the-day (0-23)",
  y_axis_title = "count",
  print_out_title = "Top-of-Hour",
  add_mean = FALSE,
  add_median = FALSE,
  add_sd = TRUE,
  add_trendline = FALSE,
  add_maximum = TRUE,
  add_minimum = FALSE,
  add_second_maximum = TRUE,
  extra_line = extra_line,
  chart_file_name = paste0(file_name_prefix, "-trend-SR_created_by_top_of_hour.pdf")
)

#########################################################################
# Show minute-by-minute creation of SRs on the busiest day of the year.
# Ensure created_date is in the correct time zone
d311 <- d311 %>%
  mutate(created_date = with_tz(created_date, tzone = "America/New_York"))

# Filter the data for the desired date and seconds value "00"
date_to_filter <- max_date
date_filtered <- d311 %>%
  filter(as_date(created_date, tz = "America/New_York") == ymd(date_to_filter, tz = "America/New_York"),
         second(created_date) == 0)

# Group by hour and minute, count the rows
minute_counts <- date_filtered %>%
  group_by(hour = hour(created_date), minute = minute(created_date)) %>%
  summarise(count = n(), .groups = 'drop')

mean_count <- round(mean(minute_counts$count), 0)
median_count <- round(median(minute_counts$count), 0)
standard_deviation <- round(sd(minute_counts$count), 0)

total_count <- sum(minute_counts$count)

# Calculate max values for annotation
max_hour_of_the_day <- minute_counts[which.max(minute_counts$count), ]
max_hour_minute_of_the_date <- paste0(sprintf("%02d", max_hour_of_the_day$hour), ":", 
                                      sprintf("%02d", max_hour_of_the_day$minute, sep = ""))
max_value <- max(minute_counts$count)

max_hour_and_minute <- paste0(max_hour_of_the_day$hour, ":", 
                              max_hour_of_the_day$minute, "0", sep = "")

# Create unique hour:minute labels
# minute_counts <- minute_counts %>%
#   mutate(hour_minute = paste0(sprintf("%02d", hour), ":", sprintf("%02d", minute), sep = ""))

# Assuming 'minute_counts' is your data frame with 'hour' and 'minute' columns
minute_counts <- minute_counts %>%
  mutate(hour_minute = sprintf("%02d:%02d", hour, minute))

# Convert hour_minute to a time format
minute_counts$hour_minute <- as.POSIXct(minute_counts$hour_minute, format = "%H:%M")
earliest_hour_minute <- min(minute_counts$hour_minute)

extra_line <- scale_x_datetime(expand = c(0.025, 0.025), date_labels = "%H:%M", breaks = "2 hour")
SR_created_by_minute_of_busiest_day <- create_bar_chart(
  minute_counts,
  x_col = "hour_minute",
  y_col = "count",
  chart_title = paste("SRs 'created' by Exact Minute-of-the-Day (00 secs) on", max_date),
  sub_title = chart_sub_title,
  x_axis_title = NULL,
  y_axis_title = "count",
  print_out_title = "By Minute (00 sec)",
  add_mean = FALSE,
  add_median = FALSE,
  add_sd = TRUE,
  add_trendline = FALSE,
  add_maximum = TRUE,
  add_minimum = FALSE,
  add_second_maximum = TRUE,
  extra_line = extra_line,
  chart_file_name = paste0(file_name_prefix, "-trend-SR_created_by_minute_of_busiest_day.pdf")
)

#########################################################################
# Determine the # of SRs closed exactly on hour with minute and seconds == 00.
# Looking for an anomaly at midnight and noon. 
# Filter rows where the created_date has seconds = "00" and minutes = "00"
filtered_by_hour <- d311 %>%
  filter(!is.na(closed_date) & minute(closed_date) == 0 & second(closed_date) == 0)

grouped_by_hour <- filtered_by_hour %>%
  group_by(hour = hour(closed_date)) %>%
  summarise(count = n())

mean_count <- round(mean(grouped_by_hour$count), 0)
median_count <- round(median(grouped_by_hour$count, 0))
standard_deviation <- round(sd(grouped_by_hour$count), 0)

total_count <- sum(grouped_by_hour$count)

cat("\nAverage top-of-the-hour 'closed' count:", format(mean_count, big.mark = ","), 
    "  Standard deviation:", format(standard_deviation, big.mark = ","))
cat("\nMedian:", format(median_count, big.mark = ","))

max_hour_of_the_day <- grouped_by_hour[which.max(grouped_by_hour$count), ]
max_hour <- as.character(max_hour_of_the_day$hour)
max_count <- max_hour_of_the_day$count

# Order the data frame by the count column in descending order
ordered_by_count <- grouped_by_hour[order(-grouped_by_hour$count), ]
# Select the second row
second_max_hour_of_the_day <- ordered_by_count[2, ]
second_max_hour <- as.character(second_max_hour_of_the_day$hour)
second_max_count <- second_max_hour_of_the_day$count

cat("\nTop of the hour with maximum 'closed' count is hour #:", max_hour, 
    "with", format(max_count, big.mark = ","), "SRs")

cat("\nTop of the hour with the 2nd most 'closed' count is hour #:", second_max_hour, 
    "with", format(second_max_count, big.mark = ","), "SRs")

min_hour_of_the_day <- grouped_by_hour[which.min(grouped_by_hour$count), ]
min_hour_of_the_day$hour <- as.character(min_hour_of_the_day$hour)
cat("\nTop of the Hour with the minimum 'created' count is hour #:", min_hour_of_the_day$hour, 
    "with", format(min_hour_of_the_day$count, big.mark = ","), "SRs\n")

SR_closed_by_top_of_hour <- create_bar_chart(
  grouped_by_hour,
  x_col = "hour",
  y_col = "count",
  chart_title = "SRs 'closed' Exactly on the Hour (zero minutes/zero seconds",
  sub_title = chart_sub_title,
  x_axis_title = NULL,
  y_axis_title = "count",
  print_out_title = "By Minute (00 sec)",
  add_mean = FALSE,
  add_median = FALSE,
  add_sd = TRUE,
  add_trendline = FALSE,
  add_maximum = TRUE,
  add_minimum = FALSE,
  extra_line = NULL,
  add_second_maximum = TRUE,
  chart_file_name = paste0(file_name_prefix, "-trend-SR_closed_by_top_of_hour.pdf")
)

#########################################################################
# Show minute-by-minute closure of SRs on the busiest day of the year.
# Ensure created_date is in the correct time zone
d311 <- d311 %>%
  mutate(closed_date = with_tz(closed_date, tzone = "America/New_York"))

# Filter the data for the desired date and seconds value "00"
date_to_filter <- max_date

date_filtered <- d311 %>%
  filter(as_date(closed_date, tz = "America/New_York") == ymd(date_to_filter, tz = "America/New_York"),
         second(closed_date) == 0 & !is.na(closed_date))

# Group by hour and minute, count the rows
minute_counts <- date_filtered %>%
  group_by(hour = hour(closed_date), minute = minute(closed_date)) %>%
  summarise(count = n(), .groups = 'drop')

mean_count <- round(mean(minute_counts$count), 0)
median_count <- round(median(minute_counts$count), 0)
standard_deviation <- round(sd(minute_counts$count), 0)

total_count <- sum(minute_counts$count)

# Calculate max values for annotation
max_hour_of_the_day <- minute_counts[which.max(minute_counts$count), ]
max_hour_minute_of_the_date <- paste0(sprintf("%02d", max_hour_of_the_day$hour), ":", sprintf("%02d", max_hour_of_the_day$minute))
max_value <- max(minute_counts$count)

max_hour_minute_of_the_day <- paste0(max_hour_of_the_day$hour, "0:", max_hour_of_the_day$minute, "0")

# # Calculate 2nd max values for annotation
# # Order the data frame by the count column in descending order
# ordered_by_count <- minute_counts[order(-minute_counts$count), ]
# # Select the second row
# second_max_hour_of_the_day <- ordered_by_count[2, ]
# 
# second_max_hour_minute_of_the_date <- paste0(sprintf("%02d", second_max_hour_of_the_day$hour), ":", sprintf("%02d", second_max_hour_of_the_day$minute))
# 
# second_max_hour <- as.character(second_max_hour_of_the_day$hour)
# second_max_count <- second_max_hour_of_the_day$count
# 
# # Create unique hour:minute labels
# minute_counts <- minute_counts %>%
#  mutate(hour_minute = paste0(sprintf("%02d", hour), ":", sprintf("%02d", minute)))

# Assuming 'minute_counts' is your data frame with 'hour' and 'minute' columns
minute_counts <- minute_counts %>%
  mutate(hour_minute = sprintf("%02d:%02d", hour, minute))

# Convert hour_minute to a time format
minute_counts$hour_minute <- as.POSIXct(minute_counts$hour_minute, format = "%H:%M")
earliest_hour_minute <- min(minute_counts$hour_minute)

extra_line <- scale_x_datetime(expand = c(.03, .03), date_labels = "%H:%M", breaks = "2 hour")
SR_closed_by_minute_of_busiest_day <- create_bar_chart(
  minute_counts,
  x_col = "hour_minute",
  y_col = "count",
  chart_title = paste("SRs 'closed' by Exact Minute-of-the-Day (00:00) on", max_date),
  sub_title = chart_sub_title,
  x_axis_title = NULL,
  y_axis_title = "count",
  print_out_title = "Minute",
  add_mean = FALSE,
  add_median = FALSE,
  add_sd = TRUE,
  add_trendline = FALSE,
  add_maximum = TRUE,
  add_minimum = FALSE,
  add_second_maximum = TRUE,
  extra_line = extra_line,
  chart_file_name = paste0(file_name_prefix, "-trend-SR_closed_by_minute_of_busiest_day.pdf")
)

#########################################################################
# Overall calendar month summary (Jan, Feb, Mar, etc.)
mean_count <- round(mean(calendar_month_df$count), 0)
median_count <- round(median(calendar_month_df$count), 0)
standard_deviation <- round(sd(calendar_month_df$count), 0)

cat("\nAverage calendar month count:", format(mean_count, big.mark = ","), 
    "  Standard deviation:", format(standard_deviation, big.mark = ","))
cat("\nMedian:", format(median_count, big.mark = ","))

# Number of days in each month, with February as 28.2 days
days_in_month <- c(
  "January" = 31, "February" = 28.2, "March" = 31, "April" = 30, 
  "May" = 31, "June" = 30, "July" = 31, "August" = 31, 
  "September" = 30, "October" = 31, "November" = 30, "December" = 31
)

max_calendar_month <- calendar_month_df[which.max(calendar_month_df$count), ]
max_calendar_month_name <- as.character(calendar_month_df$Month[which.max(calendar_month_df$count)])
max_calendar_month$Month <- as.character(month.name[max_calendar_month$Month])
max_calendar_count <- max_calendar_month$count

# Get the number of days for the max_calendar_month
days_for_max_month <- days_in_month[max_calendar_month$Month]
cat("\n\nCalendar month with maximum count:", max_calendar_month$Month, 
    "with", format(max_calendar_count, big.mark = ","), "SRs")
cat("\nPer day count (maximum):", round(max_calendar_month$count/days_for_max_month, 0) )

min_calendar_month <- calendar_month_df[which.min((calendar_month_df$count)),]
min_calendar_month_name <- as.character(calendar_month_df$Month[which.min(calendar_month_df$count)])
min_calendar_month$Month <- as.character(month.name[min_calendar_month$Month])
min_calendar_count <- min_calendar_month$count

# Get the number of days for the min_calendar_month
days_for_min_month <- days_in_month[min_calendar_month$Month]
cat("\n\nCalendar month with minimum count:", min_calendar_month$Month, 
    "with", format(min_calendar_count, big.mark = ","), "SRs")
cat("\nPer day count (minimum):", round(min_calendar_month$count/days_for_min_month, 0) )

# Add the count_per_day column
calendar_month_df$count_per_day <- round(calendar_month_df$count / days_in_month[calendar_month_df$Month],0)

# Order by calendar month (Jan, Feb, etc.)
cat("\n\nSRs by calendar month, ordered by calendar\n")
print(calendar_month_df, row.names = FALSE, right = FALSE)

# Order the dataframe by total count
calendar_month_df <- calendar_month_df %>%
  arrange(desc(count))
cat("\nSRs by calendar month, ordered by count\n")
print(calendar_month_df, row.names = FALSE, right = FALSE)

# Order the dataframe by count_per_day
calendar_month_df <- calendar_month_df %>%
  arrange(desc(count_per_day))
cat("\nSRs by calendar month, ordered by SRs/day\n")
print(calendar_month_df, row.names = FALSE, right = FALSE)

# Chart SRs by calendar month
max_count <- max(calendar_month_df$count)
total_count <- sum(calendar_month_df$count)

earliest_month <- "January"

result <- calculate_values(max_count)
starting_value <- result$starting_value
increment <- result$increment

# Create the bar chart with vertical X-axis labels
SR_calendar_month <- ggplot(calendar_month_df, aes(x = Month, y = count)) +
  geom_bar(stat = "identity", fill = "#117733") +
  scale_x_discrete() +
  theme(
    axis.title.x = element_text(vjust = 0, size = 11),
    axis.title.y = element_text(vjust = 1, size = 11),
    plot.title = element_text(hjust = 0.5, size = 13),
    plot.subtitle = element_text(size = 9),
    panel.background = element_rect(fill = "gray95", color = "gray95"),
    axis.text.x = element_text(angle = 60, vjust = 0.5, hjust = 0.5, face = "bold"),
    axis.text.y = element_text(face = "bold")
  ) +
  ggtitle("SR count by Calendar Month",
          subtitle = paste("(", earliest_title, "--", latest_title, ") ", 
                           "total=", format(total_count, big.mark = ","), sep = "")
  ) +
  geom_hline(
    yintercept = seq(starting_value, max_count, by = increment),
    linetype = "dotted", color = "gray40"
  ) +
  geom_hline(yintercept = mean_count, linetype = "twodash", color = "black", linewidth = 0.6) +
  annotate("text", x = earliest_month, y = mean_count, 
           label = paste0("Average: ", format(round(mean_count, 0), big.mark = ",")), 
           size = 4, hjust = -0.5, vjust = -0.75) +
  # Add annotations for min and max values
  # annotate("text", x = min_calendar_month_name, y = min_calendar_count,
  #          label = paste0("Min: ", format(min_calendar_count, big.mark = ",")),
  #          size = 4, color = "black", vjust = -0.6, hjust = 0.3 ) +
  annotate("text", x = max_calendar_month_name, y = max_calendar_count,
           label = paste0("Max: ", format(max_calendar_count, big.mark = ",")), 
           size = 4, color = "black", vjust = -0.6, hjust = 0.3) +
  labs(x=NULL, y=NULL)

# Print the bar chart
suppressMessages(print(SR_calendar_month))
chart_path <- file.path(chart_directory_path, paste0(file_name_prefix, "-trend-Calendar-Month.pdf"))
suppressMessages(ggsave(chart_path, plot = SR_calendar_month, width = 10, height = 8))

#########################################################################
# Overall day-of-the-year summary (Jan 1st, Jan 2nd, Jan 3rd, etc.)
mean_count <- round(mean(day_counts_df$count), 0)
median_count <- round(median(day_counts_df$count), 0)
standard_deviation <- round(sd(day_counts_df$count, 0))

cat("\nAverage day-of-the-year count:", format(mean_count, big.mark = ","), 
    "  Standard deviation:", format(standard_deviation, big.mark = ","))
cat("\nMedian:", format(median_count, big.mark = ","))
# cat("\nMaximum is", format(max(day_counts_df$count), big.mark = ","),
#     "and Minimum is", format(min(day_counts_df$count), big.mark = ","))

max_day_of_the_year <- day_counts_df[which.max(day_counts_df$count), ]
max_day_of_the_year$day_of_year <- as.character(max_day_of_the_year$day_of_year)
cat("\nDay of the year with maximum count:", max_day_of_the_year$day_of_year, 
    "with", format(max_day_of_the_year$count, big.mark = ","), "SRs")

min_day_of_the_year <- day_counts_df[which.min(day_counts_df$count), ]
min_day_of_the_year$DayOfWeek <- as.character(min_day_of_the_year$day_of_year)
cat("\nDay of the year with minimum count:", min_day_of_the_year$day_of_year, 
    "with", format(min_day_of_the_year$count, big.mark = ","), "SRs\n")

# Sort the dataframe by count in ascending order
sorted_day_counts_df <- day_counts_df[order(day_counts_df$count), ]

# Get the 2nd minimum day of the year
second_min_day_of_the_year <- sorted_day_counts_df[2, ]

# Add the day of the week as a character
second_min_day_of_the_year$DayOfWeek <- as.character(second_min_day_of_the_year$day_of_year)

# Print the result
cat("\nDay of the year with the 2nd minimum count:", second_min_day_of_the_year$day_of_year, 
    "with", format(second_min_day_of_the_year$count, big.mark = ","), "SRs\n")

cat("\nDay-of-the-Year Summary:")

# Convert day_of_year to character
day_counts_df$day_of_year <- as.character(day_counts_df$day_of_year)

# Format the columns for better alignment
formatted_day_counts_df <- data.frame(
  day_of_year = format(day_counts_df$day_of_year, width = max(nchar(day_counts_df$day_of_year))),
  count = format(day_counts_df$count, width = max(nchar(day_counts_df$count)))
)

day_counts_df <- day_counts_df[order(-day_counts_df$count),]

# Print the Day-of-the-Calendar Year Summary with custom order
cat("\nTop 10 days\n")
print(head(day_counts_df, n = 10), right = FALSE)

day_counts_df <- day_counts_df[order(day_counts_df$count),]

cat("\nBottom 10 days\n")
print(head(day_counts_df,  n= 10), right = FALSE)

# Chart SRs by day of the calendar year
max_count <- max(day_counts_df$count)
earliest_day_of_year <- min(day_counts$day_of_year)

result <- calculate_values(max_count)
starting_value <- result$starting_value
increment <- result$increment

# Convert day_of_year to a factor
day_counts_df$day_of_year <- as.factor(day_counts_df$day_of_year)

# Create the bar chart with vertical X-axis labels
SR_day_of_the_year <- ggplot(day_counts_df, aes(x = day_of_year, y = count)) +
  geom_bar(stat = "identity", fill = "#117733") +
  scale_y_continuous(labels = scales::comma) +
  theme(
    axis.title.x = element_text(vjust = 0, size = 11),
    axis.title.y = element_text(vjust = 1, size = 11),
    plot.title = element_text(hjust = 0.5, size = 13),
    plot.subtitle = element_text(size = 9),
    panel.background = element_rect(fill = "gray95", color = "gray95"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5, face = "bold"),
    axis.text.y = element_text(face = "bold")
  ) +
  ggtitle("SR count by Day-of-the-Year",
          subtitle = paste("(", earliest_title, "--", latest_title, ")", 
                           "total=", format(total_count, big.mark = ","))
  ) +
  geom_hline(
    yintercept = seq(starting_value, max_count, by = increment),
    linetype = "dotted", color = "gray40"
  ) +
  geom_hline(yintercept = mean_count, linetype = "twodash", color = "black", linewidth = 0.6) +
  annotate("text", x = earliest_day_of_year, y = mean_count, 
           label = paste0( "Average: ", format(round(mean_count, 0), big.mark = "," )), 
           size = 4, hjust = -0.5, vjust = -0.75) +
  annotate("text", x = max_day_of_the_year$day_of_year, y = max_day_of_the_year$count,
           label = paste0("Max: ", format(max_day_of_the_year$count, big.mark = ",")), 
           size = 4, color = "black", vjust = -0.6, hjust = 0.3) +
    scale_x_discrete(breaks = day_counts$day_of_year[seq(1, nrow(day_counts), by = 19)]) +
  labs(y = NULL, x = NULL)

# Print the bar chart
suppressMessages(print(SR_day_of_the_year))
chart_path <- file.path(chart_directory_path, paste0(file_name_prefix, "-trend-day-of-the-year.pdf"))
suppressMessages(ggsave(chart_path, plot = SR_day_of_the_year, width = 10, height = 8))

#########################################################################
# Overall day-of-the-week summary
mean_count <- round(mean(day_of_week_df$count), 0)
median_count <- round(median(day_of_week_df$count), 0)
standard_deviation <- round(sd(day_of_week_df$count), 0)

cat("\nAverage day-of-the-week:", format(mean_count, big.mark = ","), 
    "  Standard deviation:", format(standard_deviation, big.mark = ","))

cat("\nMedian:", format(median_count, big.mark = ","))

day_of_week_df$day_of_week <- as.character(day_of_week_df$day_of_week)

max_day_of_the_week <- day_of_week_df[which.max(day_of_week_df$count), ]
cat("\nDay-of-the-week maximum:", max_day_of_the_week$day_of_week, 
    "with", format(max_day_of_the_week$count, big.mark = ","), "SRs")

min_day_of_the_week <- day_of_week_df[which.min((day_of_week_df$count)),]
cat("\nDay-of-the-week minimum:", min_day_of_the_week$day_of_week, 
    "with", format(min_day_of_the_week$count, big.mark = ","), "SRs")

cat("\n\nDay-of-the-Week Summary:\n")
print(day_of_week_df, row.names = FALSE, right = FALSE)

# Chart SRs by day-of-the-week
max_count <- max(day_of_week_df$count)
earliest_day_of_week <- "1-Monday"
total_count <- sum(day_of_week_df$count)

result <- calculate_values(max_count)
starting_value <- result$starting_value
increment <- result$increment

# Create the bar chart with vertical X-axis labels
SR_day_of_the_week <- ggplot(day_of_week_df, aes(x = day_of_week, y = count)) +
  scale_x_discrete() +
  geom_bar(stat = "identity", fill = "#117733") +
  theme(
    axis.title.x = element_text(vjust = 0, size = 11),
    axis.title.y = element_text(vjust = 1, size = 11),
    plot.title = element_text(hjust = 0.5, size = 13),
    plot.subtitle = element_text(size = 9),
    panel.background = element_rect(fill = "gray95", color = "gray95"),
    axis.text.x = element_text(angle = 60, vjust = 0.5, hjust = 0.5, face = "bold"),
    axis.text.y = element_text(face = "bold")
  ) +
  ggtitle("SR count by Day-of-the-Week",
          subtitle = paste("(", earliest_title, "--", latest_title, ")", 
                           "total=", format(total_count, big.mark = ","))
  ) +
  geom_hline(
    yintercept = seq(starting_value, max_count, by = increment),
    linetype = "dotted", color = "gray40"
  ) +
  geom_hline(yintercept = mean_count, linetype = "twodash", color = "black", linewidth = 0.6) +
  annotate("text", x = earliest_day_of_week, y = mean_count, 
           label = paste0("Average: ", format(round(mean_count, 0), big.mark = ",")), 
           size = 4, hjust = -0.5, vjust = -0.75) +
  labs(x=NULL, y=NULL)

# Print the bar chart
suppressMessages(print(SR_day_of_the_week))
chart_path <- file.path(chart_directory_path, paste0(file_name_prefix, "-trend-day-of-the-week.pdf"))
suppressMessages(ggsave(chart_path, plot = SR_day_of_the_week, width = 10, height = 8))

#########################################################################
# Overall created time-of-day summary (0900, 1000, 1100, 1200, 1300, etc.)
mean_count <- round(mean(created_hour_of_day_df$count), 0)
median_count <- round(median(created_hour_of_day_df$count), 0)
standard_deviation <- round(sd(created_hour_of_day_df$count), 0)

cat("\nAverage hour-of-the-day 'created' count:", format(mean_count, big.mark = ","), 
    "  Standard deviation:", format(standard_deviation, big.mark = ","))
cat("\nMedian:", format(median_count, big.mark = ","))

max_hour_of_the_day <- created_hour_of_day_df[which.max(created_hour_of_day_df$count), ]
max_hour_of_the_day$created_hour <- as.character(max_hour_of_the_day$created_hour)
cat("\nHour of the day with maximum 'created' count is hour #:", max_hour_of_the_day$created_hour, 
    "with", format(max_hour_of_the_day$count, big.mark = ","), "SRs")

min_hour_of_the_day <- created_hour_of_day_df[which.min(created_hour_of_day_df$count), ]
min_hour_of_the_day$created_hour <- as.character(min_hour_of_the_day$created_hour)
cat("\nHour of the day with the minimum 'created' count is hour #:", min_hour_of_the_day$created_hour, 
    "with", format(min_hour_of_the_day$count, big.mark = ","), "SRs\n")

cat("\nHour-of-the-day 'created' Summary:\n")

# Print the created time-of-the-day Summary
print(created_hour_of_day_df, row.names = FALSE, right = FALSE)

# Chart by 'created' time-of-the-day
max_count <- max(created_hour_of_day_df$count)

result <- calculate_values(max_count)
starting_value <- result$starting_value
increment <- result$increment

SR_created_time_of_day <- create_bar_chart(
  created_hour_of_day_df,
  x_col = "created_hour",
  y_col = "count",
  chart_title = "SR 'created' count by hour-of-the-day",
  sub_title = chart_sub_title,
  x_axis_title = NULL,
  y_axis_title = "count",
  print_out_title = "Hour-of-Day",
  add_mean = TRUE,
  add_median = FALSE,
  add_sd = FALSE,
  add_trendline = FALSE,
  add_maximum = TRUE,
  add_minimum = FALSE,
  add_second_maximum = TRUE,
  extra_line = NULL,
  chart_file_name = paste0(file_name_prefix, "-trend-created-time-of-the-day.pdf")
)

#########################################################################
# Overall closed time-of-day summary (0900, 1000, 1100, 1200, 1300, etc.)
mean_count <- round(mean(closed_hour_of_day_df$count), 0)
median_count <- round(median(closed_hour_of_day_df$count), 0)
standard_deviation <- round(sd(closed_hour_of_day_df$count), 0)

cat("\nAverage hour-of-the-day 'closed' count:", format(mean_count, big.mark = ","), 
    "  Standard deviation:", format(standard_deviation, big.mark = ","))
cat("\nMedian:", format(median_count, big.mark = ","))

max_hour_of_the_day <- closed_hour_of_day_df[which.max(closed_hour_of_day_df$count), ]
max_hour_of_the_day$closed_hour <- as.character(max_hour_of_the_day$closed_hour)
cat("\nHour of the day with maximum 'closed' count is hour #:", max_hour_of_the_day$closed_hour, 
    "with", format(max_hour_of_the_day$count, big.mark = ","), "SRs")

min_hour_of_the_day <- closed_hour_of_day_df[which.min(closed_hour_of_day_df$count), ]
min_hour_of_the_day$closed_hour <- as.character(min_hour_of_the_day$closed_hour)
cat("\nHour of the day with the minimum 'closed' count is hour #:", min_hour_of_the_day$closed_hour, 
    "with", format(min_hour_of_the_day$count, big.mark = ","), "SRs\n")

cat("\nHour-of-the-day 'closed' Summary:\n")

# Print the created time-of-the-day Summary
print(closed_hour_of_day_df, row.names = FALSE, right = FALSE)

SR_closed_time_of_day <- create_bar_chart(
  closed_hour_of_day_df,
  x_col = "closed_hour",
  y_col = "count",
  chart_title = "SRs 'closed' count by time-of-the-day",
  sub_title = chart_sub_title,
  x_axis_title = NULL,
  y_axis_title = "count",
  print_out_title = "Hour-of-Day",
  add_mean = TRUE,
  add_median = FALSE,
  add_sd = FALSE,
  add_trendline = FALSE,
  add_maximum = TRUE,
  add_minimum = FALSE,
  add_second_maximum = TRUE,
  extra_line = NULL,
  chart_file_name = paste0(file_name_prefix, "-trend-SR_created_by_minute_of_busiest_day.pdf")
)

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

#########################################################################
cat("\n *****END OF PROGRAM*****")
#########################################################################
sink()