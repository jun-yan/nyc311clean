install.packages("ggplot2")
install.packages("scales")
install.packages("dplyr")
install.packages('zoo')
install.packages("ggpmisc")
install.packages("lubridate")

library(ggplot2)
library(scales)
library(dplyr)
library(zoo)
library(ggpmisc)
library(lubridate)

# Set the working directory to the "nyc311clean/code" directory to enable relative codes.
setwd("C:/Users/david/OneDrive/Documents/nyc311clean/code")
data1File <- file.path("..", "data", "JAN_APR_2024_AS_OF_2024_05_06.csv")

#Define the chart directory
chart_directory_path <- file.path("C:", "Users", "david", "OneDrive", 
                                  "Documents", "nyc311clean", "charts")

programStart <- as.POSIXct(Sys.time())
formattedStartTime <- format(programStart, "%Y-%m-%d %H:%M:%S")
cat("\nExecution begins at:", formattedStartTime)

#########################################################################
create_cumulative_percentage <- function(dataset) {
  
  dataset$percentage <- dataset$count/sum(dataset$count)
  
  dataset <- dataset[order(-dataset$percentage), ]
  
  dataset$cumulative_percentage <- round(cumsum(dataset$percentage),2)
  dataset$percentage <- round(dataset$percentage, 3)
  
  return(dataset)
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

numrows <- nrow(d311)

#########################################################################
cat("\n\n**********DATA SUMMARY**********\n")

# Extract the date part of the character string
d311$extracted_created_date <- substr(d311$created_date, 1,10)
d311$extracted_closed_date <- substr(d311$closed_date, 1,10)

# Convert character date-time strings to datetime objects
d311$created_date <- as.POSIXct(d311$created_date, format = "%m/%d/%Y %I:%M:%S %p")
d311$closed_date <- as.POSIXct(d311$closed_date, format = "%m/%d/%Y %I:%M:%S %p")

#########################################################################
earliest_date <- min(d311$created_date, na.rm = TRUE)
earliest_date_formatted <- format(earliest_date, format = "%Y-%m-%d %H:%M:%S")

latest_date <- max(d311$created_date, na.rm = TRUE)
latest_date_formatted <- format(latest_date, format = "%Y-%m-%d %H:%M:%S")

earliest_title <- format(as.Date(earliest_date_formatted), format = "%Y-%m-%d")
latest_title <- format(as.Date(latest_date_formatted), format = "%Y-%m-%d")

#########################################################################
# Create new columns for Year, Year-Month, and Quarter
d311 <- d311 %>%
  mutate(Year = format(created_date, "%Y"),
         YearMonth = format(created_date, "%Y-%m"),
         Quarter = zoo::as.yearqtr(created_date))

########################################################################                       
# Assuming 'extracted_date' is a character column
d311$extracted_date <- as.Date(d311$extracted_created_date, format = "%m/%d/%Y")

# Create a new dataframe 'daily_summary'
daily_summary <- d311 %>%
  group_by(extracted_date) %>%
  summarise(count = n())

colnames(daily_summary) <- c("created_date", "count")

max_rows_daily <- max(daily_summary$count)
#daily_summary <- na.omit(daily_summary)

#########################################################################
# Aggregate at the yearly level
yearly_summary <- d311 %>%
  group_by(Year) %>%
  summarise(count = n())
max_rows_yearly <- max(yearly_summary$count)
yearly_summary <- na.omit(yearly_summary)

#########################################################################
# Aggregate at the quarterly level
d311$Quarter <- cut(d311$created_date, breaks = "quarter", labels = FALSE)

# Calculate quarterly counts with corresponding quarter dates
quarterly_summary <- d311 %>%
  group_by(Quarter) %>%
  summarise(count = n(), Quarter = min(created_date))
max_rows_quarterly <- max(quarterly_summary$count)
quarterly_summary <- na.omit(quarterly_summary)

########################################################################                       
# Aggregate at the monthly level
monthly_summary <- d311 %>%
  group_by(YearMonth) %>%
  summarise(count = n())
max_rows_monthly <- max(monthly_summary$count)
monthly_summary <- na.omit(monthly_summary)

#########################################################################
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

#########################################################################
#Aggregate by day of the year, e.g. Jan 1st, July 4th, Dec 25th, etc.
# Remove rows with NA values in "created_date" column
d311 <- d311[!is.na(d311$created_date), ]

# Extract day of the year
d311$day_of_year <- format(d311$created_date, "%m/%d")

# Count occurrences of each day of the year
day_counts <- d311 %>%
  group_by(day_of_year) %>%
  summarise(count = n())

#########################################################################
# Aggregate by day of the week
day_of_week <- d311 %>%
  group_by(DayOfWeek = format(created_date, "%A")) %>%
  summarise(count = n())
max_rows_day_of_week <- max(day_of_week$count)
day_of_week <- na.omit(day_of_week)

# Convert DayOfWeek to factor with custom order
day_of_week$DayOfWeek <- factor(
  day_of_week$DayOfWeek,
  levels = c( "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
)

# Order the data by day of the week
day_of_week <- day_of_week[order(day_of_week$DayOfWeek), ]

#########################################################################
# Aggregate by hour of the day
# Extract hour part from created_date and closed_date
d311$created_hour <- hour(d311$created_date)
d311$closed_hour <- hour(d311$closed_date)

# Aggregate the rows by created_hour
created_counts <- d311 %>%
  group_by(created_hour) %>%
  summarise(created_count = n())

# Aggregate the rows by closed_hour, excluding NA values
closed_counts <- d311 %>%
  filter(!is.na(closed_hour)) %>%
  group_by(closed_hour) %>%
  summarise(closed_count = n())

#########################################################################
yearly_df <- as.data.frame(yearly_summary)
yearly_df <- create_cumulative_percentage(yearly_df)

quarterly_df <- select(quarterly_summary,Quarter,  count)
quarterly_df$Quarter <- format(quarterly_df$Quarter, "%Y-%m-%d")
quarterly_df <- as.data.frame(quarterly_df)
quarterly_df <- create_cumulative_percentage(quarterly_df)

monthly_df <- as.data.frame(monthly_summary)
monthly_df <- create_cumulative_percentage(monthly_df)

daily_df <- as.data.frame(daily_summary)
daily_df <- create_cumulative_percentage(daily_df)

calendar_month_df <- as.data.frame(calendar_month_summary)
calendar_month_df <- create_cumulative_percentage(calendar_month_df)

day_counts_df <- as.data.frame(day_counts)
day_counts_df <- create_cumulative_percentage(day_counts_df)

day_of_week_df <- as.data.frame(day_of_week)
day_of_week_df <- create_cumulative_percentage(day_of_week_df)

# Rename the column to 'count'
created_hour_of_day_df <- as.data.frame((created_counts))
names(created_hour_of_day_df)[2] <- "count"
created_hour_of_day_df <- create_cumulative_percentage(created_hour_of_day_df)
# Rename the column back to 'created_count'
names(created_hour_of_day_df)[2] <- "created_count"

# Rename the column to 'count'
closed_hour_of_day_df <- as.data.frame((closed_counts))
names(closed_hour_of_day_df)[2] <- "count"
closed_hour_of_day_df <- create_cumulative_percentage(closed_hour_of_day_df)
names(closed_hour_of_day_df)[2] <- "closed_count"

#########################################################################
# Overall yearly summary
average_count <- round(mean(yearly_df$count), 0)
median_count <- round(median(yearly_df$count), 0)
standard_deviation <- round(sd(yearly_df$count))

cat("\nAverage yearly count:", format(average_count, big.mark = ","))
cat("\nStandard deviation:", format(standard_deviation, big.mark = ","))
cat("\nMedian:", format(median_count, big.mark = ","))

max_year <- yearly_df[which.max(yearly_df$count), ]
cat("Year with maximum count:",max_year$Year, "with", format(max_year$count, big.mark = ","), "SRs\n")

min_year <- yearly_df[which.min((yearly_df$count)),]
cat("\nYear with minimum count:", min_year$Year, 
    "with", format(min_year$count, big.mark = ","), "SRs\n")

cat("\n\nYearly Summary:\n")
print(yearly_df, row.names = FALSE, right = FALSE)

#########################################################################
# Overall quarterly summary
average_count <- round(mean(quarterly_df$count), 0)
median_count <- round(median(quarterly_df$count), 0)
standard_deviation <- round(sd(quarterly_df$count))

cat("\nAverage quarterly count:", format(average_count, big.mark = ","), 
    "  Standard deviation:", format(standard_deviation, big.mark = ","))
cat("\nMedian:", format(median_count, big.mark = ","))

cat("\nMaximum is", format(max(quarterly_df$count), big.mark = ","),
    "and Minimum is", format(min(quarterly_df$count), big.mark = ","))

max_quarter <- quarterly_df[which.max(quarterly_df$count), ]
cat("\nQuarter with maximum count:", max_quarter$Quarter,  
    "with", format(max_quarter$count, big.mark = ","), "SRs\n")

min_quarter <- quarterly_df[which.min((quarterly_df$count)),]
cat("\nMonth with minimum count:", min_quarter$Quarter, 
    "with", format(min_quarter$count, big.mark = ","), "SRs\n")

cat("\n\nQuarterly Summary:\n")
print(quarterly_df, row.names = FALSE, right = FALSE)

#########################################################################
# Overally monthly summary
average_count <- round(mean(monthly_df$count), 0)
median_count <- round(median(monthly_df$count), 0)
standard_deviation <- round(sd(monthly_df$count))

cat("\nAverage monthly:", format(average_count, big.mark = ","), 
    "  Standard deviation of", format(standard_deviation, big.mark = ","))

cat("\nMedian:", format(median_count, big.mark = ","))

max_month <- monthly_df[which.max(monthly_df$count), ]
cat("\nMonth with maximum count:", max_month$YearMonth, 
    "with", format(max_month$count, big.mark = ","), "SRs\n")

min_month <- monthly_df[which.min((monthly_df$count)),]
cat("\nMonth with minimum count:", min_month$YearMonth, 
    "with", format(min_month$count, big.mark = ","), "SRs\n")

cat("\n\nMonthly Summary:\n")
print(monthly_df, row.names = FALSE, right = FALSE)

#########################################################################
# Overall daily summary
average_count <- round(mean(daily_df$count), 0)
median_count <- round(median(daily_df$count), 0)
standard_deviation <- round(sd(daily_df$count))

cat("\nAverage daily count:", format(average_count, big.mark = ","), 
    "  Standard deviation:", format(standard_deviation, big.mark = ",")) 

cat("\nMedian is", format(median_count, big.mark = ","))

maximum_day <- daily_df[which.max(daily_df$count), ]
cat("\nMaximum:", maximum_day$count,"on", format(maximum_day$created_date, "%Y-%m-%d"))

minimum_day <- daily_df[which.min(daily_df$count), ]
cat("\nMinimum", minimum_day$count, "on", format(minimum_day$created_date, "%Y-%m-%d"))

# # Find busiest and quietest days of the calendar year
# busiest_day <- day_counts[which.max(daily_df$count), ]
# quietest_day <- day_counts[which.min(daily_df$count), ]
# 
# Find the index of the minimum value
min_index <- which.min(daily_df$count)

# Exclude the minimum value from the data and sort the remaining counts
sorted_counts <- sort(daily_df$count[-min_index])

# Find the next lowest value
next_lowest_value <- sorted_counts[1]

# Retrieve the corresponding row from the dataframe
next_lowest_day <- daily_df[daily_df$count == next_lowest_value, ]

cat("Busiest day of the calendar year:", format(maximum_day$created_date, "%Y-%m-%d"), 
    "- Count:", format(maximum_day$count, big.mark = ","), "\n")
cat("Quietest day of the calendar year is:", format(minimum_day$created_date, "%Y-%m-%d"), 
    "- Count:", format(minimum_day$count, big.mark = ","), "\n")
cat("The next quietest day of the calendar year is:", format(next_lowest_day$created_date, "%Y-%m-%d"), 
    "- Count:", format(next_lowest_day$count, big.mark = ","), "\n")

#########################################################################
# Overall calendar month summary (Jan, Feb, Mar, etc.)
average_count <- round(mean(calendar_month_df$count), 0)
median_count <- round(median(calendar_month_df$count), 0)
standard_deviation <- round(sd(calendar_month_df$count))

cat("\nAverage calendar month count:", format(average_count, big.mark = ","), 
    "  Standard deviation:", format(standard_deviation, big.mark = ","))
cat("\nMedian:", format(median_count, big.mark = ","))

max_calendar_month <- calendar_month_df[which.max(calendar_month_df$count), ]
max_calendar_month$Month <- as.character(month.name[max_calendar_month$Month])
cat("\nCalendar month with maximum count:", max_calendar_month$Month, 
    "with", format(max_calendar_month$count, big.mark = ","), "SRs\n")

min_calendar_month <- calendar_month_df[which.min((calendar_month_df$count)),]
min_calendar_month$Month <- as.character(month.name[min_calendar_month$Month])
cat("\nMonth with minimum count:", min_calendar_month$Month, 
    "with", format(min_calendar_month$count, big.mark = ","), "SRs\n")

print(calendar_month_df, row.names = FALSE, right = FALSE)

#########################################################################
# Overall day-of-the-year summary (Jan 1st, Jan 2nd, Jan 3rd, etc.)
average_count <- round(mean(day_counts_df$count), 0)
median_count <- round(median(day_counts_df$count), 0)
standard_deviation <- round(sd(day_counts_df$count))

cat("\nAverage day-of-the-year count:", format(average_count, big.mark = ","), 
    "  Standard deviation:", format(standard_deviation, big.mark = ","))
cat("\nMedian:", format(median_count, big.mark = ","))
cat("\nMaximum is", format(max(day_counts_df$count), big.mark = ","),
    "and Minimum is", format(min(day_counts_df$count), big.mark = ","))

max_day_of_the_year <- day_counts_df[which.max(day_counts_df$count), ]
max_day_of_the_year$day_of_year <- as.character(max_day_of_the_year$day_of_year)
cat("\nDay of week with maximum count:", max_day_of_the_year$day_of_year, 
    "with", format(max_day_of_the_year$count, big.mark = ","), "SRs\n")

min_day_of_the_year <- day_counts_df[which.min(day_counts_df$count), ]
min_day_of_the_year$DayOfWeek <- as.character(min_day_of_the_year$day_of_year)
cat("\nDay of the year with minimum count:", min_day_of_the_year$day_of_year, 
    "with", format(min_day_of_the_year$count, big.mark = ","), "SRs\n")

cat("\n\nDay-of-the-Year Summary:\n")

# Print the Day-of-the-Week Summary with custom order
print(day_counts_df, row.names = FALSE, right = FALSE)

#########################################################################
# Overall created time-of-day summary (0900, 1000, 1100, 1200, 1300, etc.)
average_count <- round(mean(created_hour_of_day_df$created_count), 0)
median_count <- round(median(created_hour_of_day_df$created_count), 0)
standard_deviation <- round(sd(created_hour_of_day_df$created_count))

cat("\nAverage hour-of-the-day 'created' count:", format(average_count, big.mark = ","), 
    "  Standard deviation:", format(standard_deviation, big.mark = ","))
cat("\nMedian:", format(median_count, big.mark = ","))
cat("\nMaximum is", format(max(created_hour_of_day_df$created_count), big.mark = ","),
    "and Minimum is", format(min(created_hour_of_day_df$created_count), big.mark = ","))

max_hour_of_the_day <- created_hour_of_day_df[which.max(created_hour_of_day_df$created_count), ]
max_hour_of_the_day$created_hour <- as.character(max_hour_of_the_day$created_hour)
cat("\nHour of the day with maximum 'created' count:", max_hour_of_the_day$created_hour, 
    "with", format(max_hour_of_the_day$created_count, big.mark = ","), "SRs\n")

min_hour_of_the_day <- created_hour_of_day_df[which.min(created_hour_of_day_df$created_count), ]
min_hour_of_the_day$created_hour <- as.character(min_hour_of_the_day$created_hour)
cat("\nHour of the day with the minimum 'created' count:", min_hour_of_the_day$created_hour, 
    "with", format(min_hour_of_the_day$created_count, big.mark = ","), "SRs\n")

cat("\n\nHour-of-the-day 'created' Summary:\n")

# Print the created time-of-the-day Summary
print(created_hour_of_day_df, row.names = FALSE, right = FALSE)

#########################################################################
# Overall closed time-of-day summary (0900, 1000, 1100, 1200, 1300, etc.)
average_count <- round(mean(closed_hour_of_day_df$closed_count), 0)
median_count <- round(median(closed_hour_of_day_df$closed_count), 0)
standard_deviation <- round(sd(closed_hour_of_day_df$closed_count))

cat("\nAverage hour-of-the-day 'closed' count:", format(average_count, big.mark = ","), 
    "  Standard deviation:", format(standard_deviation, big.mark = ","))
cat("\nMedian:", format(median_count, big.mark = ","))
cat("\nMaximum is", format(max(closed_hour_of_day_df$closed_count), big.mark = ","),
    "and Minimum is", format(min(closed_hour_of_day_df$closed_count), big.mark = ","))

max_hour_of_the_day <- closed_hour_of_day_df[which.max(closed_hour_of_day_df$closed_count), ]
max_hour_of_the_day$closed_hour <- as.character(max_hour_of_the_day$closed_hour)
cat("\nHour of the day with maximum 'closed' count:", max_hour_of_the_day$closed_hour, 
    "with", format(max_hour_of_the_day$closed_count, big.mark = ","), "SRs\n")

min_hour_of_the_day <- closed_hour_of_day_df[which.min(closed_hour_of_day_df$closed_count), ]
min_hour_of_the_day$closed_hour <- as.character(min_hour_of_the_day$closed_hour)
cat("\nHour of the day with the minimum 'closed' count:", min_hour_of_the_day$closed_hour, 
    "with", format(min_hour_of_the_day$closed_count, big.mark = ","), "SRs\n")

cat("\n\nHour-of-the-day 'closed' Summary:\n")

# Print the created time-of-the-day Summary
print(closed_hour_of_day_df, row.names = FALSE, right = FALSE)

#########################################################################
# Chart SRs by year
max_count <- max(yearly_df$count)
total_count <- sum(yearly_df$count)
total_count <- comma(total_count)

result <- calculate_values(max_count)
starting_value <- result$starting_value
increment <- result$increment
scaling_factor <- result$scaling_factor
scaling_factor_str <- format(scaling_factor, scientific = FALSE, big.mark = ",")

yearly_df$Year <- as.numeric(yearly_df$Year)

# Create the bar chart with vertical X-axis labels
SR_yearly <- ggplot(yearly_df, aes(x = Year, y = count)) +
  geom_bar(stat = "identity", fill = "cadetblue") +
  labs(x = "Year" ) +
  scale_y_continuous(labels = scales::comma) +
  theme(
    axis.title.x = element_text(vjust = 0, size = 11),
    axis.title.y = element_text(vjust = 1, size = 11),
    plot.title = element_text(hjust = 0.5, size = 13),
    plot.subtitle = element_text(size = 9),
    panel.background = element_rect(fill = "gray91", color = "gray91"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5, face = "bold"),
    axis.text.y = element_text(face = "bold")
  ) +
  geom_hline(
    yintercept = seq(starting_value, max_count, by = increment),
    linetype = "dotted", color = "gray21"
  ) +
  ggtitle("Yearly SR count (w/trendline)",
          subtitle = paste("(", earliest_title, "--", latest_title, ")", "total=", total_count)
  ) +
  geom_point(color = "transparent") +
  scale_x_continuous(breaks = unique(yearly_df$Year)) +
  stat_poly_eq(use_label(c("R2"))) +
  geom_point(color = "transparent") +
  geom_smooth(method = "lm", span = 1, se = FALSE, color = "firebrick3", linetype = "dotted", linewidth = 1) +
  labs(x=NULL, y=NULL)

# Print the bar chart
suppressMessages(print(SR_yearly))
chart_path <- file.path(chart_directory_path, "Yearly.png")
suppressMessages(ggsave(chart_path, plot = SR_yearly, width = 10, height = 8))

#########################################################################
# Chart SRs by quarter
max_count <- max(quarterly_df$count)
total_count <- sum(quarterly_df$count)
total_count <- comma(total_count)

result <- calculate_values(max_count)
starting_value <- result$starting_value
increment <- result$increment
scaling_factor <- result$scaling_factor
scaling_factor_str <- format(scaling_factor, scientific = FALSE, big.mark = ",")

quarterly_df$Quarter <- as.Date(quarterly_df$Quarter)

# Create the bar chart with vertical X-axis labels
SR_quarterly <- ggplot(quarterly_df, aes(x = Quarter, y = count)) +
  geom_bar(stat = "identity", fill = "cadetblue") +
  labs(x = "Year-Quarter" ) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(labels = scales::date_format("%Y-%m"), breaks = scales::breaks_width("1 year")) +
  theme(
    axis.title.x = element_text(vjust = 0, size = 11),
    axis.title.y = element_text(vjust = 1, size = 11),
    plot.title = element_text(hjust = 0.5, size = 13),
    plot.subtitle = element_text(size = 9),
    panel.background = element_rect(fill = "gray91", color = "gray91"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5, face = "bold"),
    axis.text.y = element_text(face = "bold"),
  ) +
  geom_hline(
    yintercept = seq(starting_value, max_count, by = increment),
    linetype = "dotted", color = "gray21"
  ) +
  ggtitle("Quarterly SR count (w/trendline)",
          subtitle = paste("(", earliest_title, "--", latest_title, ")", "total=", total_count)
  ) +
  geom_point(color = "transparent") +
  #  scale_x_date(labels = scales::date_format("%Y-%m"), breaks = scales::date_breaks("1 year")) +
  stat_poly_eq(use_label(c("R2"))) +
  geom_point(color = "transparent") +
  geom_smooth(method = "lm", span = 1, se = FALSE, color = "firebrick3", linetype = "dotted", linewidth = 1) +
  labs(x=NULL, y=NULL)

# Print the bar chart
suppressMessages(print(SR_quarterly))
chart_path <- file.path(chart_directory_path, "Quarterly.png")
suppressMessages(ggsave(chart_path, plot = SR_quarterly, width = 10, height = 8))

#########################################################################
# Chart SRs by month
max_count <- max(monthly_df$count)
total_count <- sum(monthly_df$count)
total_count <- comma(total_count)

result <- calculate_values(max_count)
starting_value <- result$starting_value
increment <- result$increment
scaling_factor <- result$scaling_factor
scaling_factor_str <- format(scaling_factor, scientific = FALSE, big.mark = ",")

monthly_df$YearMonth <- as.Date(paste0(monthly_df$YearMonth, "-01"))

# Create the bar chart with vertical X-axis labels
SR_monthly <- ggplot(monthly_df, aes(x = YearMonth, y = count)) +
  geom_bar(stat = "identity", fill = "cadetblue") +
  labs(x = "Year-Month" ) +
  scale_y_continuous(labels = scales::comma) +
  theme(
    axis.title.x = element_text(vjust = 0, size = 11),
    axis.title.y = element_text(vjust = 1, size = 11),
    plot.title = element_text(hjust = 0.5, size = 13),
    plot.subtitle = element_text(size = 9),
    panel.background = element_rect(fill = "gray91", color = "gray91"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5, face = "bold"),
    axis.text.y = element_text(face = "bold"),
#    scale_x_date(labels = date_format("%Y-%m"), breaks = date_breaks("1 year")) # Display every year on X-axis
  ) +
  geom_hline(
    yintercept = seq(starting_value, max_count, by = increment),
    linetype = "dotted", color = "gray21"
  ) +
   ggtitle(" Monthly SR count (w/trendline)",
          subtitle = paste("(", earliest_title, "--", latest_title, ")", "total=", total_count)
  ) +
  geom_point(color = "transparent") +
  stat_poly_eq(use_label(c("R2"))) +
  scale_x_date(labels = scales::date_format("%Y-%m"), breaks = scales::date_breaks("1 year")) +
  geom_smooth(method = "lm", span = 1, se = FALSE, 
      color = "firebrick3", linetype = "dotted", linewidth = 1) +
  labs(x=NULL, y=NULL)

# Print the bar chart
suppressMessages(print(SR_monthly))
chart_path <- file.path(chart_directory_path, "Monthly.png")
suppressMessages(ggsave(chart_path, plot = SR_monthly, width = 10, height = 8))

#########################################################################
# Chart SRs by day
max_count <- max(daily_df$count)
median_count <- median(daily_df$count)
median_date <- total_count <- sum(daily_df$count)
total_count <- comma(total_count)

result <- calculate_values(max_count)
starting_value <- result$starting_value
increment <- result$increment
scaling_factor <- result$scaling_factor
scaling_factor_str <- format(scaling_factor, scientific = FALSE, big.mark = ",")

# Create the bar chart with vertical X-axis labels
SR_daily <- ggplot(daily_df, aes(x = created_date, y = count)) +
  geom_bar(stat = "identity", fill = "cadetblue") +
  #  labs(x = "Year-Month" ) +
  scale_y_continuous(labels = scales::comma) +
  theme(
    axis.title.x = element_text(vjust = 0, size = 11),
    axis.title.y = element_text(vjust = 1, size = 11),
    plot.title = element_text(hjust = 0.5, size = 13),
    plot.subtitle = element_text(size = 9),
    panel.background = element_rect(fill = "gray91", color = "gray91"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5, face = "bold"),
    axis.text.y = element_text(face = "bold")) +
    geom_hline(
    yintercept = median_count,  # Add average line
    linetype = "dashed", color = "gray30", linewidth = 0.7) +
  geom_hline(
    yintercept = seq(starting_value, max_count, by = increment),
    linetype = "dotted", color = "gray21"
  ) +
  ggtitle("Daily SR count (w/trendline)",
          subtitle = paste("(", earliest_title, "--", latest_title, ")", "total=", total_count)
  ) +
  geom_point(color = "transparent") +
  #  stat_poly_eq(use_label(c("R2"))) +
  scale_x_date(labels = scales::date_format("%Y-%m"), breaks = scales::date_breaks("1 year")) +
  #  geom_smooth(method = "lm", span = 1, se = FALSE, 
  #    color = "firebrick3", linetype = "dotted", linewidth = 1) +
  labs(x=NULL, y=NULL) +
  geom_text(aes(x = as.Date(earliest_date), 
                y = median_count, 
                label = "Median"), 
            color = "gray21", 
            hjust = -0.6, 
            vjust = -0.75)

# Print the bar chart
suppressMessages(print(SR_daily))
chart_path <- file.path(chart_directory_path, "Daily.png")
suppressMessages(ggsave(chart_path, plot = SR_daily, width = 20, height = 10))

#########################################################################
# Chart SRs by calendar month
max_count <- max(calendar_month_df$count)
result <- calculate_values(max_count)
starting_value <- result$starting_value
increment <- result$increment
scaling_factor <- result$scaling_factor
scaling_factor_str <- format(scaling_factor, scientific = FALSE, big.mark = ",")

# Convert Month to factor with custom order
calendar_month_df$Month <- factor(
  calendar_month_df$Month,
  levels = month.name
)

# Create the bar chart with vertical X-axis labels
SR_calendar_month <- ggplot(calendar_month_df, aes(x = Month, y = count)) +
  geom_bar(stat = "identity", fill = "cadetblue") +
  labs(x = "Calendar Month") +
  scale_y_continuous(labels = scales::comma) +
  theme(
    axis.title.x = element_text(vjust = 0, size = 11),
    axis.title.y = element_text(vjust = 1, size = 11),
    plot.title = element_text(hjust = 0.5, size = 13),
    plot.subtitle = element_text(size = 9),
    panel.background = element_rect(fill = "gray91", color = "gray91"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5, face = "bold"),
    axis.text.y = element_text(face = "bold")
  ) +
  ggtitle("SR count by Calendar Month",
          subtitle = paste("(", earliest_title, "--", latest_title, ")", "total=", total_count)
  ) +
  geom_hline(
    yintercept = seq(starting_value, max_count, by = increment),
    linetype = "dotted", color = "gray21"
  ) +
  geom_point(color = "transparent") +
  # geom_smooth(method = "lm", span = 1, se = FALSE, color = "firebrick3", linetype = "dotted", linewidth = 1.25) +
  scale_x_discrete() +
  labs(x=NULL, y=NULL)

# Print the bar chart
suppressMessages(print(SR_calendar_month))
chart_path <- file.path(chart_directory_path, "Calendar-Month.png")
suppressMessages(ggsave(chart_path, plot = SR_calendar_month, width = 10, height = 8))

#########################################################################
# Chart SRs by day of the calendar year
max_count <- max(day_counts_df$count)

result <- calculate_values(max_count)
starting_value <- result$starting_value
increment <- result$increment
scaling_factor <- result$scaling_factor
scaling_factor_str <- format(scaling_factor, scientific = FALSE, big.mark = ",")

# Create the bar chart with vertical X-axis labels
SR_day_of_the_year <- ggplot(day_counts_df, aes(x = day_of_year, y = count)) +
  geom_bar(stat = "identity", fill = "cadetblue") +
  labs(x = "Day of the Year") +
  scale_y_continuous(labels = scales::comma) +
  theme(
    axis.title.x = element_text(vjust = 0, size = 11),
    axis.title.y = element_text(vjust = 1, size = 11),
    plot.title = element_text(hjust = 0.5, size = 13),
    plot.subtitle = element_text(size = 9),
    panel.background = element_rect(fill = "gray91", color = "gray91"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5, face = "bold"),
    axis.text.y = element_text(face = "bold")
  ) +
  ggtitle("SR count by Day-of-the-Year",
          subtitle = paste("(", earliest_title, "--", latest_title, ")", "total=", total_count)
  ) +
  geom_hline(
    yintercept = seq(starting_value, max_count, by = increment),
    linetype = "dotted", color = "gray21"
  ) +
  geom_point(color = "transparent") +
  # geom_smooth(method = "lm", span = 1, se = FALSE, color = "firebrick3", linetype = "dotted", linewidth = 1.25) +
  #  scale_x_discrete() +
  scale_x_discrete(breaks = day_counts$day_of_year[seq(1, nrow(day_counts), by = 19)]) +
  labs(y = NULL, x = NULL)

# Print the bar chart
suppressMessages(print(SR_day_of_the_year))
chart_path <- file.path(chart_directory_path, "day-of-the-year.png")
suppressMessages(ggsave(chart_path, plot = SR_day_of_the_year, width = 10, height = 8))

#########################################################################
# Chart SRs by day-of-the-week
max_count <- max(day_of_week_df$count)

result <- calculate_values(max_count)
starting_value <- result$starting_value
increment <- result$increment
scaling_factor <- result$scaling_factor
scaling_factor_str <- format(scaling_factor, scientific = FALSE, big.mark = ",")

# Create the bar chart with vertical X-axis labels
SR_day_of_the_week <- ggplot(day_of_week_df, aes(x = DayOfWeek, y = count)) +
  geom_bar(stat = "identity", fill = "cadetblue") +
  labs(x = "Calendar Month") +
  scale_y_continuous(labels = scales::comma) +
  theme(
    axis.title.x = element_text(vjust = 0, size = 11),
    axis.title.y = element_text(vjust = 1, size = 11),
    plot.title = element_text(hjust = 0.5, size = 13),
    plot.subtitle = element_text(size = 9),
    panel.background = element_rect(fill = "gray91", color = "gray91"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5, face = "bold"),
    axis.text.y = element_text(face = "bold")
  ) +
  ggtitle("SR count by Day-of-the-Week",
          subtitle = paste("(", earliest_title, "--", latest_title, ")", "total=", total_count)
  ) +
  geom_hline(
    yintercept = seq(starting_value, max_count, by = increment),
    linetype = "dotted", color = "gray21"
  ) +
  geom_point(color = "transparent") +
  # geom_smooth(method = "lm", span = 1, se = FALSE, color = "firebrick3", linetype = "dotted", linewidth = 1.25) +
  scale_x_discrete() +
  labs(x=NULL, y=NULL)

# Print the bar chart
suppressMessages(print(SR_day_of_the_week))
chart_path <- file.path(chart_directory_path, "day-of-the-week.png")
suppressMessages(ggsave(chart_path, plot = SR_day_of_the_week, width = 10, height = 8))

#########################################################################
# Chart by 'created' time-of-the-day
max_count <- max(created_hour_of_day_df$created_count)

result <- calculate_values(max_count)
starting_value <- result$starting_value
increment <- result$increment
scaling_factor <- result$scaling_factor
scaling_factor_str <- format(scaling_factor, scientific = FALSE, big.mark = ",")

# Create the bar chart with vertical X-axis labels
SR_created_time_of_day <- ggplot(created_hour_of_day_df, aes(x = created_hour, y = created_count)) +
  geom_bar(stat = "identity", fill = "cadetblue") +
  scale_x_continuous(breaks = seq(0, 23, by = 1), labels = seq(0, 23, by = 1)) +  
  scale_y_continuous(labels = scales::comma) +
  theme(
    axis.title.x = element_text(vjust = 0, size = 11),
    axis.title.y = element_text(vjust = 1, size = 11),
    plot.title = element_text(hjust = 0.5, size = 13),
    plot.subtitle = element_text(size = 9),
    panel.background = element_rect(fill = "gray91", color = "gray91"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5, face = "bold"),
    axis.text.y = element_text(face = "bold")
  ) +
  ggtitle("SR 'created' count by time-of-the-day",
          subtitle = paste("(", earliest_title, "--", latest_title, ")", "total=", total_count)
  ) +
  geom_hline(
    yintercept = seq(starting_value, max_count, by = increment),
    linetype = "dotted", color = "gray21"
  ) +
  geom_point(color = "transparent") +
  labs(x=NULL, y= "hour-of-day")

# Print the bar chart
suppressMessages(print(SR_created_time_of_day))
chart_path <- file.path(chart_directory_path, "created-time-of-the-day.png")
suppressMessages(ggsave(chart_path, plot = SR_created_time_of_day, width = 10, height = 8))

#########################################################################
# Chart by 'closed' time-of-the-day
max_count <- max(closed_hour_of_day_df$closed_count)

result <- calculate_values(max_count)
starting_value <- result$starting_value
increment <- result$increment
scaling_factor <- result$scaling_factor
scaling_factor_str <- format(scaling_factor, scientific = FALSE, big.mark = ",")

# Create the bar chart with vertical X-axis labels
SR_closed_time_of_day <- ggplot(closed_hour_of_day_df, aes(x = closed_hour, y = closed_count)) +
  geom_bar(stat = "identity", fill = "cadetblue") +
  scale_x_continuous(breaks = seq(0, 23, by = 1), labels = seq(0, 23, by = 1)) +  
  scale_y_continuous(labels = scales::comma) +
  theme(
    axis.title.x = element_text(vjust = 0, size = 11),
    axis.title.y = element_text(vjust = 1, size = 11),
    plot.title = element_text(hjust = 0.5, size = 13),
    plot.subtitle = element_text(size = 9),
    panel.background = element_rect(fill = "gray91", color = "gray91"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5, face = "bold"),
    axis.text.y = element_text(face = "bold")
  ) +
  ggtitle("SR 'closed' count by time-of-the-day",
          subtitle = paste("(", earliest_title, "--", latest_title, ")", "total=", total_count)
  ) +
  geom_hline(
    yintercept = seq(starting_value, max_count, by = increment),
    linetype = "dotted", color = "gray21"
  ) +
  geom_point(color = "transparent") +
  labs(x=NULL, y= "hour-of-day")

# Print the bar chart
suppressMessages(print(SR_closed_time_of_day))
chart_path <- file.path(chart_directory_path, "closed-time-of-the-day.png")
suppressMessages(ggsave(chart_path, plot = SR_closed_time_of_day, width = 10, height = 8))

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
