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

# Set the working directory to the "nyc311clean/code" directory to enable relative codes.
setwd("C:/Users/david/OneDrive/Documents/nyc311clean/code")
data1File <- file.path("..", "data", "2022-2023 created and closed only.csv")

#Define the chart directory
chart_directory_path <- 
  file.path("..", "charts", "2022-2023 study", "Time lapse charts")

programStart <- as.POSIXct(Sys.time())
formattedStartTime <- format(programStart, "%Y-%m-%d %H:%M:%S")
cat("\nExecution begins at:", formattedStartTime)

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

numrows <- nrow(d311)

#########################################################################
cat("\n\n**********DATA SUMMARY**********\n")

# # Extract the date part of the character string
# d311$extracted_created_date <- substr(d311$created_date, 1,10)
# d311$extracted_closed_date <- substr(d311$closed_date, 1,10)

# Convert character date-time strings to datetime objects
# Convert character date-time strings to datetime objects with America/New_York timezone
d311$created_date <- as.POSIXct(d311$created_date, format = "%m/%d/%Y %I:%M:%S %p", tz = "America/New_York")
d311$closed_date <- as.POSIXct(d311$closed_date, format = "%m/%d/%Y %I:%M:%S %p", tz = "America/New_York")

d311 <- d311[!is.na(d311$created_date),]

num_years <- d311 %>%
  mutate(year = year(created_date)) %>%
  summarise(distinct_years = n_distinct(year)) %>%
  pull(distinct_years)

cat("\nTotal rows:", format(numrows, big.mark = ","), "covering", num_years,"years")

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

#########################################################################
# Aggregate at the yearly level
yearly_summary <- d311 %>%
  group_by(Year) %>%
  summarise(count = n())
max_rows_yearly <- max(yearly_summary$count)
yearly_summary <- na.omit(yearly_summary)

#########################################################################
# Aggregate at the monthly level
monthly_summary <- d311 %>%
  group_by(YearMonth) %>%
  summarise(count = n())
max_rows_monthly <- max(monthly_summary$count)
monthly_summary <- na.omit(monthly_summary)

########################################################################                       
# Aggregate at the daily level
daily_summary <- d311 %>%
  mutate(date = date(created_date)) %>%  # Extract date part from datetime_col
  group_by(date) %>%                     # Group by the extracted date
  summarise(count = n())                 # Summarize by counting the number of entries per day

colnames(daily_summary) <- c("created_date", "count")

max_rows_daily <- max(daily_summary$count)
daily_summary <- na.omit(daily_summary)

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

#########################################################################
yearly_df <- as.data.frame(yearly_summary)

monthly_df <- as.data.frame(monthly_summary)

daily_df <- as.data.frame(daily_summary)

calendar_month_df <- as.data.frame(calendar_month_summary)

day_counts_df <- as.data.frame(day_counts)

day_of_week_df <- as.data.frame(day_of_week)

# Rename the column to 'count'
created_hour_of_day_df <- as.data.frame((created_counts))

# Rename the column to 'count'
closed_hour_of_day_df <- as.data.frame((closed_counts))

#########################################################################
# Overall yearly summary
mean_count <- round(mean(yearly_df$count), 0)
median_count <- round(median(yearly_df$count), 0)
standard_deviation <- round(sd(yearly_df$count))

cat("\n\nAverage yearly count:", format(mean_count, big.mark = ","))
cat("\nStandard deviation:", format(standard_deviation, big.mark = ","))
cat("\nMedian:", format(median_count, big.mark = ","))

max_year <- yearly_df[which.max(yearly_df$count), ]
cat("\nYear with maximum count:",max_year$Year, "with", format(max_year$count, big.mark = ","), "SRs")

min_year <- yearly_df[which.min((yearly_df$count)),]
cat("\nYear with minimum count:", min_year$Year, 
    "with", format(min_year$count, big.mark = ","), "SRs\n")

# Extract the earliest and latest year counts
earliest_year_count <- yearly_df %>% filter(Year == min(Year)) %>% select(count) %>% pull()
latest_year_count <- yearly_df %>% filter(Year == max(Year)) %>% select(count) %>% pull()

# Compute the percentage growth
percentage_growth <- round(((latest_year_count - earliest_year_count) / earliest_year_count) * 100,2)

cat("\nGrowth over", nrow(yearly_df), "years is", percentage_growth, "%" )

cat("\nYearly Summary:\n")
print(yearly_df, row.names = FALSE, right = FALSE)

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
earliest_year <- min(yearly_df$Year)

# Create the bar chart with vertical X-axis labels
SR_yearly <- ggplot(yearly_df, aes(x = Year, y = count)) +
  geom_bar(stat = "identity", fill = "cadetblue") +
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
  geom_hline(yintercept = mean_count, linetype = "solid", color = "gray21", linewidth = 0.4) +
  annotate("text", x = earliest_year, y = mean_count, label = "Average", size = 3, hjust = -0.5, vjust = -0.75) +
  ggtitle("Yearly SR count (w/trendline)",
          subtitle = paste("(", earliest_title, "--", latest_title, ")", "total=", total_count)
  ) +
  geom_point(color = "transparent") +
  scale_x_continuous(breaks = unique(yearly_df$Year)) +
  stat_poly_eq(use_label(c("R2"))) +
  geom_smooth(method = "lm", span = 1, se = FALSE, color = "firebrick3", linetype = "dotted", linewidth = 1) +
  labs(x=NULL, y=NULL)

# Print the bar chart
suppressMessages(print(SR_yearly))
chart_path <- file.path(chart_directory_path, "Yearly.png")
suppressMessages(ggsave(chart_path, plot = SR_yearly, width = 10, height = 8))

#########################################################################
# Overall monthly summary
mean_count <- round(mean(monthly_df$count), 0)
median_count <- round(median(monthly_df$count), 0)
standard_deviation <- round(sd(monthly_df$count))

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

result <- calculate_values(max_count)
starting_value <- result$starting_value
increment <- result$increment
scaling_factor <- result$scaling_factor
scaling_factor_str <- format(scaling_factor, scientific = FALSE, big.mark = ",")

start_date <- min(monthly_df$YearMonth)
end_date <- max(monthly_df$YearMonth)

# Create a sequence of every other month
breaks_seq <- seq(from = start_date, to = end_date, by = "2 months")

# Create the bar chart with vertical X-axis labels
SR_monthly <- ggplot(monthly_df, aes(x = YearMonth, y = count)) +
  geom_bar(stat = "identity", fill = "cadetblue") +
  scale_y_continuous(labels = scales::comma) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 13),
    plot.subtitle = element_text(size = 9),
    panel.background = element_rect(fill = "gray91", color = "gray91"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5, face = "bold"),
    axis.text.y = element_text(face = "bold"),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
    ) + # Adjust plot margins
  geom_hline(
    yintercept = seq(starting_value, max_count, by = increment),
    linetype = "dotted", color = "gray21"
  ) +
  geom_hline(yintercept = mean_count, linetype = "solid", color = "gray21", linewidth = 0.4) +
  annotate("text", x = as.Date(earliest_YearMonth), y = mean_count, label = "Average", size = 3, hjust = -0.5, vjust = -0.75) +
  ggtitle(" Monthly SR count (w/trendline)",
          subtitle = paste("(", earliest_title, "--", latest_title, ")", "total=", total_count)
  ) +
  geom_point(color = "transparent") +
  stat_poly_eq(use_label(c("R2"))) +
  scale_x_date(labels = scales::date_format("%Y-%m"), breaks = breaks_seq,
               expand = c(0,0)) + 
  geom_smooth(method = "lm", span = 1, se = FALSE, 
              color = "firebrick3", linetype = "dotted", linewidth = 1) +
  annotate("text", x = mxmonth, y = max_month$count, label = "Max", size = 4, color = "firebrick3", hjust = -0.2, vjust = -0.5) +
  annotate("text", x = mimonth, y = min_month$count, label = "Min", size = 4, color = "dodgerblue4", hjust = -0.2, vjust = -0.5)
  labs(x=NULL, y=NULL)

# Print the bar chart
suppressMessages(print(SR_monthly))
chart_path <- file.path(chart_directory_path, "Monthly.png")
suppressMessages(ggsave(chart_path, plot = SR_monthly, width = 10, height = 8))

#########################################################################
# Overall daily summary
mean_count <- round(mean(daily_df$count), 0)
median_count <- round(median(daily_df$count), 0)
standard_deviation <- round(sd(daily_df$count))


cat("\nAverage daily count:", format(mean_count, big.mark = ","), 
    "  Standard deviation:", format(standard_deviation, big.mark = ",")) 

cat("\nMedian:", format(median_count, big.mark = ","))

daily_df$created_date <- as.character(daily_df$created_date)

#cat("\nMaximum:", maximum_day$count,"on", maximum_day$created_date)

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

# Chart SRs by day
max_count <- max(daily_df$count)
median_count <- median(daily_df$count)
mean_count <- mean(daily_df$count)
earliest_date <- min(daily_df$created_date)

# Calculate max and min values
max_value <- max(daily_df$count)
min_value <- min(daily_df$count)

# Find corresponding dates
max_date <- daily_df$created_date[which.max(daily_df$count)]
min_date <- daily_df$created_date[which.min(daily_df$count)]


total_count <- sum(daily_df$count)
total_count <- comma(total_count)

result <- calculate_values(max_count)
starting_value <- result$starting_value
increment <- result$increment
scaling_factor <- result$scaling_factor
scaling_factor_str <- format(scaling_factor, scientific = FALSE, big.mark = ",")

daily_df <- daily_df[order(daily_df$created_date), ]

daily_df$created_date <- as.Date(daily_df$created_date)

# Create the bar chart with vertical X-axis labels
SR_daily <- ggplot(daily_df, aes(x = created_date, y = count)) +
  geom_bar(stat = "identity", fill = "cadetblue") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(expand = c(0, 0), labels = scales::date_format("%Y-%m"), breaks = scales::date_breaks("6 months")) + 
  theme(
    axis.title.x = element_text(vjust = 0, size = 11),
    axis.title.y = element_text(vjust = 1, size = 11),
    plot.title = element_text(hjust = 0.5, size = 13),
    plot.subtitle = element_text(size = 9),
    panel.background = element_rect(fill = "gray91", color = "gray91"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5, face = "bold"),
    axis.text.y = element_text(face = "bold")) +
  # geom_hline(
  #   yintercept = median_count,  # Add average line
  #   linetype = "dashed", color = "gray30", linewidth = 0.7) +
  geom_hline(
    yintercept = seq(starting_value, max_count, by = increment),
    linetype = "dotted", color = "gray21"
  ) +
  ggtitle("Daily SR count (w/trendline)",
          subtitle = paste("(", earliest_title, "--", latest_title, ") ", "total=", total_count, sep="")
  ) +
  geom_point(color = "transparent") +
#  scale_x_date(labels = scales::date_format("%Y-%m"), breaks = scales::date_breaks("1 year")) +
  labs(x=NULL, y=NULL) +
  #geom_hline(yintercept = mean_count + 1*standard_deviation, linetype = "longdash", color = "goldenrod4", linewidth = 0.4) +
  #geom_hline(yintercept = mean_count + 2*standard_deviation, linetype = "longdash", color = "goldenrod4", linewidth = 0.4) +
  geom_hline(yintercept = mean_count + 3*standard_deviation, linetype = "longdash", color = "goldenrod4", linewidth = 0.4) +
  geom_hline(yintercept = mean_count, linetype = "solid", color = "gray21", linewidth = 0.4) +
  #annotate("text", x = as.Date(earliest_date), y = mean_count + 1*standard_deviation, label = "+1 SD", size = 3, color = "goldenrod4", hjust = -0.5, vjust = -0.75) +
  #annotate("text", x = as.Date(earliest_date), y = mean_count + 2*standard_deviation, label = "+2 SD", size = 3, color = "goldenrod4", hjust = -0.5, vjust = -0.75) +
  annotate("text", x = as.Date(earliest_date), y = mean_count + 3*standard_deviation, label = "+3 SD", size = 3, color = "goldenrod4", hjust = -0.5, vjust = -0.75) +
  annotate("text", x = as.Date(earliest_date), y = mean_count, color = "gray21",label = "Average", size = 6, hjust = -0.5, vjust = -0.75) +
  annotate("text", x = max_date, y = max_value, label = "Max", size = 6, color = "firebrick3", hjust = -0.2, vjust = -0.5) +
  annotate("text", x = min_date, y = min_value, label = "Min", size = 6, color = "dodgerblue4", hjust = -0.2, vjust = -0.5)

# Print the bar chart
suppressMessages(print(SR_daily))
chart_path <- file.path(chart_directory_path, "Daily.png")
suppressMessages(ggsave(chart_path, plot = SR_daily, width = 18, height = 10))

#########################################################################
# Overall calendar month summary (Jan, Feb, Mar, etc.)
mean_count <- round(mean(calendar_month_df$count), 0)
median_count <- round(median(calendar_month_df$count), 0)
standard_deviation <- round(sd(calendar_month_df$count))

cat("\nAverage calendar month count:", format(mean_count, big.mark = ","), 
    "  Standard deviation:", format(standard_deviation, big.mark = ","))
cat("\nMedian:", format(median_count, big.mark = ","))

max_calendar_month <- calendar_month_df[which.max(calendar_month_df$count), ]
max_calendar_month$Month <- as.character(month.name[max_calendar_month$Month])
cat("\n\nCalendar month with maximum count:", max_calendar_month$Month, 
    "with", format(max_calendar_month$count, big.mark = ","), "SRs")
cat("\nPer day count (maximum):",max_calendar_month$count/31 )

min_calendar_month <- calendar_month_df[which.min((calendar_month_df$count)),]
min_calendar_month$Month <- as.character(month.name[min_calendar_month$Month])
cat("\n\nCalendar month with minimum count:", min_calendar_month$Month, 
    "with", format(min_calendar_month$count, big.mark = ","), "SRs")
cat("\nPer day count (minimum):",min_calendar_month$count/28.2 )

# Number of days in each month, with February as 28.2 days
days_in_month <- c(
  "January" = 31, "February" = 28.2, "March" = 31, "April" = 30, 
  "May" = 31, "June" = 30, "July" = 31, "August" = 31, 
  "September" = 30, "October" = 31, "November" = 30, "December" = 31
)

# Add the count_per_day column
calendar_month_df$count_per_day <- round(calendar_month_df$count / days_in_month[calendar_month_df$Month],0)

# Order by calendar month (Jan, Feb, etc.)
cat("\n\nSRs by month, ordered by calendar\n")
print(calendar_month_df, row.names = FALSE, right = FALSE)

# Order the dataframe by total count
calendar_month_df <- calendar_month_df %>%
  arrange(desc(count))
cat("\nSRs by month, ordered by count\n")
print(calendar_month_df, row.names = FALSE, right = FALSE)

# Order the dataframe by count_per_day
calendar_month_df <- calendar_month_df %>%
  arrange(desc(count_per_day))
cat("\nSRs by month, ordered by SRs/day\n")
print(calendar_month_df, row.names = FALSE, right = FALSE)

# Chart SRs by calendar month
max_count <- max(calendar_month_df$count)

earliest_month <- "January"

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
          subtitle = paste("(", earliest_title, "--", latest_title, ") ", "total=", total_count, sep = "")
  ) +
  geom_hline(
    yintercept = seq(starting_value, max_count, by = increment),
    linetype = "dotted", color = "gray21"
  ) +
  geom_hline(yintercept = mean_count, linetype = "solid", color = "gray21", linewidth = 0.4) +
  annotate("text", x = earliest_month, y = mean_count, label = "Average", size = 3, hjust = -0.5, vjust = -0.75) +
  geom_point(color = "transparent") +
  # geom_smooth(method = "lm", span = 1, se = FALSE, color = "firebrick3", linetype = "dotted", linewidth = 1.25) +
  scale_x_discrete() +
  labs(x=NULL, y=NULL)

# Print the bar chart
suppressMessages(print(SR_calendar_month))
chart_path <- file.path(chart_directory_path, "Calendar-Month.png")
suppressMessages(ggsave(chart_path, plot = SR_calendar_month, width = 10, height = 8))

#########################################################################
# Overall day-of-the-year summary (Jan 1st, Jan 2nd, Jan 3rd, etc.)
mean_count <- round(mean(day_counts_df$count), 0)
median_count <- round(median(day_counts_df$count), 0)
standard_deviation <- round(sd(day_counts_df$count))

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
scaling_factor <- result$scaling_factor
scaling_factor_str <- format(scaling_factor, scientific = FALSE, big.mark = ",")

# Convert day_of_year to a factor
day_counts_df$day_of_year <- as.factor(day_counts_df$day_of_year)

# Create the bar chart with vertical X-axis labels
SR_day_of_the_year <- ggplot(day_counts_df, aes(x = day_of_year, y = count)) +
  geom_bar(stat = "identity", fill = "cadetblue") +
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
  geom_hline(yintercept = mean_count, linetype = "solid", color = "gray21", linewidth = 0.4) +
  annotate("text", x = earliest_day_of_year, y = mean_count, label = "Average", size = 3, hjust = -0.5, vjust = -0.75) +
  geom_point(color = "transparent") +
  scale_x_discrete(breaks = day_counts$day_of_year[seq(1, nrow(day_counts), by = 19)]) +
  labs(y = NULL, x = NULL)

# Print the bar chart
suppressMessages(print(SR_day_of_the_year))
chart_path <- file.path(chart_directory_path, "day-of-the-year.png")
suppressMessages(ggsave(chart_path, plot = SR_day_of_the_year, width = 10, height = 8))

#########################################################################
# Overall day-of-the-week summary
mean_count <- round(mean(day_of_week_df$count), 0)
median_count <- round(median(day_of_week_df$count), 0)
standard_deviation <- round(sd(day_of_week_df$count))

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

# # Convert the "YearMonth" column to Date format
# monthly_df$YearMonth <- as.Date(paste0(monthly_df$YearMonth, "-01"))

cat("\n\nDay-of-the-Week Summary:\n")
print(day_of_week_df, row.names = FALSE, right = FALSE)

# Chart SRs by day-of-the-week
max_count <- max(day_of_week_df$count)
earliest_day_of_week <- "1-Monday"

result <- calculate_values(max_count)
starting_value <- result$starting_value
increment <- result$increment
scaling_factor <- result$scaling_factor
scaling_factor_str <- format(scaling_factor, scientific = FALSE, big.mark = ",")

# Create the bar chart with vertical X-axis labels
SR_day_of_the_week <- ggplot(day_of_week_df, aes(x = day_of_week, y = count)) +
  geom_bar(stat = "identity", fill = "cadetblue") +
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
  geom_hline(yintercept = mean_count, linetype = "solid", color = "gray21", linewidth = 0.4) +
  annotate("text", x = earliest_day_of_week, y = mean_count, label = "Average", size = 3, hjust = -0.5, vjust = -0.75) +
    geom_point(color = "transparent") +
  scale_x_discrete() +
  labs(x=NULL, y=NULL)

# Print the bar chart
suppressMessages(print(SR_day_of_the_week))
chart_path <- file.path(chart_directory_path, "day-of-the-week.png")
suppressMessages(ggsave(chart_path, plot = SR_day_of_the_week, width = 10, height = 8))

#########################################################################
# Overall created time-of-day summary (0900, 1000, 1100, 1200, 1300, etc.)
mean_count <- round(mean(created_hour_of_day_df$count), 0)
median_count <- round(median(created_hour_of_day_df$count), 0)
standard_deviation <- round(sd(created_hour_of_day_df$count))

cat("\nAverage hour-of-the-day 'created' count:", format(mean_count, big.mark = ","), 
    "  Standard deviation:", format(standard_deviation, big.mark = ","))
cat("\nMedian:", format(median_count, big.mark = ","))
# cat("\nMaximum is", format(max(created_hour_of_day_df$count), big.mark = ","),
#     "and Minimum is", format(min(created_hour_of_day_df$count), big.mark = ","))

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
scaling_factor <- result$scaling_factor
scaling_factor_str <- format(scaling_factor, scientific = FALSE, big.mark = ",")

# Create the bar chart with vertical X-axis labels
SR_created_time_of_day <- ggplot(created_hour_of_day_df, aes(x = created_hour, y = count)) +
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
  #geom_hline(yintercept = mean_count + 1*standard_deviation, linetype = "longdash", color = "goldenrod4", linewidth = 0.4) +
  geom_hline(yintercept = mean_count + 2*standard_deviation, linetype = "longdash", color = "goldenrod4", linewidth = 0.4) +
  geom_hline(yintercept = mean_count, linetype = "solid", color = "gray21", linewidth = 0.4) +
#  annotate("text", x = 0, y = mean_count + 1*standard_deviation, label = "+1 SD", size = 3, color = "goldenrod4", hjust = -0.5, vjust = -0.75) +
  annotate("text", x = 0, y = mean_count + 2*standard_deviation, label = "+2 SD", size = 3, color = "goldenrod4", hjust = -0.5, vjust = -0.75) +
  annotate("text", x = 0, y = mean_count, label = "Average", size = 3, hjust = -0.5, vjust = -0.75) +
  labs(x="hour-of-the-day (0-23)", y= NULL)

# Print the bar chart
suppressMessages(print(SR_created_time_of_day))
chart_path <- file.path(chart_directory_path, "created-time-of-the-day.png")
suppressMessages(ggsave(chart_path, plot = SR_created_time_of_day, width = 10, height = 8))

#########################################################################
# Overall closed time-of-day summary (0900, 1000, 1100, 1200, 1300, etc.)
mean_count <- round(mean(closed_hour_of_day_df$count), 0)
median_count <- round(median(closed_hour_of_day_df$count), 0)
standard_deviation <- round(sd(closed_hour_of_day_df$count))

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

# Chart by 'closed' time-of-the-day
max_count <- max(closed_hour_of_day_df$count)
earliest_date <- 0

result <- calculate_values(max_count)
starting_value <- result$starting_value
increment <- result$increment
scaling_factor <- result$scaling_factor
scaling_factor_str <- format(scaling_factor, scientific = FALSE, big.mark = ",")

# Create the bar chart with vertical X-axis labels
SR_closed_time_of_day <- ggplot(closed_hour_of_day_df, aes(x = closed_hour, y = count)) +
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
  ggtitle("SRs 'closed' count by time-of-the-day",
          subtitle = paste("(", earliest_title, "--", latest_title, ")", "total=", total_count)
  ) +
  geom_hline(
    yintercept = seq(starting_value, max_count, by = increment),
    linetype = "dotted", color = "gray21"
  ) +
  geom_point(color = "transparent") +
#  geom_hline(yintercept = mean_count + 1*standard_deviation, linetype = "longdash", color = "goldenrod4", linewidth = 0.4) +
  geom_hline(yintercept = mean_count + 2*standard_deviation, linetype = "longdash", color = "goldenrod4", linewidth = 0.4) +
#  geom_hline(yintercept = mean_count + 3*standard_deviation, linetype = "longdash", color = "goldenrod4", linewidth = 0.4) +
  geom_hline(yintercept = mean_count, linetype = "solid", color = "gray21", linewidth = 0.4) +
#  annotate("text", x = 0, y = mean_count + 1*standard_deviation, label = "+1 SD", size = 3, color = "goldenrod4", hjust = -0.5, vjust = -0.75) +
  annotate("text", x = 0, y = mean_count + 2*standard_deviation, label = "+2 SD", size = 3, color = "goldenrod4", hjust = -0.5, vjust = -0.75) +
#  annotate("text", x = 0, y = mean_count + 3*standard_deviation, label = "+3 SD", size = 3, color = "goldenrod4", hjust = -0.5, vjust = -0.75) +
    annotate("text", x = 0, y = mean_count, label = "Average", size = 3, hjust = -0.5, vjust = -0.75) +
  labs(x= "Hour-of-the-day (0-23)", y= NULL)

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
