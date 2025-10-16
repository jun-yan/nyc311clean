################################################################################

############### Program to examine data anomalies over time ###############

################################################################################
# Set the base directory under the working directory base_dir <- getwd()
working_dir <- getwd()

base_dir <- file.path(working_dir, "datacleaningproject", "nyc311clean",  
                      "data_quality_over_time" )

# Define the path for the main data file (CSV file)
data_dir <- file.path(base_dir, "data")
 
# Define the path for the charts
chart_dir <- file.path(base_dir, "charts")

# Define the path to the directory containing your function scripts
functions_path <- file.path(base_dir, "code", "functions")

######### Commence directing console output to the file ##########
# Define the path for the console output
console_dir <- file.path(base_dir, "console_output")
console_output_file <- file.path(console_dir, 
                                 "chartsquality_over_time_console_output.txt")

################################################################################
# Select desired data file
#main_data_file <- "2-year_311SR_01-01-2023_thru_12-31-2024_AS_OF_04-06-2025.rds"
#main_data_file <- "5-year_311SR_01-01-2020_thru_12-31-2024_AS_OF_04-06-2025.rds"
#main_data_file <- "10-year_311SR_01-01-2015_thru_12-31-2024_AS_OF_05-05-2025.rds"
main_data_file <- "15-year_311SR_01-01-2010_thru_12-31-2024_AS_OF_05-05-2025.rds"

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
  "fasttime",
  "gridExtra",
  "grid",
  "gt"
)

# Check and install missing packages
missing_packages <- required_packages[!(required_packages %in% 
                                          installed.packages()[, "Package"])]
if (length(missing_packages)) {
  install.packages(missing_packages)
  cat("📦 Installed missing packages:", paste(missing_packages, 
                                              collapse = ", "), "\n")
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

################################################################################
########## Source files in the function directory ##########
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
########## Functions ##########
analyze_invalid_values <- function(dataset, 
                                   field_name, 
                                   valid_values_list, 
                                   valid_field_name,
                                   sample_seed = 42
) {
  
  # Step 1: Remove NAs from the specified field
  data_no_na <- dataset[ !is.na(dataset[[field_name]]) & 
                                        dataset[[field_name]] != "", ]
  
  # Step 2: Inspect for invalid values. Add boolean column for valid values
  data_no_na$field_validity <- data_no_na[[field_name]] %in% 
                                        valid_values_list[[valid_field_name]]
  
  # Step 3: Find minimum count per year_month to use as sample size
  total_per_month <- data_no_na[, .N, by = year_month]
  min_count <- min(total_per_month$N)
  
  # Step 4: Create the sampled dataset
  sampled_data <- data.table()
  set.seed(sample_seed)
  
  for (month in unique(data_no_na$year_month)) {
    month_data <- data_no_na[year_month == month]
    month_sample <- month_data[sample(.N, min_count, replace = FALSE)]
    sampled_data <- rbind(sampled_data, month_sample)
  }
  
  # Step 4a: Inspect the sample for valid values
  sampled_data$field_validity <- sampled_data[[field_name]] %in%
                                      valid_values_list[[valid_field_name]]
  
  # Step 5: Standardized column names for consistency, dataset & sample
  full_summary <- data_no_na[, .(
    count = sum(!field_validity),
    N = .N,
    fraction = sum(!field_validity) / .N
  ), by = year_month]  # year_month is already grouped, no need to repeat
  
  sampled_summary <- sampled_data[, .(
    count = sum(!field_validity),
    N = .N,
    fraction = sum(!field_validity) / .N
  ), by = year_month]
  
  # Step 6: Add dataset column to match structure
  full_summary[, dataset := field_name]
  sampled_summary[, dataset := paste0(field_name, "_sampled")]
  
  return(list(full_summary
              , sampled_summary)
  )
}

################################################################################
create_condition_plot <- function( 
    data, 
    title = NULL, 
    y_label = NULL, 
    subtitle = NULL, 
    value_field = "fraction",
    bias_value = NULL,  # Changed to NULL by default to enable auto-detection
    date_field = "year_month",
    loess_threshold = 10,
    auto_scale = TRUE  # New parameter to enable/disable auto-scaling
) {
  
  # Convert to data.table for efficient handling
  data <- as.data.table(data)
  
  # Coerce the date_field to date if it is not already a date/time class
  if (!inherits(data[[date_field]], c("POSIXct", "POSIXt", "Date"))) {
    cat("\nConverting", title, "to date format...")
    
    # If format looks like "YYYY-MM", assume it's monthly and add "-01"
    if (is.character(data[[date_field]]) &&
        all(grepl("^\\d{4}-\\d{2}$", na.omit(data[[date_field]])))) {
      data[[date_field]] <- as.Date(paste0(data[[date_field]], "-01"))
    } else {
      stop(paste("Unable to coerce", date_field, "to POSIXct format."))
    }
  }
  
  # Calculate process mean before any bias is applied
  raw_process_mean <- mean(data[[value_field]], na.rm = TRUE)
  
  # Auto-detect if values are small and set bias accordingly
  if (auto_scale && is.null(bias_value)) {
    if (raw_process_mean < 0.001) {
      bias_value <- 1000  # For extremely small values (< 0.001)
      y_label_suffix <- " (×1000)"
    } else if (raw_process_mean < 0.01) {
      bias_value <- 100   # For very small values (< 0.01)
      y_label_suffix <- " (×100)"
    } else if (raw_process_mean < 0.1) {
      bias_value <- 10    # For small values (< 0.1)
      y_label_suffix <- " (×10)"
    } else {
      bias_value <- 1     # For normal values
      y_label_suffix <- ""
    }
    
    # Update y-axis label to indicate scaling
    if (!is.null(y_label) && bias_value > 1) {
      y_label <- paste0(y_label, y_label_suffix)
    }
    
    cat("\nAuto-scaling applied for", title, "- Bias factor:", bias_value)
  } 
  
  # Apply bias
  data[[value_field]] <- data[[value_field]] * bias_value
  
  # Rest of the function remains the same...
  # Extract the date column once for reuse
  date_vector <- data[[date_field]]
  data$year <- year(date_vector)
  data$condition_name <- title
  
  # Save full dataset to global environment for inspection/debug
  all_results <<- list()
  all_results[[title]] <- data
  
  # Determine first and last year
  first_year <- min(data$year, na.rm = TRUE)
  last_year  <- max(data$year, na.rm = TRUE)
  
  # Compute means for first and last year
  year_data_first <- data[year == first_year]
  year_data_last  <- data[year == last_year]
  
  mean_first_year <- mean(year_data_first[[value_field]], na.rm = TRUE)
  mean_last_year  <- mean(year_data_last[[value_field]], na.rm = TRUE)
  
  # x-axis segment endpoints
  first_year_start <- min(year_data_first[[date_field]])
  first_year_end   <- max(year_data_first[[date_field]])
  
  last_year_start  <- min(year_data_last[[date_field]])
  last_year_end    <- max(year_data_last[[date_field]])
  
  # Global stats (now using the biased values)
  process_mean <- mean(data[[value_field]], na.rm = TRUE)
  process_sd   <- sd(data[[value_field]], na.rm = TRUE)
  
  # Determine smoothing method based on number of data points
  smooth_method <- ifelse(nrow(data) >= loess_threshold, "loess", "lm")
  
  # Create the plot with basic elements
  chart_object <- ggplot(data, aes(x = .data[[date_field]], 
                                   y = .data[[value_field]])) +
    geom_line(color = "steelblue2", linewidth = 0.55) +
    geom_point(size = 3.5, color = "steelblue4", shape = 18) +
    geom_hline(yintercept = process_mean, color = "darkred", 
               linetype = "dashed", linewidth = 0.8)
  
  # Add appropriate geom_smooth based on method
  if (smooth_method == "loess") {
    chart_object <- chart_object +
      geom_smooth(method = "loess", se=TRUE, color = "gray40", fill = "gray85", 
                  alpha = 0.3, linewidth = 0.85, span = 0.9)
  } else {
    chart_object <- chart_object +
      geom_smooth(method = "lm", se = TRUE, color = "gray40", fill = "gray85", 
                  alpha = 0.3, linewidth = 0.85)
  }
  
  # Add the rest of the plot elements
  chart_object <- chart_object +
    # Overall mean label
    annotate("label", 
             x = min(date_vector) + (max(date_vector) - min(date_vector))/2,
             y = process_mean,
             label = paste("Overall mean:", format(round(process_mean, 5), 
                                                   nsmall = 5)),
             hjust = 0.5, vjust = -0.2, size = 3.5, color = "darkred", 
             fill = "gray99", label.size = NA) +
    
    # Segment lines and mean labels
    annotate("segment", x = first_year_start, xend = first_year_end,
             y = mean_first_year, yend = mean_first_year,
             color = "black", linetype = "dashed", linewidth = 1) +
    annotate("segment", x = last_year_start, xend = last_year_end,
             y = mean_last_year, yend = mean_last_year,
             color = "black", linetype = "dashed", linewidth = 1) +
    
    annotate("label", x = first_year_start, y = mean_first_year,
             label = paste0(first_year, " mean: ", 
                            format(round(mean_first_year, 5), nsmall = 5)),
             hjust = 0, vjust = -0.2, size = 4, color = "black", 
             fill = "gray99", label.size = NA) +
    annotate("label", x = last_year_end, y = mean_last_year,
             label = paste0(last_year, " mean: ", 
                            format(round(mean_last_year, 5), nsmall = 5)),
             hjust = 1, vjust = -0.2, size = 4, color = "black", 
             fill = "gray99", label.size = NA) +
    
    labs(
      title = title,
      subtitle = paste(subtitle, "\n\n"),
      x = NULL,
      y = y_label,
      caption = paste("Mean:", format(round(process_mean, 5), nsmall = 5), 
                      ifelse(bias_value > 1, paste0("(Raw mean: ", 
                                                    format(round(raw_process_mean, 5), 
                                                           nsmall = 5), ")"), ""))
    ) +
    scale_x_date(
      date_breaks = "1 year",
      date_labels = "%Y",
      expand = expansion(mult = c(0.05, 0.1))
    ) +
    scale_y_continuous(labels = function(x) format(round(x, 2), nsmall = 5)) +
    theme(
      plot.title = element_text(size = 14, face = "bold", color = "black", 
                                margin = margin(b = 10)),
      plot.subtitle = element_text(size = 11, color = "black", 
                                   margin = margin(b = 15)),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.75),
      axis.text.x = element_text(angle = 20, hjust = 1, color = "black"),  
      panel.grid.major = element_line(color = "gray88", linewidth = 0.5),  
      panel.grid.minor = element_blank(),
      plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"),  
      panel.background = element_rect(fill = "gray99", color = NA),  
      plot.background = element_rect(fill = "gray90", color = NA),   
      plot.caption = element_text(hjust = 1, size = 9, color = "black"),
      axis.title.y = element_text(margin=margin(r = 10, l = 0), color = "black"),
      axis.title.x = element_text(color = "black"),
      axis.text.y = element_text(color = "black")
    )
  
  return(list(
    plot = chart_object,
    title = title,
    first_year_mean = mean_first_year,
    last_year_mean = mean_last_year,
    smooth_method = smooth_method,
    bias_value = bias_value,  # Return the bias value that was used
    raw_process_mean = raw_process_mean  # Return the original mean before scaling
  ))
}

################################################################################
# For analyze_data_conditions function:
analyze_data_conditions <- function(
                                    data, 
                                    sample_seed = 42)
{
  
  data <- copy(data)
  
  ####################
  # Calculate the minimum monthly count to use as sample size
  total_per_month <- data[, .N, by = year_month]
  min_count <- min(total_per_month$N)
  
  # Take random samples of equal size from each month
  sampled_data <- data.table()
  
  # Set seed for reproducibility
  set.seed(sample_seed)
  
  # Loop through each month and collect a sample of rows
  sampled_list <- vector("list", length(unique(data$year_month)))
  i <- 1
  for (month in unique(data$year_month)) {
    month_data <- data[year_month == month]
    month_sample <- month_data[sample(.N, min_count, replace = FALSE)]
    sampled_list[[i]] <- month_sample
    i <- i + 1
  }
  sampled_data <- rbindlist(sampled_list)
  
  ####################
  # Create indicators for all conditions (in both sampled and full data)
  # First for sampled data
  sampled_data[, SRs_with_a_midnight_closed_date := 
                 ifelse(!is.na(closed_date), 
                        format(closed_date, "%H:%M:%S") == "00:00:00", FALSE)]
  
  sampled_data[, SRs_with_a_resolution_update_date_occurring_before_creation := 
           ifelse(!is.na(resolution_action_updated_date) & !is.na(created_date), 
                  resolution_action_updated_date < created_date, FALSE)]
  
  sampled_data[, SRs_with_not_closed_status_but_with_a_closed_date := 
                 ifelse(!is.na(closed_date) & !is.na(status), 
                      !is.na(closed_date) & status != "CLOSED", FALSE)]
  
  sampled_data[, SRs_with_status_set_to_closed_but_no_closed_date 
               := ifelse(!is.na(status), status == "CLOSED" 
                         & is.na(closed_date), FALSE)]
  
  sampled_data[, SRs_with_late_resolution_updates := 
             ifelse(!is.na(closed_date) & !is.na(resolution_action_updated_date), 
             as.numeric(difftime(resolution_action_updated_date, 
                                 closed_date, units = "days")) > 90, FALSE)]
  
  sampled_data[, SRs_with_negative_or_zero_duration := 
                 ifelse(!is.na(closed_date) & !is.na(created_date), 
                                           closed_date <= created_date, FALSE)]
  
  sampled_data[, SRs_with_cross_intersection_1_matching_intersection_street_1 := 
                 ifelse(cross_street_1 != intersection_street_1, FALSE, TRUE)]
  
  sampled_data[, SRs_with_cross_intersection_2_matching_intesection_street_2 := 
                 ifelse(cross_street_2 != intersection_street_2, FALSE, TRUE)]
  
  sampled_data[, SRs_wth_street_name_matching_landmark := 
                 ifelse(street_name != landmark, FALSE, TRUE)]
  
  ####################
  # Now for full data
  data[, SRs_with_a_midnight_closed_date := 
         ifelse(!is.na(closed_date), 
                format(closed_date, "%H:%M:%S") == "00:00:00", FALSE)]
  
  data[, SRs_with_a_resolution_update_date_occurring_before_creation := 
         ifelse(!is.na(resolution_action_updated_date) & 
                  !is.na(created_date), resolution_action_updated_date 
                < created_date, FALSE)]
  
  data[, SRs_with_not_closed_status_but_with_a_closed_date := 
         ifelse(!is.na(closed_date) & !is.na(status), 
                !is.na(closed_date) & status != "CLOSED", FALSE)]
  
  data[, SRs_with_status_set_to_closed_but_no_closed_date := 
         ifelse(!is.na(status), status == "CLOSED" & is.na(closed_date), FALSE)]
  
  data[, SRs_with_late_resolution_updates := ifelse(!is.na(closed_date) & 
                            !is.na(resolution_action_updated_date), 
                            as.numeric(difftime(resolution_action_updated_date, 
                                closed_date, units = "days")) >90, FALSE)]
  
  data[, SRs_with_negative_or_zero_duration := 
         ifelse(!is.na(closed_date) & !is.na(created_date), 
                closed_date <= created_date, FALSE)]
  
  data[, SRs_with_cross_intersection_1_matching_intersection_street_1 := 
         ifelse(cross_street_1 != intersection_street_1, FALSE, TRUE)]
  
  data[, SRs_with_cross_intersection_2_matching_intesection_street_2  := 
         ifelse(cross_street_2 != intersection_street_2, FALSE, TRUE)]
  
  data[, SRs_wth_street_name_matching_landmark  := 
         ifelse(street_name != landmark, FALSE, TRUE)]
  
  ####################
  # Create a list to store all results
  results <- list()
  
  # Define conditions
  conditions <- c(
    "SRs_with_a_midnight_closed_date", 
    "SRs_with_a_resolution_update_date_occurring_before_creation", 
    "SRs_with_not_closed_status_but_with_a_closed_date", 
    "SRs_with_status_set_to_closed_but_no_closed_date", 
    "SRs_with_late_resolution_updates", 
    "SRs_with_negative_or_zero_duration",
    "SRs_with_cross_intersection_1_matching_intersection_street_1",
    "SRs_with_cross_intersection_2_matching_intesection_street_2",
    "SRs_wth_street_name_matching_landmark"
  )
  
  ####################
  # Process each condition for both sampled and full data
  for (condition in conditions) {
    
    # 1. For sampled data (for QCC and QIC)
    sampled_result <- sampled_data[, .(
      count = sum(get(condition), na.rm = TRUE),
      N = .N,
      fraction = sum(get(condition), na.rm = TRUE) / .N
    ), by = year_month]
    
    # 2. For full data (for ggplot)
    full_result <- data[, .(
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
    results[[paste0(condition, "_sampled")]] <- copy(sampled_result)
    results[[condition]] <- copy(full_result)
  }
  
  return(results)
}

################################################################################

#========= Main Execution =========

################################################################################
#sink(console_output_file)

cat("\nExecution begins at:", formattedStartTime)

# Process 311 data
cat("\n\nReading in 311 Service Request data...")
file_path <- file.path(data_dir, main_data_file )
cleaned_data <- readRDS(file_path)

# Ensure cleaned_data is a data.table
setDT(cleaned_data)

# Vector of columns to keep
columns_to_keep <- c("unique_key",
                     "created_date",
                     "closed_date",
                     "incident_zip",
                     "status",
                     "resolution_action_updated_date",
                     "community_board",
                     "cross_street_1",
                     "intersection_street_1",
                     "cross_street_2",
                     "intersection_street_2",
                     "street_name",
                     "landmark",
                     "borough"
                     )

# Retain only those columns
cleaned_data <- cleaned_data[, ..columns_to_keep]

cat("\n\nData set row count:", format(nrow(cleaned_data), big.mark=","))

# Extract the 10-character date after "AS_OF_"
max_closed_date <- sub(".*AS_OF_([0-9-]+)\\.csv$", "\\1", main_data_file)

# Convert to POSIXct format
max_closed_date <- as.POSIXct(max_closed_date, format = "%m-%d-%Y", 
                              tz = "America/New_York") + (23*3600 + 59*60 + 59)

cat("\n\nDataset Created Dates span from", 
    format(min(cleaned_data$created_date), "%Y-%m-%d %H:%M:%S"), "to", 
    format(max(cleaned_data$created_date), "%Y-%m-%d %H:%M:%S"))

################################################################################      
# Extract year & year-month for grouping
cat("\n\nCreating year_month grouping...")

cleaned_data[, `:=` (
  year = as.integer(format(created_date, "%Y")),
  year_month = format(created_date, "%Y-%m")
)]

################################################################################
#Count records by year (already extracted) and add a numeric column for plotting
cat("\nCreating yearly growth chart...\n")

yearly_counts <- cleaned_data[, .N, by = year][order(year)]
setnames(yearly_counts, "N", "count")
yearly_counts[, year_numeric := as.numeric(year)]  # Optional if year is integer

# Add the projected value for 2025
setnames(yearly_counts, "count", "n")  # Rename to match your ggplot code

yearly_counts <- yearly_counts %>%
  bind_rows(tibble(year = 2025, n =  3517953, year_numeric = 2025)) %>%
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

# # Format the trend label with + or - sign and include R²
trend_label <- ifelse(total_percent_change >= 0,
                      paste0("+", format(round(total_percent_change, 1),
                                         nsmall = 1),
                             "%   (R²: ", r_squared, ")"),
                      paste0(format(round(total_percent_change, 1), nsmall = 1),
                             "%   (R²: ", r_squared, ")"))

# Create the bar chart with trend line
growth_chart <- ggplot(yearly_counts, aes(x = year, y = n, fill = status)) +
  
  # Add bars with different colors for actual vs projected
  geom_col(alpha = 0.7) +
  
  # Custom fill colors
  scale_fill_manual(values = c("Actual" = "dodgerblue4", "
                               Projected" = "ivory4")) +
  
  # Add trend line
  geom_smooth(aes(x = year_numeric, y = n), method = "lm", 
              color = "darkgoldenrod1", se = FALSE, linetype = "twodash", 
              linewidth = 1.2) +
  
  # Add count labels on top of bars
  geom_text(aes(label = paste0(format(round(n/1e6, 2), nsmall = 2))), 
            vjust = -0.5, size = 3.5) +  
  
  # Add trend annotation
  annotate("text", 
           x = mean(yearly_counts$year), 
           y = max(yearly_counts$n) * 0.9,  
           label = paste("Trend:", trend_label ),
           hjust = 0.95, vjust = -1.9,  
           color = "gray30", fontface = "bold", size = 5.5) +
  annotate("text",
           x = 2025,
           y = yearly_counts$n[yearly_counts$year == 2025] * 0.5, 
           label = "Projected",
           angle = 90,
           color = "red",
           vjust = 0,
           size = 4,
           fontface = "bold") +

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

# Add a 2 second delay to let plots render completely
cat("\nWaiting 2 seconds before continuing to next chart...\n")
Sys.sleep(2)   

file_path <- paste0(chart_dir, "/year_over_year.pdf")

# Save the last printed plot
ggsave(file_path, 
       growth_chart, 
       width = 13, 
       height = 8.5)

################################################################################
# Analyze all conditions
cat("\nAnalyzing condition results...\n")

data_condition_results <- analyze_data_conditions(cleaned_data)

# Define updated subtitles with your preferred descriptions
subtitles <- list(
  
  SRs_with_a_midnight_closed_date = "closed_date occurs exactly at 00:00:00",
  
  SRs_with_a_resolution_update_date_occurring_before_creation = 
              "resolution_action_updated_date occurs before created_date",
  
  SRs_with_not_closed_status_but_with_a_closed_date = 
              "closed_date exists but status is not CLOSED",
  
  SRs_with_status_set_to_closed_but_no_closed_date = 
              "status is CLOSED but there is no closed_date",
  
  SRs_with_late_resolution_updates = 
              "resolution_action_updated_date is >90 days after closed_date",
  
  SRs_with_negative_or_zero_duration = 
                "closed_date is <= created_date (infeasible duration)",
  
  SRs_with_cross_intersection_1_matching_intersection_street_1 = "cross_street_1 matches intersection_street_1",
  
  SRs_with_cross_intersection_2_matching_intesection_street_2 = "cross_street_2 matches intersection_street_2",
  
  SRs_wth_street_name_matching_landmark = "street_name matches landmark"
)

# Filter just the original (non-sampled) condition names
original_conditions <- names(data_condition_results)[!grepl("_sampled$",
                                                names(data_condition_results))]
# Initialize a list to store summaries
condition_summary <- list()

# Global list
all_results <- list()

# At the beginning of your script
graphics.off()  # Just once to ensure a clean start

####################
# Loop through only the original conditions
for (condition in original_conditions) {
  
  full_data <- copy(data_condition_results[[condition]])
  setDT(full_data)  # Ensure proper data.table references
  
  sampled_data <- copy(data_condition_results[[paste0(condition, "_sampled")]])
  setDT(sampled_data)  # Ensure proper data.table references
  
  cat("\n\n======= CONDITION:", condition, "=======\n")

  # === 1. ggplot chart (FULL data) ===
  result <- create_condition_plot(
    data = full_data,
    paste("Proportion of", gsub("_", " ", tools::toTitleCase(condition)), 
                                                      "w/loess Fitting"), 
    "Non-conforming Proportion",
    subtitle = subtitles[[condition]],
    "fraction"
  )
  
  gg_plot <- result$plot
  
  # Print ggplot to the new device
  print(gg_plot)
  
  cat("\nWaiting 2 seconds before continuing to next chart...\n")
  Sys.sleep(2)
  
  # Save to file without affecting the display device
  ggsave(paste0(chart_dir, "/", condition, ".pdf"), 
         plot = gg_plot, 
         width = 13,
         height = 8.5)
  
  condition_summary[[condition]] <- list(
    title = result$title,
    first_year_mean = result$first_year_mean,
    last_year_mean = result$last_year_mean
  )
  
  # === 2. QCC chart ===
  count_data <- sampled_data$count
  sample_sizes <- sampled_data$N
  chart_title <- paste("QCC p-chart of", gsub("_", " ", 
                                              tools::toTitleCase(condition)))
  x_labels <- format(sampled_data$year_month, "%Y-%m")

  ####################
  # Create QCC object without plotting
  qcc_plot <- qcc(count_data,
                    sizes = sample_sizes,
                    type = "p",
                    title = chart_title,
                    xlab = "Year-Month",
                    ylab = "Non-conforming Proportion",
                    labels = x_labels,
                    plot = TRUE)
    
   # Save to PDF using separate device that won't affect display
  pdf(paste0(chart_dir, "/qcc_p_chart_", condition, ".pdf"), width = 13, 
        height = 8.5)
  plot(qcc_plot, title = chart_title)
  dev.off()
  
  cat("\nWaiting 2 seconds before continuing to next chart...\n")
  Sys.sleep(2)
} 

####################
# Create a data frame from the summaries
summary_df <- data.frame(
  condition = names(condition_summary),
  title = sapply(condition_summary, function(x) x$title),
  first_year_mean = sapply(condition_summary, function(x) x$first_year_mean),
  last_year_mean  = sapply(condition_summary, function(x) x$last_year_mean)
)

################################################################################

########## Zip Code Evaluation ##########

################################################################################
# Process USPS data
usps_zipcodes <- readRDS(file.path(data_dir, "USPS_zipcodes.rds"))
setDT(usps_zipcodes)

# valid_zips <- usps_zipcodes$zip
# 
# # Step 1: Filter cleaned_data to exclude NA and blank values
# dt <- cleaned_data[!is.na(incident_zip) & incident_zip != "",]
# 
# # Step 2: Clean incident_zip and extract 5-digit ZIP from end of string 
# # (after removing ZIP+4 if present)
# dt[, incident_zip_clean := trimws(incident_zip)]
# 
# # Extract ZIP5: remove ZIP+4 suffix and extract trailing 5-digit ZIP
# dt[, zip5 := toupper(str_extract(
#   gsub("[- /][0-9]{1,4}$", "", incident_zip_clean),
#   "[0-9]{5}$"
# ))]
# 
# # Step 3: Check if zip5 is a strictly valid ZIP format (i.e., exactly 5 digits)
# dt[, is_format_valid_zip := grepl("^[0-9]{5}$", zip5)]
# 
# # Step 4: Check against reference ZIPs
# dt[, is_in_ref := zip5 %in% valid_zips]
# 
# # Step 5: Flag invalid ZIPs
# dt[, is_invalid_zip := !(is_format_valid_zip & is_in_ref)]
# 
# # Step 6: Table of invalid ZIPs (from original values)
# invalid_zip_table <- dt[is_invalid_zip == TRUE, .N, 
#                         by = incident_zip_clean][order(-N)]
# fwrite(invalid_zip_table, file = file.path(data_dir, "invalid_zip_codes.csv"))
# 
# # Step 7: Count invalid ZIPs by year_month
# invalid_counts <- dt[is_invalid_zip == TRUE, .N, by = year_month]
# setnames(invalid_counts, "N", "num_invalid_zipcodes")
# 
# # Step 8: Count total valid incidents (non-empty, non-NA ZIPs)
# total_valid_counts <- cleaned_data[!is.na(incident_zip) & incident_zip != "", 
#                                    .N, by = year_month]
# setnames(total_valid_counts, "N", "total_valid_incidents")
# 
# # Step 10: Merge and compute fraction
# summary_stats <- merge(invalid_counts, total_valid_counts, by = "year_month", 
#                        all = TRUE)
# 
# # Fill NAs *before* computing fraction
# summary_stats[is.na(num_invalid_zipcodes), num_invalid_zipcodes := 0]
# summary_stats[is.na(total_valid_incidents), total_valid_incidents := 0]
# 
# # Now safely compute fraction
# summary_stats[, fraction_invalid := num_invalid_zipcodes / total_valid_incidents]
# 
# summary_stats$fraction_invalid <- summary_stats$fraction_invalid # bias
# 
# # Step 11: Save summary stats
# fwrite(summary_stats, file = file.path(data_dir, 
#                                        "invalid_zip_summary_stats.csv"))
# 
# # Optional: Print results
# cat("\n")
# print(head(invalid_zip_table, 40))
# #print(summary_stats[order(-year_month)])
# 
# #######################
# # Create ggplot for ZIP codes
# zip_gg_plot <- create_condition_plot(
#                   data = summary_stats,
#                   title = "Proportion of Invalid ZIP Codes w/loess fitting",
#                   y_label = "Non-conforming Proportion",
#                   subtitle = "incident_zip codes not in USPS database",
#                   value_field = "fraction_invalid",
#                   bias_value = 1
# )
# 
# # Extract details from the plot result
# zip_title <- zip_gg_plot$title
# zip_first_mean <- zip_gg_plot$first_year_mean
# zip_last_mean  <- zip_gg_plot$last_year_mean
# zip_plot <- zip_gg_plot$plot
# 
# # Display and save the ggplot
# print(zip_plot)
# 
# # Delay between charts
# cat("\nWaiting 2 seconds between charts...\n")
# Sys.sleep(2)
# 
# # Access the plot element from the list
# file_path <- paste0(chart_dir, "/invalid_zipcodes.pdf")
# ggsave(file_path, 
#        plot = zip_gg_plot$plot, 
#        width = 13, 
#        height = 8.5)
# 
# #######################
# # Create QCC chart for ZIP codes
# set.seed(42)
# 
# # Step 1: Compute the minimum number of rows per year_month
# min_count <- dt[, .N, by = year_month][, min(N)]
# 
# # Step 2: Sample min_count rows per year_month
# dt_sampled <- dt[, .SD[sample(.N, min_count)], by = year_month]
# 
# # Step 3: Define variables explicitly
# 
# # Count of TRUE is_invalid_zip per year_month
# # Step 1: Get all year_month values (i.e., sample structure)
# all_months <- dt_sampled[, .(year_month = unique(year_month))]
# 
# # Step 2: Count TRUE is_invalid_zip values by year_month
# zip_counts <- dt_sampled[is_invalid_zip == TRUE, .N, by = year_month]
# 
# # Step 3: Left join to include all months, even those with 0
# zip_count_data <- merge(all_months, zip_counts, by = "year_month", all.x = TRUE)
# 
# # Step 4: Replace NA with 0
# zip_count_data[is.na(N), N := 0]
# 
# # Sample size per year_month (constant, but computed for verification)
# zip_sample_sizes <- dt_sampled[, .N, by = year_month][order(year_month)]
# 
# # Extract just the count and sample size vectors
# zip_counts <- zip_count_data$invalid_zip_count
# zip_sizes <- zip_sample_sizes$N
# zip_chart_title <- "QCC p-chart of Invalid Zipcodes"
# 
# # Create and display QCC chart directly
# zip_qcc_plot <- qcc(summary_stats$N,  # Count of defects
#  #                   sizes = zip_sizes,  # Sample sizes
#                     type = "p",  # Proportion chart
#                     title = zip_chart_title,
#                     xlab = "Year-Month",
#                     ylab = "Non-conforming Proportion",
#                     labels = zip_sample_sizes$year_month,
#                     plot = TRUE)  # This will display the plot in RStudio
# 
# # Add a 2 second delay to let plots render completely
# cat("\nWaiting 2 seconds before continuing to next chart...\n")
# Sys.sleep(3)
# 
# # Save QCC chart (keeping the separate PDF device since QCC requires it)
# pdf(paste0(chart_dir, "/qcc_p_chart_invalid_zipcodes.pdf"), 
#                                   width = 13, height = 8.5)
# plot(zip_qcc_plot, title = zip_chart_title)
# dev.off()


# Step 1: Copy cleaned_data to preserve original
zip_check_data <- copy(cleaned_data)

# Step 2: Extract first 5 characters of incident_zip
zip_check_data[, incident_zip_5 := 
                 substr(trimws(as.character(incident_zip)), 1, 5)]

# Step 3: Identify and write malformed ZIPs (not 5 digits)
invalid_zip_table <- zip_check_data[
  !grepl("^\\d{5}$", incident_zip_5) | is.na(incident_zip_5),
  .N,
  by = .(original_incident_zip = incident_zip)
][order(-N)]

fwrite(invalid_zip_table, file = file.path(data_dir, "invalid_zip_codes.csv"))

# Step 4: Keep only rows with valid 5-digit ZIPs (formatted correctly)
zip_cleaned_data <- zip_check_data[grepl("^\\d{5}$", incident_zip_5)]
zip_cleaned_data[, incident_zip := incident_zip_5]  # overwrite incident_zip
zip_cleaned_data[, incident_zip_5 := NULL]          # drop helper column

# Step 5: Derive valid ZIP reference from data itself
valid_zips_clean <- unique(zip_cleaned_data[, .(zip = incident_zip)])


# Step 6: Run analyze_invalid_values
incident_zip_results <- analyze_invalid_values(
  dataset = zip_cleaned_data,
  field_name = "incident_zip",
  valid_values_list = usps_zipcodes,
  valid_field_name = "zip"
)

# Step 7: Extract results
incident_zip_full   <- incident_zip_results[[1]]
incident_zip_sample <- incident_zip_results[[2]]

# Step 8: Extract logically invalid ZIPs (those not in reference list)
# Ensure ZIP is character and trimmed
zip_cleaned_data[, incident_zip := trimws(as.character(incident_zip))]

# Filter: ZIPs not in the USPS reference list
invalid_zip_details <- zip_cleaned_data[!incident_zip %in% usps_zipcodes$zip]

# Summary table
invalid_zip_summary <- invalid_zip_details[, .N, by = .(incident_zip)][order(-N)]

# Save to disk
fwrite(invalid_zip_details, file = file.path(data_dir, 
                                             "invalid_zip_logical_details.csv"))
fwrite(invalid_zip_summary, file = file.path(data_dir, 
                                             "invalid_zip_logical_summary.csv"))

# Step 9: Create the year-month colums
# Ensure both are data.tables
setDT(incident_zip_full)
setDT(incident_zip_sample)

# Convert year_month to Date and sort
incident_zip_full[, year_month := as.Date(paste0(year_month, "-01"))]
incident_zip_sample[, year_month := as.Date(paste0(year_month, "-01"))]

setorder(incident_zip_full, year_month)
setorder(incident_zip_sample, year_month)

# Step 10: Generate the line chart
# Create ggplot for incident ZIPs
zip_gg_plot <- create_condition_plot(
  data = incident_zip_full,
  title = "Proportion of Invalid Incident ZIPs w/loess fitting",
  y_label = "Non-conforming Proportion",
  subtitle = "Incident ZIPs not found in USPS reference list",
  value_field = "fraction"
)

# Display plot
print(zip_gg_plot$plot)  # Use $plot to access ggplot object from list

# Save plot to PDF
file_path <- file.path(chart_dir, "invalid_incident_zips.pdf")
ggsave(file_path,
       plot = zip_gg_plot$plot,
       width = 13,
       height = 8.5)

# Pause to allow rendering
cat("\nWaiting 2 seconds between charts...\n")
Sys.sleep(2)

# Step 11: Generate the qcc p-chart
# Extract metadata from ggplot object
zip_title <- zip_gg_plot$title
zip_first_mean <- zip_gg_plot$first_year_mean
zip_last_mean  <- zip_gg_plot$last_year_mean

# Define variables for QCC
zip_count_data <- incident_zip_sample$count
zip_sample_sizes <- incident_zip_sample$N
zip_chart_title <- "QCC p-chart of Invalid Incident ZIPs"
zip_labels <- format(incident_zip_sample$year_month, "%Y-%m")

# Create and display QCC chart
zip_qcc_plot <- qcc(zip_count_data,
                    sizes = zip_sample_sizes,
                    type = "p",
                    title = zip_chart_title,
                    xlab = "Year-Month",
                    ylab = "Non-conforming Proportion",
                    labels = zip_labels,
                    plot = TRUE)

# Let plot render before next chart
cat("\nWaiting 2 seconds before continuing to next chart...\n")
Sys.sleep(2)

# Save QCC chart to PDF
pdf(file.path(chart_dir, "qcc_p_chart_invalid_incident_zips.pdf"),
    width = 13, height = 8.5)
plot(zip_qcc_plot, title = zip_chart_title)
dev.off()



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
                              cleaned_data, 
                              "community_board", 
                              valid_cb, "cb")

# Extract the data
community_board_full <- community_board_results[[1]]
community_board_sample <- community_board_results[[2]]

# Convert year_month to Date
community_board_full$year_month <- 
                      as.Date(paste0(community_board_full$year_month, "-01"))
community_board_sample$year_month <- 
        as.Date(paste0(community_board_sample$year_month, "-01"))

# Arrange by year_month from oldest to newest
community_board_full <- community_board_full %>% arrange(year_month)
community_board_sample <- community_board_sample %>% arrange(year_month)

####################### 
# Create ggplot for community boards
cb_gg_plot <- create_condition_plot(
  data = community_board_full,
  title = "Proportion of Invalid Community Boards w/loess fitting",
  y_label = "Non-conforming Proportion",
  subtitle = "Community Boards that do not exist",
  value_field = "fraction"
)

print(cb_gg_plot$plot)  # Note the $plot to access the plot from the return list

# Display and save the ggplot
file_path <- paste0(chart_dir, "/invalid_community_boards.pdf")
ggsave(file_path, 
       plot = cb_gg_plot$plot, 
       width = 13, 
       height = 8.5)

# Delay between charts
cat("\nWaiting 2 seconds between charts...\n")
Sys.sleep(2)

# Extract details from the plot result
cb_title <- cb_gg_plot$title
cb_first_mean <- cb_gg_plot$first_year_mean
cb_last_mean  <- cb_gg_plot$last_year_mean

# Create QCC chart for community boards
# Define variables explicitly
cb_count_data <- community_board_sample$count
cb_sample_sizes <- community_board_sample$N
cb_chart_title <- "QCC p-chart of Invalid Community Boards"
cb_labels <- format(community_board_sample$year_month, "%Y-%m")
  
# Create and display QCC chart directly
cb_qcc_plot <- qcc(cb_count_data,  # Use invalid_zips from the sample data
                   sizes = cb_sample_sizes,  # Use total_records as sizes
                   type = "p",
                   title = cb_chart_title,
                   xlab = "Year-Month",
                   ylab = "Non-conforming Proportion",
                   labels = cb_labels,
                   plot = TRUE)  # This will display the plot in RStudio

# Add a 2 second delay to let plots render completely
cat("\nWaiting 2 seconds before continuing to next chart...\n")
Sys.sleep(2)

# Save QCC chart - with explicit title
pdf(paste0(chart_dir, "/qcc_p_chart_invalid_community_boards.pdf"),
    width = 13, height = 8.5)
plot(cb_qcc_plot, title = cb_chart_title)
dev.off()

####################### Add zipcode and cb values to summary table.
# New conditions data frame
new_conditions_df <- data.frame(
  condition = c("invalid_zipcodes", "invalid_community_boards"),
  title = c(zip_title, cb_title),
  first_year_mean = c(zip_first_mean, cb_first_mean),
  last_year_mean  = c(zip_last_mean, cb_last_mean)
)

# Add delta and percentage columns
new_conditions_df$delta <- new_conditions_df$last_year_mean - 
                                              new_conditions_df$first_year_mean
new_conditions_df$percent_change <- round(
  100 * new_conditions_df$delta / new_conditions_df$first_year_mean, 2)

# Add delta and percentage columns
summary_df$delta <- summary_df$last_year_mean - summary_df$first_year_mean
summary_df$percent_change <- round(
  100 * summary_df$delta / summary_df$first_year_mean, 2)

# new_conditions_df <- new_conditions_df %>% select(-title)
# summary_df <- summary_df %>% select(-title)

# Append new conditions to summary_df
summary_df <- rbind(summary_df, new_conditions_df)

# Check and replace values > 500 with "N/A" in the percent_change column
summary_df$percent_change <- ifelse(summary_df$percent_change > 500, 
                                    "N/A", summary_df$percent_change)

# Save the final summary
write.csv(summary_df, paste0(chart_dir, "/condition_summaries.csv"), 
                              row.names = FALSE)

###############################################################################
# DST end-day computations. Clock is reset from 0159 => 0100.
# Filter for rows that meet all three conditions

# Step 1: Extract DST end days from cleaned_data
november_data <- cleaned_data %>%
  filter(month(created_date) == 11)

dst_end_days <- november_data %>%
  filter(day(created_date) <= 7, wday(created_date) == 1)

# Step 2: Filter to DST hour (01:00–01:59)
dst_hour_data <- dst_end_days %>%
  filter(hour(created_date) >= 1 & hour(created_date) < 2) %>%
  mutate(dst_end_date = as.Date(created_date))

# Step 3a: Create full summary
summary_data_dst_end <- dst_hour_data %>%
  mutate(duration_minutes = 
           as.numeric(difftime(closed_date, created_date, units = "mins"))) %>%
  group_by(dst_end_date) %>%
  summarize(
    records_0100_0200 = n(),
    records_closed_leq_created = 
      sum(!is.na(closed_date) & closed_date <= created_date, na.rm = TRUE),
    avg_negative_duration_minutes = 
      if (sum(!is.na(closed_date) & closed_date <= created_date) > 0) {
      mean(duration_minutes[!is.na(closed_date) 
                            & closed_date <= created_date], na.rm = TRUE)
    } else {
      0
    },
    .groups = "drop"
  ) %>%
  mutate(
    fraction_closed_leq_created = ifelse(records_0100_0200 > 0,
                               records_closed_leq_created / records_0100_0200,
                               0),
    abs_duration_minutes = abs(avg_negative_duration_minutes)
  ) %>%
  arrange(dst_end_date)

# Step 3b: Determine sampling size
sample_size <- dst_hour_data %>%
  count(dst_end_date) %>%
  summarize(min_n = min(n)) %>%
  pull(min_n)

# Step 4: Create a stratified sample by dst_end_date (equal n per group)
set.seed(42)

sampled_dst_end_data <- dst_hour_data %>%
  group_by(dst_end_date) %>%
  slice_sample(n = sample_size) %>%
  ungroup()
 
# Step 5: Create summary for the sampled dataset
sample_dst_end <- sampled_dst_end_data %>%
  mutate(duration_minutes = as.numeric(difftime(closed_date, created_date, 
                                                units = "mins"))) %>%
  group_by(dst_end_date) %>%
  summarize(
    records_0100_0200 = n(),
    records_closed_leq_created = 
      sum(!is.na(closed_date) & closed_date <= created_date, na.rm = TRUE),
    avg_negative_duration_minutes = 
      if (sum(!is.na(closed_date) & closed_date <= created_date) > 0) {
      mean(duration_minutes[!is.na(closed_date) 
                            & closed_date <= created_date], na.rm = TRUE)
    } else {
      0
    },
    .groups = "drop"
  ) %>%
  mutate(
    fraction_closed_leq_created = ifelse(records_0100_0200 > 0,
                             records_closed_leq_created / records_0100_0200,
                             0),
    abs_duration_minutes = abs(avg_negative_duration_minutes)
  ) %>%
  arrange(dst_end_date)

summary_data_dst_end <- summary_data_dst_end %>%
  mutate(
    abs_duration_minutes = abs(avg_negative_duration_minutes),
    duration_for_plot = abs_duration_minutes
  )

stacked_data <- summary_data_dst_end %>%
  mutate(
    other_records = records_0100_0200 - records_closed_leq_created
  ) %>%
  pivot_longer(
    cols = c(other_records, records_closed_leq_created),  # Order matters here
    names_to = "record_type",
    values_to = "count"
  ) %>%
  mutate(
    record_type = factor(record_type, levels = c("other_records", 
                                                 "records_closed_leq_created"))
  )

####################
# ---- Plot Stacked Bar ----
# Scale the duration values to match the count scale for plotting
# Make sure your limits are high enough for all data
# Calculate max values for both axes
max_count <- max(summary_data_dst_end$records_0100_0200, na.rm = TRUE)
max_count <- ceiling(max_count/100) * 100  # Round up to nearest 100

max_duration <- max(summary_data_dst_end$abs_duration_minutes, na.rm = TRUE)
max_duration <- ceiling(max_duration/10) * 10  # Round up to nearest 10

# Define appropriate steps
count_step <- max(1, round(max_count/5))  # 5 breaks for count axis
duration_step <- max(1, round(max_duration/4))  # 4 breaks for duration axis

# Calculate the scale factor between count and duration
scale_anchor <- max_count / max_duration

# Scale the duration values to match the count scale for plotting
summary_data_dst_end$duration_for_plot <- 
                        summary_data_dst_end$abs_duration_minutes * scale_anchor

dst_end_chart_stacked <- ggplot(stacked_data, 
                              aes(x = as.factor(dst_end_date), y = count, 
                              fill = record_type)) +
  
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  
  # Plot average duration as points (optional)
  geom_point(
    data = summary_data_dst_end,
    aes(x = as.factor(dst_end_date), y = duration_for_plot),
    inherit.aes = FALSE,
    color = "red3",
    shape = 18,
    size = 4
  ) +
  
  # Duration text labels above the diamond points (optional)
  geom_text(
    data = summary_data_dst_end,
    aes(x = as.factor(dst_end_date), y = duration_for_plot,
        label = paste0("-", round(abs_duration_minutes, 1), " min")),
    inherit.aes = FALSE,
    vjust = -1,
    size = 3.5,
    color = "red3"
  ) +
  geom_text(
    data = stacked_data %>% filter(record_type == "records_closed_leq_created"),
    aes(x = as.factor(dst_end_date), y = count, label = count),
    position = position_stack(vjust = 1),  # Top of the segment
    inherit.aes = FALSE,
    size = 3,
    color = "black",
    vjust = -0.2  # Slight nudge above the bar
  ) +
  geom_text(
    data = summary_data_dst_end,
    aes(x = as.factor(dst_end_date), 
        y = records_0100_0200, 
        label = records_0100_0200),
    inherit.aes = FALSE,
    vjust = -0.5,
    size = 3,
    color = "black"
  ) +

  scale_fill_manual(
    values = c(
      "other_records" = "steelblue3",                     # Base
      "records_closed_leq_created" = "darkorange"         # Overlay
    ),
    labels = c(
      "other_records" = "Total 0100–0159 Records",
      "records_closed_leq_created" = "Closed <= Created"
    )
  ) +

  scale_y_continuous(
    name = "# of Records: 0100–0159 on DST end day",
    breaks = seq(0, max_count, count_step),
    limits = c(0, max_count),
    sec.axis = sec_axis(
      transform = ~ . / scale_anchor,
      name = "Avg Duration minutes (closed <= created)",
      breaks = seq(0, max_duration, duration_step)
    )
  ) +

  theme(
    plot.title = element_text(size = 14, face = "bold", color = "black", 
                              margin = margin(b = 10)),
    plot.subtitle = element_text(size = 11, color = "black", 
                                 margin = margin(b = 15)),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.75),
    panel.grid.major = element_line(color = "gray88", linewidth = 0.5),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "gray99", color = NA),
    plot.background = element_rect(fill = "gray90", color = NA),
    axis.text.x = element_text(angle = 40, hjust = 1, color = "black"),
    axis.title.y = element_text(margin = margin(r = 10), color = "black"),
    
    # Secondary axis styling - all red
    axis.title.y.right = element_text(color = "red3"),  # Red title
    axis.text.y.right = element_text(color = "red3"),   # Red tick labels
    axis.ticks.y.right = element_line(color = "red3"),  # Red tick marks
    axis.line.y.right = element_line(color = "red3"),   # Red axis line
    
    plot.caption = element_text(hjust = 1, size = 9, color = "black"),
    legend.position = "inside",
    legend.position.inside = c(0.02, 0.98),
    legend.justification.inside = c(0, 1),
    legend.background = element_rect(fill = "gray95", color = "black", 
                                     linewidth = 0.5),
    legend.key = element_rect(fill = NA),
    legend.title = element_text(face = "bold")
  ) +

labs(
  title = "DST Ending Days Analysis",
  subtitle = "Comparing 0100–0159 created records to early closures",
  y = "# of Records: 0100-0159 on DST end day",
  fill = "Record Type",
  x = NULL 
)

print(dst_end_chart_stacked)
cat("\nWaiting 2 seconds between charts...\n")
Sys.sleep(2)

# ---- Export ----
output_file <- file.path(chart_dir, "paired_bar_dst.pdf")
ggsave(
  filename = output_file,
  plot = dst_end_chart_stacked,
  device = "pdf",
  width = 13,
  height = 8.5,
  units = "in"
)

dst_end_chart <- create_condition_plot( data = summary_data_dst_end,
              title = "Proportion of affected SRs on DST 'end-day' w/trendline",
              y_label = "Affected Proportion",
              subtitle = "SRs where closed_date <= created_date",
              value_field = "fraction_closed_leq_created",
              bias_value = 1,
              date_field = "dst_end_date"
                                  )
print(dst_end_chart$plot)

cat("\nWaiting 2 seconds between charts...\n")
Sys.sleep(2)

# ---- Export ----
output_file <- file.path(chart_dir, "Affected_SRs_on DST_end-day.pdf")
ggsave(
      filename = output_file,
      plot = dst_end_chart$plot,
      device = "pdf",
      width = 13,
      height = 8.5,
      units = "in"
)

# Create QCC chart for community boards
# Define variables explicitly
# Count of "defects" — i.e., closed <= created
dst_count_data <- sample_dst_end$records_closed_leq_created

# Sample size — i.e., all records from 01:00–01:59 on DST-end date
dst_sample_sizes <- sample_dst_end$records_0100_0200

# Title for the chart
dst_chart_title <- "QCC p-chart of DST-End Negative Durations"

# X-axis labels — use full date for clarity
dst_labels <- format(sample_dst_end$dst_end_date, "%Y-%m-%d")

# Create and display QCC chart directly
dst_end_qcc_plot <- qcc(
  data = dst_count_data,
  sizes = dst_sample_sizes,
  type = "p",
  labels = dst_labels,
  title = dst_chart_title,
  plot = TRUE  # This will display the plot in RStudio
)

# Add a 2 second delay to let plots render completely
cat("\nWaiting 2 seconds before continuing to next chart...\n")
Sys.sleep(2)

# Save QCC chart - with explicit title
pdf(paste0(chart_dir, "/QCC p-chart of DST-End with negative durations.pdf"),
    width = 13, height = 8.5)
plot(dst_end_qcc_plot, title = dst_chart_title)
dev.off()

################################################################################
# DST start-day computations. Clock is moved forward from 0159 => 0300.
# Filter for rows that meet conditions for DST start (spring forward)

# Step 1: Extract DST end days from cleaned_data
march_data <- cleaned_data %>% filter(month(created_date) == 3)

dst_start_days <- march_data %>%
  filter(day(created_date) > 7, , day(created_date) <= 14, 
                                                      wday(created_date) == 1 )

# Step 2: Filter to DST hour (01:00-01:59) and identify affected records
dst_start_affected_records <- dst_start_days %>%
  filter(hour(created_date) >= 1 & hour(created_date) < 2) %>%
  mutate(
    dst_start_date = as.Date(created_date),
    duration_minutes = as.numeric(difftime(closed_date, created_date, 
                                                          units = "mins"))
  )

# Step 3: Create summary dataset for records affected by DST start
summary_data_dst_start <- if (nrow(dst_start_affected_records) == 0) {
  # Handle edge case where no records exist
  tibble(
    dst_start_date = as.Date(character(0)),
    records_0100_0200 = integer(0),
    records_affected_by_dst = integer(0),
    fraction_affected = numeric(0)
  )
} else {
  # Normal case - process the records
  dst_start_affected_records %>%
    mutate(
      affected_by_dst = is.na(closed_date) | 
        (hour(closed_date) >= 3 & as.Date(closed_date) == as.Date(created_date))
    ) %>%
    group_by(dst_start_date) %>%
    summarize(
      records_0100_0200 = n(),
      records_affected_by_dst = sum(affected_by_dst, na.rm = TRUE),
      fraction_affected = sum(affected_by_dst, na.rm = TRUE) / n()
    ) %>%
    arrange(dst_start_date)
}

# Step 4: Create summary for the sampled DST start dataset
# Determine the minimum count of records across all DST start dates
sample_size <- dst_start_affected_records %>%
  count(dst_start_date) %>%
  summarize(min_n = min(n)) %>%
  pull(min_n)

# Step 4b: Create a stratified sample by dst_start_date (equal n per group)
set.seed(42)

dst_start_sampled_records <- dst_start_affected_records %>%
  group_by(dst_start_date) %>%
  slice_sample(n = sample_size) %>%
  ungroup()

# Step 5: Create summary for the sampled dataset
sample_dst_start <- dst_start_sampled_records %>%
  group_by(dst_start_date) %>%
  summarize(
    records_0100_0200 = n(),
    records_affected_by_dst = sum(is.na(closed_date) | 
      (hour(closed_date) >= 3 & as.Date(closed_date) == as.Date(created_date)), 
      na.rm = TRUE),
      .groups = "drop"
  ) %>%
  mutate(
    fraction_affected = ifelse(records_0100_0200 > 0,
                               records_affected_by_dst / records_0100_0200,
                               0)
  ) %>%
  arrange(dst_start_date)

# Create stacked bar data
stacked_data <- summary_data_dst_start %>%
  mutate(
    unaffected_records = records_0100_0200 - records_affected_by_dst
  ) %>%
  pivot_longer(
    cols = c(unaffected_records, records_affected_by_dst),
    names_to = "record_type",
    values_to = "count"
  ) %>%
  mutate(
    record_type = factor(record_type, levels = c("unaffected_records", 
                                                 "records_affected_by_dst"))
  )

# ---- Final plot (stacked bar version) ----
dst_start_stacked_chart <- ggplot(stacked_data, aes(x = 
                              as.factor(dst_start_date), y = count, 
                              fill = record_type)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  
  # Show count of affected records
  geom_text(
    data = stacked_data %>% filter(record_type == "records_affected_by_dst"),
    aes(x = as.factor(dst_start_date), y = count, label = count),
    position = position_stack(vjust = 1),  # Top of the segment
    inherit.aes = FALSE,
    size = 3,
    color = "black",
    vjust = -0.2  # Slight nudge above the bar
  ) +
  
  # Show total count
  geom_text(
    data = summary_data_dst_start,
    aes(x = as.factor(dst_start_date), y = records_0100_0200, 
      label = records_0100_0200),
      inherit.aes = FALSE,
      vjust = -0.5,
      size = 3,
      color = "black"
  ) +
  
  scale_fill_manual(
    values = c(
      "unaffected_records" = "steelblue3",           
      "records_affected_by_dst" = "darkorange"       
    ),
    labels = c(
      "unaffected_records" = "Total 0100–0159 Records",
      "records_affected_by_dst" = "Affected by DST (+1hr)"
    )
  ) +
  
  scale_y_continuous(
    name = "# of Records: 0100–0159 on DST start day"
  ) +
  
  theme(
    plot.title = element_text(size = 14, face = "bold", color = "black", 
                              margin = margin(b = 10)),
    plot.subtitle = element_text(size = 11, color = "black", 
                                 margin = margin(b = 15)),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.75),
    panel.grid.major = element_line(color = "gray88", linewidth = 0.5),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "gray99", color = NA),
    plot.background = element_rect(fill = "gray90", color = NA),
    axis.text.x = element_text(angle = 40, hjust = 1, color = "black"),
    axis.title.y = element_text(margin = margin(r = 10), color = "black"),
    plot.caption = element_text(hjust = 1, size = 9, color = "black"),
    legend.position = "inside",
    legend.position.inside = c(0.02, 0.98),
    legend.justification.inside = c(0, 1),
    legend.background = element_rect(fill = "gray95", color = "black", 
                                     linewidth = 0.5),
    legend.key = element_rect(fill = NA),
    legend.title = element_text(face = "bold")
  ) +
  
  labs(
    title = "DST Starting Days Analysis",
    subtitle = "Comparing 0100–0159 created records to those affected by 'spring forward'",
    y = "# of Records: 0100-0159 on DST start day",
    fill = "Record Type",
    x = NULL
  )

print(dst_start_stacked_chart)
cat("\nWaiting 2 seconds between charts...\n")
Sys.sleep(2)

# ---- Export ----
output_file <- file.path(chart_dir, "paired_bar_dst.pdf")
ggsave(
  filename = output_file,
  plot = dst_start_stacked_chart,
  device = "pdf",
  width = 13,
  height = 8.5,
  units = "in"
)

dst_start_chart <- create_condition_plot( data = summary_data_dst_start,
                                        title = "Proportion of affected SRs on DST 'start-day' w/trendline",
                                        y_label = "Affected Proportion (X 1000)",
                                        subtitle = "SRs where 1 hour added to closed_date",
                                        value_field = "fraction_affected",
                                        bias_value = 1,
                                        date_field = "dst_start_date"
)

print(dst_start_chart$plot)

cat("\nWaiting 2 seconds between charts...\n")
Sys.sleep(2)

# ---- Export ----
output_file <- file.path(chart_dir, "Affected_SRs_on DST_start-day.pdf")
ggsave(
  filename = output_file,
  plot = dst_start_chart$plot,
  device = "pdf",
  width = 13,
  height = 8.5,
  units = "in"
)

# Create QCC chart for community boards
# Count of "defects" — i.e., closed <= created
dst_count_data <- sample_dst_start$records_affected_by_dst

# Sample size — i.e., all records from 01:00–01:59 on DST-end date
dst_sample_sizes <- sample_dst_start$records_0100_0200

# Title for the chart
dst_chart_title <- "QCC p-chart of DST-Start-day affected SRs"

# X-axis labels — use full date for clarity
dst_labels <- format(sample_dst_start$dst_start_date, "%Y-%m-%d")

# Create and display QCC chart directly
dst_qcc_plot <- qcc(
  data = dst_count_data,
  sizes = dst_sample_sizes,
  type = "p",
  labels = dst_labels,
  title = dst_chart_title,
  plot = TRUE  # This will display the plot in RStudio
)

# Add a 2 second delay to let plots render completely
cat("\nWaiting 2 seconds before continuing to next chart...\n")
Sys.sleep(2)

# Save QCC chart - with explicit title
pdf(paste0(chart_dir, "/QCC p-chart of DST-Start-day affected SRs.pdf"),
    width = 13, height = 8.5)
plot(dst_qcc_plot, title = dst_chart_title)
dev.off()

################################################################################
# Create response time by borough and plot



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