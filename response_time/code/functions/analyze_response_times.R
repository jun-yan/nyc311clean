################################################################################
# Agency Response Time Analysis Program
# Creates cumulative percentage chart for response times

library(data.table)
library(ggplot2)
library(gridExtra)
library(scales)

################################################################################
# Function to analyze response times for a specific agency
analyze_response_times <- function(data = cleaned_data, 
                                   target_agency = NULL,
                                   year_filter = NULL,
                                   max_response_days = NULL)
                                  {
  
  working_dir <- getwd() 
  
  base_dir <- file.path(working_dir, "datacleaningproject", "nyc311clean",  
                        "response_time" )
  
  # Define the path for the charts
  chart_dir <- file.path(base_dir, "charts")
  
  cumulative_dir <- file.path(chart_dir, "cumulative_charts")
  
  # Input validation
  if(is.null(target_agency)) {
    stop("Please specify target_agency parameter")
  }
  
  # Check if response_time column exists
  if(!"response_time" %in% names(data)) {
    stop("Column 'response_time' not found in the dataset")
  }
  
  # Filter data for the specified agency
  agency_data <- data[agency == target_agency]
  
  if(nrow(agency_data) == 0) {
    stop("No data found for agency:", target_agency)
  }
  
  # Apply year filter if specified
  if(!is.null(year_filter)) {
    if("created_date" %in% names(agency_data)) {
      agency_data <- agency_data[year(created_date) %in% year_filter]
      cat("Filtered to years:", paste(year_filter, collapse = ", "), "\n")
    } else {
      warning("Year filter requested but no 'created_date' column found")
    }
  }

  if(nrow(agency_data) == 0) {
    stop("No valid response time data after removing missing/negative values")
  }
  
  # If max_response_days not specified, use agency's actual maximum
  if(is.null(max_response_days)) {
    max_response_days <- max(agency_data$response_time, na.rm = TRUE)
#    cat("Using dynamic max response days:", round(max_response_days, 1), "\n")
  } else {
    cat("Using specified max response days:", max_response_days, "\n")
  }
  
  # Apply the max response days filter
  agency_data <- agency_data[response_time <= max_response_days]
  
  if(nrow(agency_data) == 0) {
    stop("No valid response time data after filtering")
  }
  
  # cat("Records with valid response times:", 
  #     format(nrow(agency_data), big.mark = ","), 
  #     "out of", format(original_count, big.mark = ","), "\n")
  
  # Calculate summary statistics
  response_times <- agency_data$response_time
  
  summary_stats <- list(
    count = length(response_times),
    mean = mean(response_times),
    median = median(response_times),
    q25 = quantile(response_times, 0.25),
    q75 = quantile(response_times, 0.75),
    min = min(response_times),
    max = max(response_times),
    std_dev = sd(response_times)
  )
  
  # Print summary statistics
  cat("\nSummary Statistics for", target_agency, "Response Times:\n")
  cat("Count:", format(summary_stats$count, big.mark = ","), "\n")
  cat("Mean:", round(summary_stats$mean, 4), "days\n")
  cat("Median:", round(summary_stats$median, 4), "days\n")
  cat("25th percentile:", round(summary_stats$q25, 4), "days\n")
  cat("75th percentile:", round(summary_stats$q75, 4), "days\n")
  cat("Min:", round(summary_stats$min, 4), "days\n")
  cat("Max:", round(summary_stats$max, 4), "days\n")
  cat("Standard deviation:", round(summary_stats$std_dev, 2), "days\n")
 
################################################################################   
  # Create histogram data
  hist_data <- data.table(response_time = response_times)
  
  # Calculate cumulative distribution
  sorted_times <- sort(response_times)
  cum_data <- data.table(
        response_time = sorted_times,
        cumulative_pct = (1:length(sorted_times)) / length(sorted_times) * 100
  )
  
################################################################################       
  # Create cumulative percentage plot
  p2 <- ggplot(cum_data, aes(x = response_time, y = cumulative_pct)) +
    geom_line(color = "steelblue4", linewidth = 1.15) +
    
    # Add reference lines for common percentiles
    geom_hline(yintercept = c(50, 90, 95), color = "grey40", 
               linetype = "dotted", alpha = 0.7, linewidth = 1.1) +
    geom_vline(xintercept = summary_stats$median, color = "orange3", 
               linetype = "dashed", linewidth = 1.15) +
    
    # Add percentile annotations
    annotate("text", x = max(sorted_times) * 0.8, y = 50, 
             label = "50%", hjust = 0, vjust = -0.5, color = "grey40") +
    annotate("text", x = max(sorted_times) * 0.8, y = 90, 
             label = "90%", hjust = 0, vjust = -0.5, color = "grey40") +
    annotate("text", x = max(sorted_times) * 0.8, y = 95, 
             label = "95%", hjust = 0, vjust = -0.5, color = "grey40") +
    
    scale_x_continuous(breaks = pretty_breaks(n = 10)) +
    scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +
    
    labs(
      title = paste("Cumulative Response Time Distribution for", target_agency),
      subtitle = paste("50% of cases resolved within", 
                       round(summary_stats$median, 2), "days"),
      x = "Response Time (Days)",
      y = "Cumulative Percentage (%)"
    ) +
    
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "grey93"),
      panel.grid.major = element_line(color = "white", linewidth = 0.75),
      panel.grid.minor = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 11),
      axis.text = element_text(face = "bold")
    )
  
  print(p2)
  Sys.sleep(2)
  
  # Create file paths
  cumulative_file <- file.path(cumulative_dir, 
                               paste0(target_agency, "_cumulative.pdf"))
  
  # Save individual plots as PDFs
  ggsave(filename = cumulative_file, plot = p2, width = 13, height = 8.5, dpi = 300, device = "pdf")
  
################################################################################     

  # Return summary statistics and plots
  result <- list(
    agency = target_agency,
    summary_stats = summary_stats,
    cumulative_plot = p2,
    data_count = nrow(agency_data),
    max_response_days_used = max_response_days
  )
  
  return(result)
}

################################################################################