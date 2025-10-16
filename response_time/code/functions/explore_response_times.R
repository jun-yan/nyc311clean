# Function to explore response time data quality
explore_response_times <- function(data = cleaned_data, 
                                   target_agency = NULL,
                                   reasonable_days = 90,    # 0.25 years
                                   long_days = 180,            # 0.5 year  
                                   very_long_days = 365) {    # 1 years
  
  cat("=== RESPONSE TIME DATA EXPLORATION FOR", target_agency, "===\n\n")
  
  # Filter to target agency
  agency_data <- data[agency == target_agency]
  cat("Total", target_agency, "records:", format(nrow(agency_data), 
                                                 big.mark = ","), "\n\n")
  
  # 1. Basic response time statistics
  cat("1. BASIC RESPONSE TIME STATISTICS:\n")
  cat("-----------------------------------\n")
  response_summary <- summary(agency_data$response_time)
  # Format summary to 4 decimal places
  formatted_summary <- round(response_summary, 4)
  print(formatted_summary)
  cat("\nStandard deviation:", round(sd(agency_data$response_time, 
                                        na.rm = TRUE), 4), "days\n\n")
  
  # 2. Percentile breakdown
  cat("2. RESPONSE TIME PERCENTILES:\n")
  cat("-----------------------------\n")
  percentiles <- quantile(agency_data$response_time, 
                          probs = c(0.5, 0.75, 0.9, 0.95, 0.99, 0.999, 1.0), 
                          na.rm = TRUE)
  for(i in 1:length(percentiles)) {
    pct_name <- names(percentiles)[i]
    pct_val <- percentiles[i]
    pct_years <- round(pct_val / 365, 1)
    cat(sprintf("%-6s: %8.1f days (%4.1f years)\n", pct_name, pct_val, 
                                                                    pct_years))
  }
  cat("\n")
  
  # 3. Count of extreme response times
  cat("3. RESPONSE TIME COUNTS BY CATEGORY:\n")
  cat("------------------------------------\n")
  
  # Calculate counts first
  reasonable_count <- sum(agency_data$response_time <= 
                            reasonable_days, na.rm = TRUE)
  long_count <- sum(agency_data$response_time > 
                      reasonable_days & agency_data$response_time <= 
                      long_days, na.rm = TRUE)
  very_long_count <- sum(agency_data$response_time > 
                           long_days & agency_data$response_time <= 
                           very_long_days, na.rm = TRUE)
  extremely_long_count <- sum(agency_data$response_time > 
                                very_long_days, na.rm = TRUE)
  
  # Create named list
  extreme_counts <- list(reasonable_count, long_count, very_long_count, 
                         extremely_long_count)
  names(extreme_counts) <- c(
    paste("Reasonable (≤", reasonable_days, "days)"),
    paste("Long (", reasonable_days + 0.001, "-", long_days, "days)"),
    paste("Very Long (", long_days + 0.001, "-", very_long_days, "days)"),
    paste("Extremely Long (>", very_long_days, "days)")
  )
  
  total_records <- nrow(agency_data)
  for(name in names(extreme_counts)) {
    count <- extreme_counts[[name]]
    pct <- round(100 * count / total_records, 2)
    cat(sprintf("%-35s: %8s (%5.2f%%)\n", name, format(count, 
                                                       big.mark = ","), pct))
  }
  cat("\n")
  
  # 4. Date range analysis
  cat("4. DATE RANGE ANALYSIS:\n")
  cat("-----------------------\n")
  if("created_date" %in% names(agency_data)) {
    cat("Created date range:", 
        as.character(min(agency_data$created_date, na.rm = TRUE)), "to",
        as.character(max(agency_data$created_date, na.rm = TRUE)), "\n")
  }
  
  if("closed_date" %in% names(agency_data)) {
    cat("Closed date range:", 
        as.character(min(agency_data$closed_date, na.rm = TRUE)), "to",
        as.character(max(agency_data$closed_date, na.rm = TRUE)), "\n")
  }
  cat("\n")
  
  # 5. Look at the most extreme cases
  cat("5. MOST EXTREME CASES (Top 5 longest response times):\n")
  cat("------------------------------------------------------\n")
  
  extreme_cases <- agency_data[order(-response_time)][1:5]
  
  # Select relevant columns for display
  display_cols <- c("response_time")
  if("created_date" %in% names(extreme_cases)) display_cols <- 
    c(display_cols, "created_date")
  if("closed_date" %in% names(extreme_cases)) display_cols <- 
    c(display_cols, "closed_date")
  if("status" %in% names(extreme_cases)) display_cols <- 
    c(display_cols, "status")
  if("complaint_type" %in% names(extreme_cases)) display_cols <- 
    c(display_cols, "complaint_type")
  
  extreme_display <- extreme_cases[, ..display_cols]
  extreme_display[, response_years := round(response_time / 365, 1)]
  
  print(extreme_display)
  cat("\n")
  
  # 6. Check for common date patterns in extreme cases
  cat("6. DATE PATTERN ANALYSIS FOR EXTREMELY LONG CASES (>", 
      very_long_days, "days):\n")
  cat("-------------------------------------------------------\n")
  
  if("created_date" %in% names(agency_data) && "closed_date" %in% 
     names(agency_data)) {
    extreme_data <- agency_data[response_time > very_long_days]
    
    if(nrow(extreme_data) > 0) {
      cat("Records with >", very_long_days, "days response times:", 
          nrow(extreme_data), "\n")
      
      # Check for common created_date patterns
      created_years <- year(extreme_data$created_date)
      created_table <- table(created_years)
      cat("\nCreated dates by year (for >", very_long_days, 
          "days response times):\n")
      print(head(sort(created_table, decreasing = TRUE), 10))
      
      # Check for common closed_date patterns  
      closed_years <- year(extreme_data$closed_date)
      closed_table <- table(closed_years)
      cat("\nClosed dates by year (for >", very_long_days, 
          "days response times):\n")
      print(head(sort(closed_table, decreasing = TRUE), 10))
      
    } else {
      cat("No records with >", very_long_days, "days response times found.\n")
    }
  } else {
    cat("Cannot analyze date patterns - missing created_date or closed_date\n")
  }
  
  cat("\n")
  
  # 7. Sample of reasonable vs extreme cases
  #  cat("7. COMPARISON BY CATEGORIES:\n")
  cat("----------------------------\n")
  
  reasonable <- agency_data[response_time <= reasonable_days]
  long <- agency_data[response_time > reasonable_days & response_time <= 
                        long_days]
  very_long <- agency_data[response_time > long_days & response_time <= 
                         very_long_days]
  extremely_long <- agency_data[response_time > very_long_days]
  
  cat("\n=== EXPLORATION COMPLETE ===\n")
  
  # Return useful data for further analysis
  return(list(
    agency_data = agency_data,
    reasonable_cases = reasonable,
    long_cases = long,
    very_long_cases = very_long,
    extremely_long_cases = extremely_long,
    summary_stats = response_summary,
    thresholds = list(
      reasonable_days = reasonable_days,
      long_days = long_days,
      very_long_days = very_long_days
    )
  ))
}
