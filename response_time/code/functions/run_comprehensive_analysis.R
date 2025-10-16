################################################################################
# Expanded execution script - analyzes by agency, borough, and complaint_type
run_comprehensive_analysis <- function(data = cleaned_data, 
                                       distro_dir,
                                       analysis_types = c("agency", "borough", 
                                                          "complaint_type"),
                                       min_records_threshold = 100) {
  
  # Validate required columns exist
  required_cols <- c("agency", "borough", "complaint_type", "response_time")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Initialize storage for all results
  comprehensive_results <- list()
  comprehensive_plots <- list()
  
  # Process each analysis type
  for (analysis_type in analysis_types) {
    
    cat("\n", rep("=", 80), "\n")
    cat("STARTING", toupper(analysis_type), "ANALYSIS\n")
    cat(rep("=", 80), "\n")
    
    # Get unique values for current analysis type
    unique_values <- sort(unique(data[[analysis_type]]))
    
    # Remove any NA values
    unique_values <- unique_values[!is.na(unique_values)]
    
    # Add "ALL" option for boroughs only
    if (analysis_type == "borough") {
      unique_values <- c("ALL", unique_values)
    }
    
    # Apply minimum records threshold filter - ONLY for complaint types
    if (!is.null(min_records_threshold) && min_records_threshold > 0 && 
        analysis_type == "complaint_type") {
      
      cat("Applying minimum threshold of", min_records_threshold, 
          "records to complaint types\n")
      
      # Get record counts for complaint types
      item_counts <- data[, .N, by = analysis_type]
      
      # Filter to complaint types with sufficient records
      sufficient_items <- item_counts[N >= 
                                        min_records_threshold][[analysis_type]]
      
      # Filter unique_values to include complaint types with sufficient records
      original_count <- length(unique_values)
      unique_values <- intersect(unique_values, sufficient_items)
      filtered_count <- length(unique_values)
      
      if (filtered_count < original_count) {
        cat("Filtered complaint types from", original_count, "to", 
            filtered_count, "using minimum threshold of", 
            min_records_threshold, "records\n")
      }
      
    } else if (!is.null(min_records_threshold) && min_records_threshold > 0) {
      # For non-complaint types, just log that threshold is being skipped
      cat("Skipping minimum threshold filter for", analysis_type, 
          "(threshold only applies to complaint types)\n")
    }
    
    cat("Found", length(unique_values), analysis_type, "categories\n")
    if (length(unique_values) <= 20) {
      cat("Categories:", paste(unique_values, collapse = ", "), "\n")
    } else {
      cat("Categories: (showing first 10)", paste(head(unique_values, 10), 
                                                  collapse = ", "), "...\n")
    }
    cat("\n")
    
    # Initialize storage for this analysis type
    type_results <- list()
    type_plots <- list()
    
    # Loop through all unique values for this analysis type
    max_items <- length(unique_values)
    for (i in 1:max_items) {
      current_item <- unique_values[i]
      
      cat("\n", rep("-", 60), "\n")
      cat("PROCESSING", toupper(analysis_type), i, "OF", max_items, ":", 
          current_item, "\n")
      cat(rep("-", 60), "\n")
      
      tryCatch({
        # Filter data for current item and prepare for exploration
        if (analysis_type == "agency") {
          filtered_data <- data[agency == current_item]
        } else if (analysis_type == "borough") {
          if (current_item == "ALL") {
            filtered_data <- data  # Use all data for "ALL" borough analysis
          } else {
            filtered_data <- data[borough == current_item]
          }
        } else if (analysis_type == "complaint_type") {
          filtered_data <- data[complaint_type == current_item]
        }
        
        # Check if we have enough data
        if (nrow(filtered_data) < 10) {
          cat("  ⚠ Skipping", current_item, "- insufficient data (", 
              nrow(filtered_data), "records)\n")
          next
        }
        
        # since explore_response_times expects to filter by agency
        if (analysis_type != "agency") {
          # Create a copy. Add temporary agency column with the current item name
          filtered_data_copy <- copy(filtered_data)
          filtered_data_copy[, agency := current_item]
          exploration_data <- filtered_data_copy
        } else {
          exploration_data <- filtered_data
        }
        
        # Run exploration with custom parameters
        item_exploration <- explore_response_times(
          data = exploration_data,
          target_agency = current_item,  # This will work for all types now
          reasonable_days = 30,       # 30 days
          long_days = 90,           # 90 days  
          very_long_days = 180       # 180 days
        )
        
        # Create and display plots
        item_plot <- create_agency_plot(item_exploration, current_item)
        item_violin <- create_agency_violin(item_exploration, current_item)
        
        # Store results
        type_results[[current_item]] <- item_exploration
        type_plots[[current_item]] <- list(
          histogram = item_plot,
          violin = item_violin
        )
        
        cat("  ✓ Successfully processed", current_item, "\n")
        
      }, error = function(e) {
        cat("  ✗ Error processing", current_item, ":", e$message, "\n")
      })
    }
    
    # Store results for this analysis type
    comprehensive_results[[analysis_type]] <- type_results
    comprehensive_plots[[analysis_type]] <- type_plots
    
    # Print summary for this analysis type
    cat("\n", rep("-", 60), "\n")
    cat("SUMMARY FOR", toupper(analysis_type), "ANALYSIS\n")
    cat(rep("-", 60), "\n")
    
    processed_count <- length(type_results)
    cat("Successfully processed", processed_count, "out of", max_items, 
        analysis_type, "categories\n")
    
    if (processed_count > 0) {
      for (item_name in names(type_results)) {
        result <- type_results[[item_name]]
        total_records <- nrow(result$agency_data)  
        # Note: still called agency_data in the exploration function
        reasonable_pct <- round(100 * nrow(result$reasonable_cases) / 
                                  total_records, 2)
        long_pct <- round(100 * nrow(result$long_cases) / total_records, 2)
        very_long_pct <- round(100 * nrow(result$very_long_cases) / 
                                 total_records, 2)
        extremely_long_pct <- round(100 * nrow(result$extremely_long_cases) / 
                                      total_records, 2)
        median_response <- round(median(result$agency_data$response_time), 3)
        
        cat(sprintf("  %-25s: %8s records | %5.2f%% reasonable | %5.2f%% long | %5.2f%% very long | %5.2f%% extreme | Median: %6.3f days\n", 
                    substr(item_name, 1, 25),  # Truncate long names
                    format(total_records, big.mark = ","),
                    reasonable_pct,
                    long_pct,
                    very_long_pct,
                    extremely_long_pct,
                    median_response))
      }
    }
  }
  
  # Overall comprehensive summary
  cat("\n", rep("=", 80), "\n")
  cat("COMPREHENSIVE ANALYSIS COMPLETE\n")
  cat(rep("=", 80), "\n")
  
  for (analysis_type in names(comprehensive_results)) {
    processed_count <- length(comprehensive_results[[analysis_type]])
    total_available <- length(sort(unique(data[[analysis_type]])))
    cat("  ", toupper(analysis_type), ":", processed_count, "processed out of", 
        total_available, "available\n")
  }
  
  # Return comprehensive results
  return(list(
    results = comprehensive_results,
    plots = comprehensive_plots,
    analysis_types = analysis_types,
    summary = list(
      agencies_processed = length(comprehensive_results[["agency"]]),
      boroughs_processed = length(comprehensive_results[["borough"]]),
      complaint_types_processed = 
        length(comprehensive_results[["complaint_type"]])
    )
  ))
}

################################################################################
